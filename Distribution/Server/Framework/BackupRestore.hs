{-# LANGUAGE RankNTypes, RecordWildCards, GeneralizedNewtypeDeriving, BangPatterns, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Distribution.Server.Framework.BackupRestore (
    BackupEntry(..),
    restoreServerBackup,
    importBlank,

    importCSV,
    parseText,
    parseRead,
    parseUTCTime, formatUTCTime,

    equalTarBall,

    module Distribution.Server.Util.Merge,

    -- * We are slowly transitioning to a pure restore process
    -- Once that transition is complete this will replace RestoreBackup
    RestoreBackup(..),
    restoreBackupUnimplemented,
    concatM,
    Restore,
    restoreAddBlob,
    restoreGetBlob,

    AbstractRestoreBackup(..),
    abstractRestoreBackup
  ) where

import qualified Distribution.Server.Framework.BlobStorage as Blob
import Distribution.Server.Framework.BlobStorage (BlobStorage, BlobId)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Distribution.Server.Util.GZip (decompressNamed)
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Data.Time (UTCTime)
import qualified Data.Time as Time
import Data.Time.Locale.Compat (defaultTimeLocale)

import Distribution.Server.Util.Merge
import Distribution.Server.Util.Parse (unpackUTF8)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS (readFile)
import qualified Data.Map as Map
import Data.Ord (comparing)
import System.FilePath ((</>), takeDirectory, splitDirectories)
import System.Directory (doesFileExist, doesDirectoryExist)
import Text.CSV hiding (csv)
import Distribution.Text
import Data.Map (Map)
import Data.List (sortBy)


--------------------------------------------------------------------------------
-- Creating/restoring backups                                                 --
--------------------------------------------------------------------------------

data BackupEntry =
    BackupByteString [FilePath] ByteString
  | BackupBlob [FilePath] BlobId

data RestoreBackup st = RestoreBackup {
    restoreEntry    :: BackupEntry -> Restore (RestoreBackup st)
  , restoreFinalize :: Restore st
  }

instance Functor RestoreBackup where
  f `fmap` RestoreBackup {..} = RestoreBackup {
      restoreEntry    = liftM (fmap f) . restoreEntry
    , restoreFinalize = liftM f restoreFinalize
    }

restoreBackupUnimplemented :: RestoreBackup st
restoreBackupUnimplemented = RestoreBackup {
    restoreEntry    = const (return restoreBackupUnimplemented)
  , restoreFinalize = fail "Backup unimplemented"
  }

data AbstractRestoreBackup = AbstractRestoreBackup {
    abstractRestoreEntry    :: BlobStorage -> BackupEntry -> IO (Either String AbstractRestoreBackup)
  , abstractRestoreFinalize :: BlobStorage -> IO (Either String (IO ()))
  }

abstractRestoreBackup :: (st -> IO ()) -> RestoreBackup st -> AbstractRestoreBackup
abstractRestoreBackup putSt = go
  where
    go RestoreBackup {..} = AbstractRestoreBackup {
        abstractRestoreEntry = \store entry ->
          liftM go    <$> (runRestore store $ restoreEntry entry)
      , abstractRestoreFinalize = \store ->
          liftM putSt <$> (runRestore store $ restoreFinalize)
      }

instance Monoid AbstractRestoreBackup where
  mempty = AbstractRestoreBackup {
      abstractRestoreEntry = \_ _ -> return . Right $ mempty
    , abstractRestoreFinalize = \_ -> return . Right $ return ()
    }
  mappend (AbstractRestoreBackup run fin) (AbstractRestoreBackup run' fin') = AbstractRestoreBackup {
      abstractRestoreEntry = \store entry -> do
         res <- run store entry
         case res of
           Right backup -> do
             res' <- run' store entry
             return $ fmap (mappend backup) res'
           Left bad -> return $ Left bad
    , abstractRestoreFinalize = \store -> do
         res <- fin store
         case res of
           Right finalizer -> do
             res' <- fin' store
             return $ fmap (finalizer >>) res'
           Left bad -> return $ Left bad
    }

--------------------------------------------------------------------------------
-- Utilities for creating RestoreBackup objects                               --
--------------------------------------------------------------------------------

concatM :: (Monad m) => [a -> m a] -> (a -> m a)
concatM fs = foldr (>=>) return fs

importCSV :: Monad m => FilePath -> ByteString -> m CSV
importCSV filename inp = case parseCSV filename (unpackUTF8 inp) of
    Left err  -> fail (show err)
    Right csv -> return (chopLastRecord csv)
  where
    -- | Chops off the last entry if it's null.
    -- I'm not sure why parseCSV does this, but it's
    -- irritating.
    chopLastRecord [] = []
    chopLastRecord ([""]:[]) = []
    chopLastRecord (x:xs) = x : chopLastRecord xs

parseRead :: (Read a, Monad m) => String -> String -> m a
parseRead label str = case reads str of
    [(value, "")] -> return value
    _ -> fail $ "Unable to parse " ++ label ++ ": " ++ show str

parseUTCTime :: (Monad m, MonadError String m) => String -> String -> m UTCTime
parseUTCTime label str =
    case Time.parseTime defaultTimeLocale timeFormatSpec str of
      Nothing -> throwError $ "Unable to parse UTC timestamp " ++ label ++ ": " ++ str
      Just x  -> return x

formatUTCTime :: UTCTime -> String
formatUTCTime = Time.formatTime defaultTimeLocale timeFormatSpec

timeFormatSpec :: String
timeFormatSpec = "%Y-%m-%d %H:%M:%S%Q %z"

-- Parse a string, throw an error if it's bad
parseText :: (Text a, Monad m) => String -> String -> m a
parseText label text = case simpleParse text of
    Nothing -> fail $ "Unable to parse " ++ label ++ ": " ++ show text
    Just a -> return a

{-------------------------------------------------------------------------------
  Restore Monad

  This is a free monad. Monad laws:

  LEFT IDENTITY

    return x >>= f
  = RestoreDone x >>= f
  = f x

  RIGHT IDENTITY

    m >>= return
  = m >>= RestoreDone

  Induction on m:

    1.   RestoreDone x >>= RestoreDone
       = RestoreDone x
    2.   RestoreFail err >>= RestoreDone
       = RestoreFail err
    3.   RestoreAddBLob bs f >>= RestoreDone
       = RestoreAddBlob bs $ \bid -> f bid >>= RestoreDone
           { by induction }
       = RestoreAddBlob bs $ \bid -> f bid
           { eta reduction }
       = RestoreAddBlob bs f
    4. Analogous to (3).

  ASSOCIATIVITY

    You'll have to take my word for it :-)
-------------------------------------------------------------------------------}


data Restore a = RestoreDone a
               | RestoreFail String
               | RestoreAddBlob ByteString (BlobId -> Restore a)
               | RestoreGetBlob BlobId (ByteString -> Restore a)

instance Monad Restore where
  return = RestoreDone
  fail   = RestoreFail
  RestoreDone x        >>= g = g x
  RestoreFail err      >>= _ = RestoreFail err
  RestoreAddBlob bs  f >>= g = RestoreAddBlob bs  $ \bid -> f bid >>= g
  RestoreGetBlob bid f >>= g = RestoreGetBlob bid $ \bs  -> f bs  >>= g

instance MonadError [Char] Restore where
  throwError = RestoreFail
  catchError (RestoreFail s) h = h s
  catchError x _ = x

instance Functor Restore where
  fmap = liftM

instance Applicative Restore where
  pure      = return
  mf <*> mx = do f <- mf ; x <- mx ; return (f x)

runRestore :: BlobStorage -> Restore a -> IO (Either String a)
runRestore store = go
  where
    go :: forall a. Restore a -> IO (Either String a)
    go (RestoreDone a)        = return (Right a)
    go (RestoreFail err)      = return (Left err)
    go (RestoreAddBlob bs f)  = Blob.add   store bs  >>= go . f
    go (RestoreGetBlob bid f) = Blob.fetch store bid >>= go . f

restoreAddBlob :: ByteString -> Restore BlobId
restoreAddBlob = (`RestoreAddBlob` RestoreDone)

restoreGetBlob :: BlobId -> Restore ByteString
restoreGetBlob = (`RestoreGetBlob` RestoreDone)

--------------------------------------------------------------------------------
-- Import a backup                                                            --
--------------------------------------------------------------------------------

-- featureBackups must contain a SINGLE entry for each feature
restoreServerBackup :: BlobStorage -> FilePath -> Bool
                    -> [(String, AbstractRestoreBackup)] -> IO (Maybe String)
restoreServerBackup store tarFile consumeBlobs featureBackups = do
    checkBlobDirExists
    tar <- BS.readFile tarFile
    finalizers <- evalImport store blobdir consumeBlobs featureBackups $ do
        fromEntries . Tar.read . decompressNamed tarFile $ tar
        finalizeBackups store (map fst featureBackups)
    completeBackups finalizers
  where
    blobdir = takeDirectory tarFile </> "blobs"
    checkBlobDirExists = do
      exists <- doesDirectoryExist blobdir
      when (not exists) $ fail $
           "Expected to find the blobs directory in the same location as the "
        ++ "tar file " ++ tarFile

-- A variant of importTar that finalizes immediately.
importBlank :: BlobStorage -> [(String, AbstractRestoreBackup)] -> IO (Maybe String)
importBlank store featureBackups = do
    finalizers <- evalImport store "not-used" False featureBackups $
        finalizeBackups store (map fst featureBackups)
    completeBackups finalizers

-- | Call restoreFinalize for every backup. Caller must ensure that every
-- feature name is in the map.
finalizeBackups :: BlobStorage -> [String] -> Import [IO ()]
finalizeBackups store list = forM list $ \name -> do
    features <- gets importStates
    liftIO $ putStrLn $ "finalising data for feature " ++ name
    mbackup  <- liftIO $ abstractRestoreFinalize (features Map.! name) store
    case mbackup of
        Left err       -> throwError $ "Error restoring data for feature " ++ name
                              ++ ":" ++ err
        Right finalize -> return finalize

completeBackups :: Either String [IO ()] -> IO (Maybe String)
completeBackups res = case res of
    Left err -> return $ Just err
    Right putSt -> sequence_ putSt >> return Nothing

-- internal import utils

newtype Import a = Import { unImp :: StateT ImportState (ExceptT String IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState ImportState, MonadError String)

evalImport :: BlobStorage -> FilePath -> Bool
           -> [(String, AbstractRestoreBackup)]
           -> Import a -> IO (Either String a)
evalImport store blobdir consumeBlobs featureBackups imp =
    runExceptT (evalStateT (unImp imp) initState)
  where
    initState = initialImportState store blobdir consumeBlobs featureBackups

data ImportState = ImportState {
    -- | The 'RestoreBackup' of each state
    importStates    :: !(Map String AbstractRestoreBackup),

    -- | The blob storage we are writing to
    importBlobStore :: BlobStorage,

    -- | The backup blob dir we are reading from
    importBlobDir   :: FilePath,

    -- | If we should work in a mode where we destructively consume the blobs
    -- from the blob dir when adding them into the blob store. You might want
    -- to do this is to reduce the amount of disk I\/O, but you obviously have
    -- to be rather careful with it.
    importConsumeBlobs :: Bool,

    -- | We often read the same blob more than once, so this caches
    -- the blobs we've written as a map from blobid-string to blobid.
    importBlobsWritten :: Map String BlobId
  }

initialImportState :: BlobStorage -> FilePath -> Bool
                   -> [(String, AbstractRestoreBackup)]
                   -> ImportState
initialImportState store blobdir consumeBlobs featureBackups = ImportState {
    importStates    = Map.fromList featureBackups,
    importBlobStore = store,
    importBlobDir   = blobdir,
    importConsumeBlobs = consumeBlobs,
    importBlobsWritten = Map.empty
  }

updateImportState :: String -> AbstractRestoreBackup -> Import ()
updateImportState feature !backup = do
  st <- get
  put $! st { importStates = Map.insert feature backup (importStates st) }

lookupBlobWritten :: String -> Import (Maybe BlobId)
lookupBlobWritten blobidstr = gets (Map.lookup blobidstr . importBlobsWritten)

addBlobsWritten :: String -> BlobId -> Import ()
addBlobsWritten blobidstr !blobid = do
  st <- get
  put $! st {
    importBlobsWritten = Map.insert blobidstr blobid (importBlobsWritten st)
  }


fromEntries :: Tar.Entries Tar.FormatError -> Import ()
fromEntries Tar.Done        = return ()
fromEntries (Tar.Fail err)  = throwError $ "Problem in backup tarball: " ++ show err
fromEntries (Tar.Next x xs) = fromEntry x >> fromEntries xs

fromEntry :: Tar.Entry -> Import ()
fromEntry entry =
    case Tar.entryContent entry of
      Tar.NormalFile bytes _  -> fromFile (Tar.entryPath entry) bytes
      Tar.Directory {}        -> return () -- ignore directory entries
      Tar.SymbolicLink target -> fromLink (Tar.entryPath entry) (Tar.fromLinkTarget target)
      _ -> throwError $ "Unexpected entry in backup tarball: " ++ Tar.entryPath entry

fromFile :: FilePath -> ByteString -> Import ()
fromFile path contents = fromBackupEntry path (`BackupByteString` contents)

fromLink :: FilePath -> FilePath -> Import ()
fromLink path linkTarget
  -- links have format "../../../blobs/${blobId}" (for some number of "../"s)
  | (blobIdStr : "blobs" : _) <- reverse (splitDirectories linkTarget) = do
      blobdir <- gets importBlobDir
      let blobFile = blobdir </> blobIdStr
      blobId <- checkBlobWrittenCache blobIdStr
                  (checkBlobFileExists blobFile >> importBlobFile blobFile)
      checkBlobIdAsExpected blobId blobIdStr blobFile
      fromBackupEntry path (`BackupBlob` blobId)

  | otherwise = throwError $ "Unexpected tar link entry: " ++ path
                                           ++ " -> " ++ linkTarget
  where
    checkBlobWrittenCache blobIdStr getBlobId = do
      mblobId <- lookupBlobWritten blobIdStr
      case mblobId of
        Just blobId -> return blobId
        Nothing     -> do
          blobId <- getBlobId
          addBlobsWritten blobIdStr blobId
          return blobId

    importBlobFile blobFile = do
      store        <- gets importBlobStore
      consumeBlobs <- gets importConsumeBlobs
      if consumeBlobs
        then liftIO $ Blob.consumeFile store blobFile
        else liftIO $ Blob.add store =<< BS.readFile blobFile

    checkBlobFileExists blobFile = do
      exists <- liftIO $ doesFileExist blobFile
      when (not exists) $ throwError $ "Missing blob file " ++ blobFile
                       ++ "\nneeded by backup entry " ++ path

    checkBlobIdAsExpected blobId blobIdStr blobFile =
      when (Blob.blobMd5 blobId /= blobIdStr) $
        throwError $ "Incorrect blob file " ++ blobFile
            ++ "\nit actually has an md5sum of " ++ Blob.blobMd5 blobId


fromBackupEntry :: FilePath -> ([FilePath] -> BackupEntry) -> Import ()
fromBackupEntry path mkEntry
  | (_rootdir:featureName:pathEnd) <- splitDirectories path = do
  store         <- gets importBlobStore
  featureStates <- gets importStates
  case Map.lookup featureName featureStates of
    Just restorer -> do
      res <- liftIO $ abstractRestoreEntry restorer store (mkEntry pathEnd)
      case res of
        Left e          -> throwError $ "Error importing '" ++ path ++ "' :" ++ e
        Right restorer' -> updateImportState featureName restorer'
    Nothing -> throwError $ "Backup tarball contains file for unknown feature: " ++ featureName
fromBackupEntry path _ = throwError $ "Backup tarball contains unexpected file: " ++ path


--------------------------------------------------------------------------------
-- Compare tarballs                                                           --
--------------------------------------------------------------------------------

-- Used to compare export/import tarballs for equality by the backup/restore test:
equalTarBall :: ByteString -- ^ "Before" tarball
             -> ByteString -- ^ "After" tarball
             -> [String]
equalTarBall tar1 tar2 = do
  let entries :: Either String ([Tar.Entry], [Tar.Entry])
      entries = do
        entries1 <- sortBy (comparing Tar.entryTarPath) <$> readTar "before" tar1
        entries2 <- sortBy (comparing Tar.entryTarPath) <$> readTar "after"  tar2
        return (entries1, entries2)
  case entries of
    Left err -> [err]
    Right (entries1, entries2) ->
      flip concatMap (mergeBy (comparing Tar.entryTarPath) entries1 entries2) $ \mr -> case mr of
          OnlyInLeft  entry -> [Tar.entryPath entry ++ " only in 'before' tarball"]
          OnlyInRight entry -> [Tar.entryPath entry ++ " only in 'after' tarball"]
          InBoth entry1 entry2 -> concat [
                checkEq "content" Tar.entryContent,
                checkEq "permissions" Tar.entryPermissions,
                checkEq "ownership" Tar.entryOwnership
                -- Don't particularly care about modification time/tar format
              ]
            where
              tarPath = Tar.entryPath entry1
              checkEq :: Eq a => String -> (Tar.Entry -> a) -> [String]
              checkEq what f = if (f entry1 /= f entry2)
                                 then [tarPath ++ ": " ++ what ++ " did not match"]
                                 else []
  where
    readTar :: (Monad m, MonadError String m) => String -> ByteString -> m [Tar.Entry]
    readTar err = entriesToList err . Tar.read

    entriesToList :: (Monad m, MonadError String m, Show a) => String -> Tar.Entries a -> m [Tar.Entry]
    entriesToList err (Tar.Next entry entries) = liftM (entry :) $ entriesToList err entries
    entriesToList _   Tar.Done                 = return []
    entriesToList err (Tar.Fail s)             = throwError ("Could not read '" ++ err ++ "' tarball: " ++ show s)
