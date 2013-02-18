{-# LANGUAGE RankNTypes, RecordWildCards, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Distribution.Server.Framework.BackupRestore (
    BackupEntry(..),
    importTar,
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

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Codec.Compression.GZip (decompress)
import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Writer
import Data.Time (UTCTime)
import qualified Data.Time as Time
import System.Locale

import Distribution.Server.Util.Merge
import Distribution.Server.Util.Parse (unpackUTF8)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Map as Map
import Data.Ord (comparing)
import System.FilePath (splitDirectories, joinPath)
import Data.List (isPrefixOf)
import Text.CSV hiding (csv)
import Distribution.Text
import Data.Map (Map)
import Data.List (sortBy)

import Distribution.Server.Framework.BlobStorage (BlobStorage, BlobId, add, fetch)

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

parseUTCTime :: Monad m => String -> String -> m UTCTime
parseUTCTime label str =
    case Time.parseTime defaultTimeLocale "%c" str of
      Nothing -> fail $ "Unable to parse UTC timestamp " ++ label ++ ": " ++ str
      Just x  -> return x

formatUTCTime :: UTCTime -> String
formatUTCTime = Time.formatTime defaultTimeLocale "%c"

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
    go (RestoreAddBlob bs f)  = add store bs    >>= go . f
    go (RestoreGetBlob bid f) = fetch store bid >>= go . f

restoreAddBlob :: ByteString -> Restore BlobId
restoreAddBlob = (`RestoreAddBlob` RestoreDone)

restoreGetBlob :: BlobId -> Restore ByteString
restoreGetBlob = (`RestoreGetBlob` RestoreDone)

--------------------------------------------------------------------------------
-- Import a backup                                                            --
--------------------------------------------------------------------------------

-- featureBackups must contain a SINGLE entry for each feature
importTar :: BlobStorage -> ByteString -> [(String, AbstractRestoreBackup)] -> IO (Maybe String)
importTar store tar featureBackups = do
    finalizers <- evalImport store featureBackups $ do
        fromEntries . Tar.read . decompress $ tar
        finalizeBackups store (map fst featureBackups)
    completeBackups finalizers

-- A variant of importTar that finalizes immediately.
importBlank :: BlobStorage -> [(String, AbstractRestoreBackup)] -> IO (Maybe String)
importBlank store featureBackups = do
    finalizers <- evalImport store featureBackups $
        finalizeBackups store (map fst featureBackups)
    completeBackups finalizers

-- | Call restoreFinalize for every backup. Caller must ensure that every
-- feature name is in the map.
finalizeBackups :: BlobStorage -> [String] -> Import [IO ()]
finalizeBackups store list = forM list $ \name -> do
    features <- gets featureMap
    mbackup <- liftIO $ abstractRestoreFinalize (features Map.! name) store
    case mbackup of
        Left err       -> do -- TODO: instead: fail $ "Error finalizing feature " ++ name ++ ":" ++ err
                             liftIO . putStrLn $ "WARNING: Error finalizing feature " ++  name ++ ":" ++ err
                             return (return ())
        Right finalize -> return finalize

completeBackups :: Either String [IO ()] -> IO (Maybe String)
completeBackups res = case res of
    Left err -> return $ Just err
    Right putSt -> sequence_ putSt >> return Nothing

-- internal import utils

newtype Import a = Import { unImp :: StateT FeatureImportState (ErrorT String IO) a }
  deriving (Monad, MonadIO, MonadState FeatureImportState)

evalImport :: BlobStorage -> [(String, AbstractRestoreBackup)] -> Import a -> IO (Either String a)
evalImport store featureBackups imp = runErrorT (evalStateT (unImp imp) initState)
  where
    initState = initialFeatureImportState store featureBackups

data FeatureImportState = FeatureImportState {
    -- | The 'RestoreBackup' of each state
    featureMap :: Map String AbstractRestoreBackup

    -- | The blob storage
  , featureStorage :: BlobStorage

    -- | Mapping from strings to blob IDs
    --
    -- TODO: The tar format we use is
    --
    --   export-{date}/blobs/{blobId}
    --   ..
    --   export-{date}/core/{package}/{package}.tar.gz --> ../../blobs/{blobId}
    --
    -- These {blobId}s are strings, however, and we cannot easily convert a
    -- string into a blobID (in particular, there is a Show instance for
    -- MD5Digest but no Read instance). We therefore maintain a mapping from
    -- string's to blobIds -- whenever we encounter
    --
    --   export-{date}/blobs/{blobId}
    --
    -- we add the file to the blob storage; this gets us a "real" blob ID, and
    -- we update this mapping. Note however that this is inefficient for two
    -- reasons: first of all, there should be no need to maintain this mapping
    -- (once we have a Read instance for MD5Digest). Secondly, when we 'add'
    -- a blob to the storage, we /recompute/ its hash. This is not necessary:
    -- we already /know/ the hash.
  , featureBlobs :: Map String BlobId
  }

initialFeatureImportState :: BlobStorage -> [(String, AbstractRestoreBackup)] -> FeatureImportState
initialFeatureImportState store featureBackups = FeatureImportState {
    featureMap     = Map.fromList featureBackups
  , featureBlobs   = Map.empty
  , featureStorage = store
  }

updateFeatureMap :: String -> AbstractRestoreBackup -> Import ()
updateFeatureMap feature backup =
  modify (\st -> st { featureMap = Map.insert feature backup (featureMap st) })

updateFeatureBlobs :: String -> BlobId -> Import ()
updateFeatureBlobs str blobid =
  modify (\st -> st { featureBlobs = Map.insert str blobid (featureBlobs st) })

fromEntries :: Tar.Entries Tar.FormatError -> Import ()
fromEntries Tar.Done        = return ()
fromEntries (Tar.Fail err)  = fail (show err)
fromEntries (Tar.Next x xs) = fromEntry x >> fromEntries xs

fromEntry :: Tar.Entry -> Import ()
fromEntry entry = case Tar.entryContent entry of
        Tar.NormalFile bytes _ -> fromFile (Tar.entryPath entry) bytes
        Tar.Directory {} -> return () -- ignore directory entries
        Tar.SymbolicLink target -> fromLink (Tar.entryPath entry) (Tar.fromLinkTarget target)
        _ -> fail $ "Unexpected Tar.Entry: " ++ Tar.entryPath entry

fromFile :: FilePath -> ByteString -> Import ()
fromFile path contents = case splitDirectories path of
  baseDir:rest | "export-" `isPrefixOf` baseDir ->
    case rest of
      ["blobs", blobIdStr] -> do
        store <- gets featureStorage
        blobId <- liftIO $ add store contents
        updateFeatureBlobs blobIdStr blobId
      _ ->
        fromBackupEntry rest (`BackupByteString` contents)
  _ ->
    return () -- ignore unknown files

fromLink :: FilePath -> FilePath -> Import ()
fromLink path linkTarget =
  -- links have format "../../../blobs/blobId" (for some number of "../"s)
  case (splitDirectories path, reverse (splitDirectories linkTarget)) of
    (baseDir : rest, blobIdStr : "blobs" : _) | "export-" `isPrefixOf` baseDir -> do
      mBlobId <- gets (Map.lookup blobIdStr . featureBlobs)
      case mBlobId of
        Just blobId -> fromBackupEntry rest (`BackupBlob` blobId)
        Nothing     -> fail $ "Unknown blob ID " ++ blobIdStr
    _ ->
      return ()

fromBackupEntry :: [FilePath] -> ([FilePath] -> BackupEntry) -> Import ()
fromBackupEntry path@(pathFront:pathEnd) mkEntry = do
  store    <- gets featureStorage
  features <- gets featureMap
  case Map.lookup pathFront features of
    Just restorer -> do
      res <- liftIO $ abstractRestoreEntry restorer store (mkEntry pathEnd)
      case res of Left e          -> fail $ "Error importing '" ++ joinPath path ++ "' :" ++ e
                  Right restorer' -> restorer' `seq` updateFeatureMap pathFront restorer'
    Nothing ->
      return ()
fromBackupEntry _ _ = return ()

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
    readTar :: Monad m => String -> ByteString -> m [Tar.Entry]
    readTar err = entriesToList err . Tar.read . decompress

    entriesToList :: (Monad m, Show a) => String -> Tar.Entries a -> m [Tar.Entry]
    entriesToList err (Tar.Next entry entries) = liftM (entry :) $ entriesToList err entries
    entriesToList _   Tar.Done                 = return []
    entriesToList err (Tar.Fail s)             = fail ("Could not read '" ++ err ++ "' tarball: " ++ show s)
