{-# LANGUAGE RankNTypes, MultiParamTypeClasses, RecordWildCards, FlexibleInstances, StandaloneDeriving, KindSignatures, GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Distribution.Server.Framework.BackupRestore (
    BackupEntry(..),
    importTar,
    importBlank,

    importCSV,
    parseText,
    parseTime,
    timeFormatSpec,
    parseRead,

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
import Control.Monad.State.Class
import Control.Monad.Writer
import Data.Time (UTCTime)
import qualified Data.Time as Time
import System.Locale

import Distribution.Server.Util.Merge
import Distribution.Server.Util.Parse (unpackUTF8)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Foldable (sequenceA_, traverse_)
import qualified Data.Map as Map
import Data.Ord (comparing)
import System.FilePath (splitDirectories, joinPath)
import Data.List (isPrefixOf)
import Text.CSV hiding (csv)
import qualified Control.Exception as Exception
import Distribution.Text
import Data.Map (Map)
import Data.List (sortBy)

import Distribution.Server.Framework.BlobStorage (BlobStorage, BlobId, add, fetch)

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


data Restore :: * -> * where
  RestoreDone :: forall a. a -> Restore a
  RestoreBind :: Restore a -> (a -> Restore b) -> Restore b
  RestoreFail :: forall a. String -> Restore a
  RestoreAddBlob :: ByteString -> Restore BlobId
  RestoreGetBlob :: BlobId -> Restore ByteString

instance Functor Restore where
  f `fmap` x = x >>= return . f

instance Monad Restore where
  return = RestoreDone
  (>>=)  = RestoreBind
  fail   = RestoreFail

runRestore :: BlobStorage -> Restore a -> IO (Either String a)
runRestore store = go
  where
    go :: forall a. Restore a -> IO (Either String a)
    go (RestoreDone a)      = return (Right a)
    go (RestoreBind x f)    = do mx' <- go x
                                 case mx' of
                                   Left err -> return (Left err)
                                   Right x' -> go (f x')
    go (RestoreFail err)    = return (Left err)
    go (RestoreAddBlob bs)  = Right <$> add store bs
    go (RestoreGetBlob bid) = Right <$> fetch store bid

restoreAddBlob :: ByteString -> Restore BlobId
restoreAddBlob = RestoreAddBlob

restoreGetBlob :: BlobId -> Restore ByteString
restoreGetBlob = RestoreGetBlob

-- featureBackups must contain a SINGLE entry for each feature
importTar :: BlobStorage -> ByteString -> [(String, AbstractRestoreBackup)] -> IO (Maybe String)
importTar store tar featureBackups = do
    finalizers <- evalImport (initialFeatureImportState store featureBackups) $ do
        fromEntries . Tar.read . decompress $ tar
        finalizeBackups store (map fst featureBackups)
    completeBackups finalizers

-- A variant of importTar that finalizes immediately.
importBlank :: BlobStorage -> [(String, AbstractRestoreBackup)] -> IO (Maybe String)
importBlank store featureBackups = do
    finalizers <- evalImport (initialFeatureImportState store featureBackups)
         $ finalizeBackups store (map fst featureBackups)
    completeBackups finalizers

-- | Call restoreFinalize for every backup. Caller must ensure that every
-- feature name is in the map.
finalizeBackups :: BlobStorage -> [String] -> Import FeatureImportState [IO ()]
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

updateFeatureMap :: String -> AbstractRestoreBackup -> Import FeatureImportState ()
updateFeatureMap feature backup =
  modify (\st -> st { featureMap = Map.insert feature backup (featureMap st) })

updateFeatureBlobs :: String -> BlobId -> Import FeatureImportState ()
updateFeatureBlobs str blobid =
  modify (\st -> st { featureBlobs = Map.insert str blobid (featureBlobs st) })

fromEntries :: Tar.Entries Tar.FormatError -> Import FeatureImportState ()
fromEntries Tar.Done        = return ()
fromEntries (Tar.Fail err)  = fail (show err)
fromEntries (Tar.Next x xs) = fromEntry x >> fromEntries xs

fromEntry :: Tar.Entry -> Import FeatureImportState ()
fromEntry entry = case Tar.entryContent entry of
        Tar.NormalFile bytes _ -> fromFile (Tar.entryPath entry) bytes
        Tar.Directory {} -> return () -- ignore directory entries
        Tar.SymbolicLink target -> fromLink (Tar.entryPath entry) (Tar.fromLinkTarget target)
        _ -> fail $ "Unexpected Tar.Entry: " ++ Tar.entryPath entry

fromFile :: FilePath -> ByteString -> Import FeatureImportState ()
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

fromLink :: FilePath -> FilePath -> Import FeatureImportState ()
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

fromBackupEntry :: [FilePath] -> ([FilePath] -> BackupEntry) -> Import FeatureImportState ()
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

-- Used to compare export/import tarballs for equality by the backup/restore test:

equalTarBall :: ByteString -- ^ "Before" tarball
             -> ByteString -- ^ "After" tarball
             -> [String]
equalTarBall tar1 tar2 = runFailable_ $ do
    entries1 <- sortBy (comparing Tar.entryTarPath) <$> readTar "before" tar1
    entries2 <- sortBy (comparing Tar.entryTarPath) <$> readTar "after"  tar2
    flip traverse_ (mergeBy (comparing Tar.entryTarPath) entries1 entries2) $ \mr -> case mr of
        OnlyInLeft  entry -> fail $ Tar.entryPath entry ++ " only in 'before' tarball"
        OnlyInRight entry -> fail $ Tar.entryPath entry ++ " only in 'after' tarball"
        InBoth entry1 entry2 -> sequenceA_ [
              checkEq "content" Tar.entryContent,
              checkEq "permissions" Tar.entryPermissions,
              checkEq "ownership" Tar.entryOwnership
              -- Don't particularly care about modification time/tar format
            ]
          where
            tarPath = Tar.entryPath entry1
            checkEq :: Eq a => String -> (Tar.Entry -> a) -> Failable ()
            checkEq what f = when (f entry1 /= f entry2) $ fail $ tarPath ++ ": " ++ what ++ " did not match"
  where
    readTar err = entriesToList err . Tar.read . decompress

    entriesToList err (Tar.Next entry entries) = liftM (entry :) $ entriesToList err entries
    entriesToList _   Tar.Done                 = return []
    entriesToList err (Tar.Fail s)             = fail ("Could not read '" ++ err ++ "' tarball: " ++ show s)

data Failable a = Failed [String] | NotFailed a

runFailable_ :: Failable () -> [String]
runFailable_ (Failed errs)  = errs
runFailable_ (NotFailed ()) = []

instance Functor Failable where fmap = liftM

instance Applicative Failable where
    pure = return
    Failed errs1 <*> Failed errs2 = Failed (errs1 ++ errs2)
    Failed errs  <*> NotFailed _  = Failed errs
    NotFailed _  <*> Failed errs  = Failed errs
    NotFailed f  <*> NotFailed x  = NotFailed (f x)

instance Monad Failable where
    return = NotFailed
    NotFailed x >>= f = f x
    Failed errs >>= _ = Failed errs
    fail = Failed . return


{-
Some utilities worth recreating for the newer Import scheme, mostly
to import documentation:

addFile :: ByteString -> Import BlobId
addFile file = do
  store <- gets isStorage
  io2imp $ BlobStorage.add store file

addTarIndex :: BlobId -> Import ()
addTarIndex blob
    = do
  store <- gets isStorage
  let tarFile = BlobStorage.filepath store blob
  index <- io2imp $ TarIndex.readTarIndex tarFile
  modify $ \is -> is { isTarIndex = TarIndexMap.insertTarIndex blob index (isTarIndex is) }
-}

-- implementation of the Import data type

concatM :: (Monad m) => [a -> m a] -> (a -> m a)
concatM fs = foldr (>=>) return fs


newtype Import s a = Imp {unImp :: forall r. (String -> IO r) -- error
                                          -> (a -> s -> IO r) -- success
                                          -> s -- state
                                          -> IO r }

evalImport :: s -> Import s a -> IO (Either String a)
evalImport initState imp = unImp imp err k initState
  where k a _ = return $ Right a
        err   = return . Left

instance Functor (Import s) where
    f `fmap` g = Imp $ \err k st -> unImp g err (k . f) st

instance Monad (Import s) where
    return a = Imp $ \_ k st -> k a st

    m >>= f
      = Imp $ \err k -> unImp m err $ \a -> unImp (f a) err k

    m >> n
      = Imp $ \err k ->
        unImp m err $ \_ ->
        unImp n err k

    fail str
        = Imp $ \err _k _s -> err str

instance MonadState s (Import s) where
    get   = Imp $ \_ k s -> k s s
    put s = Imp $ \_ k _ -> {- s `seq` -} k () s

-- not sure if this is as good as it could be,
-- but it seems that k must be fully applied if I want
-- to unwrap x
instance MonadIO (Import s) where
  --liftIO x = Imp $ \_ k s -> x >>= \v -> k v s
    liftIO x = Imp $ \err k s -> do
        Exception.catch (x >>= \v -> k v s)
                        (\e -> let msg = "Caught exception: " ++ show (e :: Exception.SomeException) in err msg)

-- | Chops off the last entry if it's null.
-- I'm not sure why parseCSV does this, but it's
-- irritating.
customParseCSV :: String -> String -> Either String CSV
customParseCSV filename inp = case parseCSV filename inp of
    Left err -> Left . show $ err
    Right csv -> Right . chopLastRecord $ csv
 where
   chopLastRecord [] = []
   chopLastRecord ([""]:[]) = []
   chopLastRecord (x:xs) = x : chopLastRecord xs

importCSV :: Monad m => FilePath -> ByteString -> m CSV
importCSV filename inp = case customParseCSV filename (unpackUTF8 inp) of
  Left err  -> fail err
  Right csv -> return csv

parseRead :: (Read a, Monad m) => String -> String -> m a
parseRead label str = case reads str of
    [(value, "")] -> return value
    _ -> fail $ "Unable to parse " ++ label ++ ": " ++ show str

parseTime :: Monad m => String -> m UTCTime
parseTime str = case Time.parseTime defaultTimeLocale timeFormatSpec str of
    Nothing -> fail $ "Unable to parse time: " ++ str
    Just x  -> return x

-- | Time/Date format used in exported files.
-- Variant on ISO formatted date, with time and time zone.
timeFormatSpec :: String
timeFormatSpec = "%Y-%m-%d %H:%M:%S%Q %z"

-- Parse a string, throw an error if it's bad
parseText :: (Text a, Monad m) => String -> String -> m a
parseText label text = case simpleParse text of
    Nothing -> fail $ "Unable to parse " ++ label ++ ": " ++ show text
    Just a -> return a
