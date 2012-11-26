{-# LANGUAGE RankNTypes, MultiParamTypeClasses, RecordWildCards, FlexibleInstances, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Distribution.Server.Framework.BackupRestore (
    RestoreBackup(..),
    BackupEntry(..),
    TestRoundtrip,
    Import,
    importTar,
    importBlank,

    importCSV,
    runImport,
    getImport,
    withSubImport,
    parseText,
    parseTime,
    timeFormatSpec,
    parseRead,

    equalTarBall,

    module Distribution.Server.Util.Merge
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
import System.FilePath (splitDirectories)
import Data.List (isPrefixOf)
import Text.CSV hiding (csv)
import qualified Control.Exception as Exception
import Distribution.Text
import Data.Map (Map)

import Distribution.Server.Framework.BlobStorage (BlobId)

-- | Used to test that the backup/restore stuff works.
--
-- The outermost IO action takes an immutable snapshot of the internal state
-- of the feature. The innermost IO action compares that to the new state and
-- returns a list of errors if any problems are found.
type TestRoundtrip = IO (IO [String])

data BackupEntry =
    BackupByteString [FilePath] ByteString
  | BackupBlob [FilePath] BlobId

data RestoreBackup = RestoreBackup {
    restoreEntry :: [FilePath] -> ByteString -> IO (Either String RestoreBackup),
    -- Final checks and transformations on the accumulated data.
    -- For the most part, it combines partial data accumulated during importing
    -- tar entries, failing if bits are missing. It should not fail if no
    -- entries have been imported.
    -- It is called before restoreComplete.
    restoreFinalize :: IO (Either String RestoreBackup),
    -- This is where Happstack updating and any additional state manipulating should happen.
    -- This should \*not\* fail. Features may rely on the state of their dependencies (this
    -- is useful for analyses of data in other features that can be recalculated on import)
    restoreComplete :: IO ()
}
instance Monoid RestoreBackup where
    mempty = RestoreBackup
        { restoreEntry    = \_ _ -> return . Right $ mempty
        , restoreFinalize = return . Right $ mempty
        , restoreComplete = return ()
        }
    mappend (RestoreBackup run fin comp) (RestoreBackup run' fin' comp') = RestoreBackup
        { restoreEntry = \path bs -> do
              res <- run path bs
              case res of
                  Right backup -> do
                      res' <- run' path bs
                      return $ fmap (mappend backup) res'
                  Left bad -> return $ Left bad
        , restoreFinalize = do
              res <- fin
              case res of
                  Right backup -> do
                      res' <- fin'
                      return $ fmap (mappend backup) res'
                  Left bad -> return $ Left bad
        , restoreComplete = comp >> comp'
        }

-- featureBackups must contain a SINGLE entry for each feature
importTar :: ByteString -> [(String, RestoreBackup)] -> IO (Maybe String)
importTar tar featureBackups = do
    res <- runImport (Map.fromList featureBackups) $ do
        fromEntries . Tar.read . decompress $ tar
        finalizeBackups (map fst featureBackups)
    completeBackups res

-- A variant of importTar that finalizes immediately.
importBlank :: [(String, RestoreBackup)] -> IO (Maybe String)
importBlank featureBackups = do
    res <- runImport (Map.fromList featureBackups)
         $ finalizeBackups (map fst featureBackups)
    completeBackups res

-- | Call restoreFinalize for every backup. Caller must ensure that every
-- feature name is in the map.
finalizeBackups :: [String] -> Import FeatureMap ()
finalizeBackups list = forM_ list $ \name -> do
    featureMap <- get
    mbackup <- liftIO $ restoreFinalize (featureMap Map.! name)
    case mbackup of
        Left err      -> fail $ "Error finalizing feature " ++ name ++ ":" ++ err
        Right backup' -> put (Map.insert name backup' featureMap)

completeBackups :: Either String FeatureMap -> IO (Maybe String)
completeBackups res = case res of
    Left err -> return $ Just err
    Right featureMap -> do
        mapM_ restoreComplete (Map.elems featureMap)
        return Nothing

-- internal import utils
type FeatureMap = Map String RestoreBackup

fromEntries :: Tar.Entries Tar.FormatError -> Import FeatureMap ()
fromEntries Tar.Done = return ()
fromEntries (Tar.Fail err) = fail (show err)
fromEntries (Tar.Next x xs) = fromEntry x >> fromEntries xs

fromEntry :: Tar.Entry -> Import FeatureMap ()
fromEntry entry = case Tar.entryContent entry of
        Tar.NormalFile bytes _ -> fromFile (Tar.entryPath entry) bytes
        Tar.Directory {} -> return () -- ignore directory entries
        _ -> fail $ "Unexpected Tar.Entry: " ++ Tar.entryPath entry

fromFile :: FilePath -> ByteString -> Import FeatureMap ()
fromFile path contents
    = let paths = splitDirectories path
      in case paths of
           baseDir:rest | "export-" `isPrefixOf` baseDir -> go rest
           _ -> return () -- ignore unknown files
 where
    go (pathFront:pathEnd) = do
        featureMap <- get
        case Map.lookup pathFront featureMap of
            Just restorer -> do
                res <- liftIO $ restoreEntry restorer pathEnd contents
                case res of Left e          -> fail $ "Error importing '" ++ path ++ "' :" ++ e
                            Right restorer' -> restorer' `seq` put (Map.insert pathFront restorer' featureMap)
            Nothing -> return ()
    go _ = return ()


-- Used to compare export/import tarballs for equality by the backup/restore test:

equalTarBall :: ByteString -- ^ "Before" tarball
             -> ByteString -- ^ "After" tarball
             -> [String]
equalTarBall tar1 tar2 = runFailable_ $ do
    (entries1, entries2) <- liftA2 (,) (readTar "before" tar1) (readTar "after" tar2)
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

newtype Import s a = Imp {unImp :: forall r. (String -> IO r) -- error
                                          -> (a -> s -> IO r) -- success
                                          -> s -- state
                                          -> IO r }

runImport :: s -> Import s a -> IO (Either String s)
runImport initState imp = unImp imp err k initState
  where k _ s = return $ Right s
        err   = return . Left

getImport :: s -> Import s a -> IO (Either String (s, a))
getImport initState imp = unImp imp err k initState
  where k a s = return $ Right (s, a)
        err   = return . Left

-- For nested import. Not as useful in practice.
withSubImport :: (s -> s') -> (s' -> s -> s) -> Import s' a -> Import s a
withSubImport extractState returnState subImport = do
    initState <- get
    let subState = extractState initState
    mresult <- liftIO $ getImport subState subImport
    case mresult of
        Right (subState', result) -> put (returnState subState' initState) >> return result
        Left  err -> fail err

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

-- | Made customParseCSV into a nifty combinator.
importCSV :: String -> ByteString -> (CSV -> Import s a) -> Import s a
importCSV filename inp comb = case customParseCSV filename (unpackUTF8 inp) of
    Left  err -> fail err
    Right csv -> comb csv

parseRead :: Read a => String -> String -> Import s a
parseRead label str = case reads str of
    [(value, "")] -> return value
    _ -> fail $ "Unable to parse " ++ label ++ ": " ++ show str

parseTime :: String -> Import s UTCTime
parseTime str = case Time.parseTime defaultTimeLocale timeFormatSpec str of
    Nothing -> fail $ "Unable to parse time: " ++ str
    Just x  -> return x

-- | Time/Date format used in exported files.
-- Variant on ISO formatted date, with time and time zone.
timeFormatSpec :: String
timeFormatSpec = "%Y-%m-%d %H:%M:%S %z"

-- Parse a string, throw an error if it's bad
parseText :: Text a => String -> String -> Import s a
parseText label text = case simpleParse text of
    Nothing -> fail $ "Unable to parse " ++ label ++ ": " ++ show text
    Just a -> return a
