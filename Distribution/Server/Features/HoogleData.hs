{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             RankNTypes, NamedFieldPuns, RecordWildCards,
             RecursiveDo, BangPatterns, CPP #-}
module Distribution.Server.Features.HoogleData (
    initHoogleDataFeature,
    HoogleDataFeature(..),
  ) where

import Distribution.Server.Framework hiding (path)
import Distribution.Server.Framework.BlobStorage (BlobId)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage

import Distribution.Server.Features.Core
import Distribution.Server.Features.Documentation
import Distribution.Server.Features.TarIndexCache

import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Data.TarIndex as TarIndex

import Distribution.Package
import Distribution.Text

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib.Internal as Zlib

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import Data.Serialize (runGetLazy, runPutLazy)
import Data.SafeCopy (SafeCopy, safeGet, safePut)

import Data.Maybe
import Control.Monad.State
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Directory
import System.FilePath
import Control.Concurrent.MVar
import Control.Concurrent.Async (async, wait)
import Control.Exception
import qualified System.IO.Error as IOError


-- | A feature to serve up a tarball of hoogle files, for the hoogle client.
--
data HoogleDataFeature = HoogleDataFeature {
    hoogleDataFeatureInterface :: HackageFeature
}

instance IsHackageFeature HoogleDataFeature where
  getFeatureInterface = hoogleDataFeatureInterface


----------------------------------------
-- Feature definition & initialisation
--

initHoogleDataFeature :: ServerEnv
                      -> IO (CoreFeature
                          -> DocumentationFeature
                          -> TarIndexCacheFeature
                          -> IO HoogleDataFeature)
initHoogleDataFeature env@ServerEnv{ serverCacheDelay,
                                     serverVerbosity = verbosity } = do
    -- Ephemeral state
    docsUpdatedState <- newMemStateWHNF Set.empty

    hoogleBundleUpdateJob <- newAsyncUpdate serverCacheDelay verbosity
                                            "hoogle.tar.gz"

    return $ \core docs tarIndexCache -> do
      let feature = hoogleDataFeature docsUpdatedState
                                      hoogleBundleUpdateJob
                                      env core docs tarIndexCache

      return feature


hoogleDataFeature :: MemState (Set PackageId)
                  -> AsyncUpdate
                  -> ServerEnv
                  -> CoreFeature
                  -> DocumentationFeature
                  -> TarIndexCacheFeature
                  -> HoogleDataFeature
hoogleDataFeature docsUpdatedState hoogleBundleUpdateJob
                  ServerEnv{serverBlobStore = store, serverStateDir}
                  CoreFeature{..} DocumentationFeature{..}
                  TarIndexCacheFeature{..}
  = HoogleDataFeature {..}

  where
    hoogleDataFeatureInterface = (emptyHackageFeature "hoogle-data") {
        featureDesc      = "Provide a tarball of all package's hoogle files"
      , featureResources = [hoogleBundleResource]
      , featureState     = []
      , featureCaches    = []
      , featurePostInit  = postInit
      }

    -- Resources
    --

    hoogleBundleResource =
      (resourceAt "/packages/hoogle.tar.gz") {
        resourceDesc   = [ (GET, "get the tarball of hoogle files for all packages")
                         ]
      , resourceGet    = [ ("tarball", serveHoogleData) ]
      }

    -- Request handlers
    --

    featureStateDir = serverStateDir </> "db" </> "HoogleData"
    bundleTarGzFile = featureStateDir </> "hoogle.tar.gz"
    bundleCacheFile = featureStateDir </> "cache"

    serveHoogleData :: DynamicPath -> ServerPartE Response
    serveHoogleData _ =
        -- serve the cached hoogle.tar.gz file
        serveFile (asContentType "application/x-gzip") bundleTarGzFile

    postInit :: IO ()
    postInit = do
        createDirectoryIfMissing False featureStateDir
        prodFileCacheUpdate
        registerHook documentationChangeHook $ \pkgid -> do
          modifyMemState docsUpdatedState (Set.insert pkgid)
          prodFileCacheUpdate

    prodFileCacheUpdate :: IO ()
    prodFileCacheUpdate =
       asyncUpdate hoogleBundleUpdateJob updateHoogleBundle

    -- Actually do the update. Here we are guaranteed that we're only doing
    -- one update at once, no concurrent updates.
    updateHoogleBundle :: IO ()
    updateHoogleBundle = do
       docsUpdated <- readMemState  docsUpdatedState
       writeMemState docsUpdatedState Set.empty
       updated <- maybeWithFile bundleTarGzFile $ \mhOldTar -> do
         mcache <- readCacheFile bundleCacheFile
         let docEntryCache = maybe Map.empty fst mcache
             oldTarPkgids  = maybe Set.empty snd mcache
             tmpdir = featureStateDir
         updateTarBundle mhOldTar tmpdir
                         docEntryCache oldTarPkgids
                         docsUpdated
       case updated of
         Nothing -> return ()
         Just (docEntryCache', newTarPkgids, newTarFile) -> do
           renameFile newTarFile bundleTarGzFile
           writeCacheFile bundleCacheFile (docEntryCache', newTarPkgids)

    updateTarBundle :: Maybe Handle -> FilePath
                    -> Map PackageId (Maybe (BlobId, TarEntryOffset))
                    -> Set PackageId
                    -> Set PackageId
                    -> IO (Maybe (Map PackageId (Maybe (BlobId, TarEntryOffset))
                                 ,Set PackageId, FilePath))
    updateTarBundle mhOldTar tmpdir docEntryCache oldTarPkgids docsUpdated = do

      -- Invalidate cached info about any package docs that have been updated
      let docEntryCache' = docEntryCache `Map.difference` fromSet docsUpdated
          cachedPkgids   = fromSet (oldTarPkgids `Set.difference` docsUpdated)

      -- get the package & docs index
      pkgindex <- queryGetPackageIndex
      docindex <- queryDocumentationIndex

      -- Select the package ids that have corresponding docs that contain a
      -- hoogle .txt file.
      -- We prefer later package versions, but if a later one is missing the
      -- hoogle .txt file then we fall back to older ones.
      --
      -- For the package ids we pick we keep the associated doc tarball blobid
      -- and the offset of the hoogle .txt file within that tarball.
      --
      -- Looking up if a package's docs contains the hoogle .txt file is
      -- expensive (have to read the doc tarball's index) so we maintain a
      -- cache of that information.
      (selectedPkgids, docEntryCache'') <-
        -- use a state monad for access to and updating the cache
        flip runStateT docEntryCache' $
          fmap (Map.fromList . catMaybes) $
          sequence
            [ findFirstCached (lookupHoogleEntry docindex)
                              (reverse (map packageId pkgs))
            | pkgs <- PackageIndex.allPackagesByName pkgindex ]

          -- the set of pkgids to try to reuse from the existing tar file
      let reusePkgs :: Map PackageId ()
          reusePkgs = cachedPkgids `Map.intersection` selectedPkgids

          -- the packages where we need to read it fresh
          readFreshPkgs :: Map PackageId (BlobId, TarEntryOffset)
          readFreshPkgs = selectedPkgids `Map.difference` reusePkgs

      if Map.null readFreshPkgs && Map.keysSet reusePkgs == oldTarPkgids
        then return Nothing
        else liftM  Just $
          withTempFile tmpdir "newtar" $ \hNewTar newTarFile ->
          withWriter (tarWriter hNewTar) $ \putEntry -> do

            -- We truncate on tar format errors. This works for the empty case
            -- and should be self-correcting for real errors. It just means we
            -- miss a few entries from the tarball 'til next time its updated.
            oldEntries <- case mhOldTar of
              Nothing      -> return []
              Just hOldTar -> do
                contents <- BS.hGetContents hOldTar
                return . Tar.foldEntries (:) [] (const [])
                       . Tar.read
                       . BS.fromChunks
                       . Zlib.foldDecompressStreamWithInput (:) (\_ -> []) (\_ -> [])
                         (Zlib.decompressST Zlib.gzipFormat Zlib.defaultDecompressParams)
                       $ contents

            -- Write out the cached ones
            sequence_
              [ putEntry entry
              | entry <- oldEntries
              , pkgid <- maybeToList (entryPkgId entry)
              , pkgid `Map.member` reusePkgs ]

            -- Write out the new/changed ones
            sequence_
              [ withFile doctarfile ReadMode $ \hDocTar -> do
                  mentry <- newCacheTarEntry pkgid hDocTar taroffset
                  maybe (return ()) putEntry mentry
              | (pkgid, (doctarblobid, taroffset)) <- Map.toList readFreshPkgs
              , let doctarfile = BlobStorage.filepath store doctarblobid ]

            return (docEntryCache'', Map.keysSet selectedPkgids, newTarFile)

    lookupHoogleEntry :: Map PackageId BlobId -> PackageId -> IO (Maybe (BlobId, TarEntryOffset))
    lookupHoogleEntry docindex pkgid
      | Just doctarblobid <- Map.lookup pkgid docindex
      = do doctarindex <- cachedTarIndex doctarblobid
           case lookupPkgDocHoogleFile pkgid doctarindex of
             Nothing     -> return Nothing
             Just offset -> return (Just (doctarblobid, offset))
      | otherwise = return Nothing

fromSet :: Ord a => Set a -> Map a ()
fromSet = Map.fromAscList . map (\x -> (x, ())) . Set.toAscList

-- | Like list 'find' but with a monadic lookup function and we cache the
-- results of that lookup function.
--
findFirstCached :: (Ord a, Monad m)
                => (a -> m (Maybe b))
                -> [a] -> StateT (Map a (Maybe b)) m (Maybe (a, b))
findFirstCached _ []     = return Nothing
findFirstCached f (x:xs) = do
    cache <- get
    case Map.lookup x cache of
      Just m_y -> checkY m_y
      Nothing  -> do
        m_y <- lift (f x)
        put (Map.insert x m_y cache)
        checkY m_y
  where
    checkY Nothing  = findFirstCached f xs
    checkY (Just y) = return (Just (x, y))

withTempFile :: FilePath -> String -> (Handle -> FilePath -> IO a) -> IO a
withTempFile tmpdir template action =
    mask $ \restore -> do
      (fname, hnd) <- openTempFile tmpdir template
      x <- restore (action hnd fname)
             `onException` (hClose hnd >> removeFile fname)
      hClose hnd
      return x

maybeWithFile :: FilePath -> (Maybe Handle -> IO a) -> IO a
maybeWithFile file action =
  mask $ \unmask -> do
    mhnd <- try $ openFile file ReadMode
    case mhnd of
      Right hnd -> unmask (action (Just hnd)) `finally` hClose hnd
      Left  e    | IOError.isDoesNotExistError e
                 , Just file == IOError.ioeGetFileName e
                -> unmask (action Nothing)
      Left  e   -> throw e

readCacheFile :: SafeCopy a => FilePath -> IO (Maybe a)
readCacheFile file =
    maybeWithFile file $ \mhnd ->
      case mhnd of
        Nothing  -> return Nothing
        Just hnd -> do
          content <- BS.hGetContents hnd
          case runGetLazy safeGet content of
            Left  _ -> return Nothing
            Right x -> return (Just x)

writeCacheFile :: SafeCopy a => FilePath -> a -> IO ()
writeCacheFile file x =
    BS.writeFile file (runPutLazy (safePut x))

lookupPkgDocHoogleFile :: PackageId -> TarIndex -> Maybe TarEntryOffset
lookupPkgDocHoogleFile pkgid index = do
    TarFileEntry offset <- TarIndex.lookup index path
    return offset
  where
    path = (display pkgid ++ "-docs") </> display (packageName pkgid) <.> "txt"

newCacheTarEntry :: PackageId -> Handle -> TarEntryOffset -> IO (Maybe Tar.Entry)
newCacheTarEntry pkgid htar offset
  | Just entrypath <- hoogleDataTarPath pkgid = do
    morigEntry <- readTarEntryAt htar offset
    case morigEntry of
      Nothing        -> return Nothing
      Just origEntry ->
        return $ Just
          (Tar.simpleEntry entrypath (Tar.entryContent origEntry)) {
            Tar.entryTime = Tar.entryTime origEntry
          }

  | otherwise = return Nothing

hoogleDataTarPath :: PackageId -> Maybe Tar.TarPath
hoogleDataTarPath pkgid =
    either (const Nothing) Just (Tar.toTarPath False filepath)
  where
    -- like zlib/0.5.4.1/doc/html/zlib.txt
    filepath = joinPath [ display (packageName pkgid)
                        , display (packageVersion pkgid)
                        , "doc", "html"
                        , display (packageName pkgid) <.> "txt" ]

entryPkgId  :: Tar.Entry -> Maybe PackageId
entryPkgId = parseEntryPath . Tar.entryPath

parseEntryPath :: FilePath -> Maybe PackageId
parseEntryPath filename
  | [namestr, verstr,
     "doc", "html",
     filestr]          <- splitDirectories filename
  , Just pkgname       <- simpleParse namestr
  , Just pkgver        <- simpleParse verstr
  , (namestr', ".txt") <- splitExtension filestr
  , Just pkgname'      <- simpleParse namestr'
  , pkgname == pkgname'
  = Just (PackageIdentifier pkgname pkgver)

  | otherwise
  = Nothing

readTarEntryAt :: Handle -> TarEntryOffset -> IO (Maybe Tar.Entry)
readTarEntryAt htar off = do
  hSeek htar AbsoluteSeek (fromIntegral (off * 512))
  header <- BS.hGet htar 512
  case Tar.read header of
    (Tar.Next entry@Tar.Entry{Tar.entryContent = Tar.NormalFile _ size} _) -> do
         content <- BS.hGet htar (fromIntegral size)
         return $ Just entry { Tar.entryContent = Tar.NormalFile content size }
    _ -> return Nothing


data Writer a = Writer { wWrite :: a -> IO (), wClose :: IO () }

withWriter :: IO (Writer b) -> ((b -> IO ()) -> IO a) -> IO a
withWriter mkwriter action = bracket mkwriter wClose (action . wWrite)

tarWriter :: Handle -> IO (Writer Tar.Entry)
tarWriter hnd = do
    chan <- newBChan
    awriter <- async $ do
      entries <- getBChanContents chan
      BS.hPut hnd ((GZip.compress . Tar.write) entries)
    return Writer {
      wWrite = writeBChan chan,
      wClose = do closeBChan chan
                  wait awriter
    }

newtype BChan a = BChan (MVar (Maybe a))

newBChan :: IO (BChan a)
newBChan = liftM BChan newEmptyMVar

writeBChan :: BChan a -> a -> IO ()
writeBChan (BChan c) = putMVar c . Just

closeBChan :: BChan a -> IO ()
closeBChan (BChan c) = putMVar c Nothing

getBChanContents :: BChan a -> IO [a]
getBChanContents (BChan c) = do
    res <- takeMVar c
    case res of
      Nothing -> return []
      Just x  -> do xs <- unsafeInterleaveIO (getBChanContents (BChan c))
                    return (x : xs)

