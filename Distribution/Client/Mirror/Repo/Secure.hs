{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.Mirror.Repo.Secure (
    withSourceRepo
  , downloadIndex
  , downloadPackage
  , finalizeLocalMirror
  , cacheTargetIndex
  ) where

-- stdlib
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Cont
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime)
import Network.URI (URI)
import System.Directory
import System.FilePath
import System.IO
import qualified Data.ByteString.Lazy as BS.L

-- Cabal
import Distribution.Package
import Distribution.Verbosity

-- hackage
import Distribution.Client hiding (downloadIndex)
import Distribution.Client.Mirror.Session
import Distribution.Client.Mirror.Repo.Util
import Distribution.Client.Mirror.Repo.Types

-- hackage-security
import qualified Hackage.Security.Client                    as Sec
import qualified Hackage.Security.Client.Repository.Cache   as Sec.Cache
import qualified Hackage.Security.Client.Repository.HttpLib as Sec
import qualified Hackage.Security.Client.Repository.Remote  as Sec.Remote
import qualified Hackage.Security.Util.Checked              as Sec
import qualified Hackage.Security.Util.Path                 as Sec
import qualified Hackage.Security.Util.Pretty               as Sec

withSourceRepo :: Verbosity
               -> Sec.HttpLib
               -> URI
               -> FilePath
               -> Maybe Sec.KeyThreshold
               -> Maybe [Sec.KeyId]
               -> (SourceRepo -> IO a) -> IO a
withSourceRepo verbosity httpLib uri cacheDir threshold keys callback = do
    cacheDir' <- Sec.makeAbsolute (Sec.fromFilePath cacheDir)

    -- It is important that we get the compressed index _as it exists_
    -- on the server because we cannot reliably recreate it (with the same
    -- hash) from the uncompressed index. Therefore we record in the cache
    -- layout where we want the compressed index to be stored, and we tell
    -- the repository that it should always download the compressed index.

    let rp :: Sec.Path Sec.Unrooted -> Sec.CachePath
        rp = Sec.rootPath

        cache :: Sec.Cache.Cache
        cache = Sec.Cache.Cache {
            Sec.Cache.cacheRoot   = cacheDir'
          , Sec.Cache.cacheLayout = Sec.cabalCacheLayout {
                Sec.cacheLayoutIndexTarGz = rp $ Sec.fragment "00-index.tar.gz"
              }
          }

        repoOpts :: Sec.Remote.RepoOpts
        repoOpts = Sec.Remote.defaultRepoOpts {
            Sec.Remote.repoAllowAdditionalMirrors = False
          }

        logger :: Sec.LogMessage -> IO ()
        logger msg = when (verbosity >= verbose) $
                       putStrLn $ Sec.pretty msg

    Sec.Remote.withRepository
      httpLib
      [uri]
      repoOpts
      cache
      Sec.hackageRepoLayout
      Sec.hackageIndexLayout
      logger $ \rep ->
        callback SourceSecure {
            sourceRepository    = rep
          , sourceRepoCache     = cache
          , sourceRepoRootKeys  = fromMaybe [] keys
          , sourceRepoThreshold = fromMaybe (Sec.KeyThreshold 0) threshold
          }

downloadIndex :: Sec.Repository down
              -> Sec.Cache.Cache
              -> [Sec.KeyId]
              -> Sec.KeyThreshold
              -> MirrorSession [PkgIndexInfo]
downloadIndex rep Sec.Cache.Cache{..} rootKeys threshold =
    handleChecked (mirrorError . verificationError) $
    handleChecked (mirrorError . remoteError)       $ do
      _hasUpdates <- liftIO $ do
        requiresBootstrap <- Sec.requiresBootstrap rep
        when requiresBootstrap $ Sec.bootstrap rep rootKeys threshold
        now <- getCurrentTime
        Sec.checkForUpdates rep (Just now)
      -- TODO: Is this hasUpdates values useful anywhere?
      readIndex (show rep) indexPath
  where
    verificationError = GetEntityError EntityIndex . GetVerificationError
    remoteError       = GetEntityError EntityIndex . GetRemoteError

    indexPath = Sec.toFilePath $
      Sec.anchorCachePath cacheRoot (Sec.cacheLayoutIndexTar cacheLayout)

downloadPackage :: Sec.Repository down
                -> PackageId
                -> FilePath
                -> FilePath
                -> MirrorSession (Maybe GetError)
downloadPackage rep pkgId locCab locTgz =
   handleChecked (return . Just . GetInvalidPackage)    $
   handleChecked (return . Just . GetVerificationError) $
   handleChecked (return . Just . GetRemoteError)       $
     liftIO $ do
       Sec.downloadPackage' rep pkgId locTgz
       cabalFile <- Sec.withIndex rep $ \Sec.IndexCallbacks{..} ->
         Sec.trusted `liftM` indexLookupCabal pkgId
       BS.L.writeFile locCab cabalFile
       return Nothing

-- | Finalize the mirror (copy over index and TUF files)
--
-- We copy everything to temporary files in the destination directory first;
-- only when all files have been successfully copied we move them to their final
-- location. This is especially important if the target directory is mounted on,
-- say, an @sshfs@ file system, where the initial copy can be slow. We want to
-- avoid that these files are out of sync with each other for longer than
-- necessary. (In particular, it is important that these files are never out of
-- sync for longer than it takes a client to cycle through a few
-- check-for-update attempts before giving up.)
--
-- This happens _after_ all packages have been copied over, which poses no
-- problems: as long as the index is not updated, these additional packages are
-- not visible to clients.
--
-- Then we copy over the index before the TUF files; in principle, clients who
-- do an incremental update will not be affected by this (but new clients will
-- be, of course). The relative order of updating the @.tar@ versus the
-- @.tar.gz@ is unimportant: clients will never request both.
--
-- Then we update the TUF files in the opposite order that the client needs
-- them: mirrors, root, snapshot, and timestamp. This minimizes how many clients
-- will see an inconsistent state, although it does not eliminate the problem
-- altogether. (For instance, if we update the snapshot before the timestamp, a
-- client downloading the timestamp will see the old timestamp, conclude there
-- are no chances, and never see the modified snapshot.) However, even if we did
-- (by putting all these files in a directory, say, and symlinking the
-- directory) clients could _still_ see an inconsistent state because they
-- request these files one by one. If we really wanted to fix this, we'd have to
-- start versioning files on the server as described in the TUF spec; however,
-- since this is only applies to a few files, and clients will simply retry when
-- they get a verification error, it's not a priority.
finalizeLocalMirror :: Sec.Cache.Cache -> FilePath -> MirrorSession ()
finalizeLocalMirror cache targetRepoPath = liftIO $ do
    repoRoot <- Sec.makeAbsolute $ Sec.fromFilePath targetRepoPath
    finalizeLocalMirror' cache repoRoot

finalizeLocalMirror' :: Sec.Cache.Cache -> Sec.Path Sec.Absolute -> IO ()
finalizeLocalMirror' cache repoRoot = (`runContT` return) $ do
    -- TODO: We need to think about updating these files atomically
    cp Sec.cacheLayoutIndexTar   Sec.repoLayoutIndexTar
    cp Sec.cacheLayoutIndexTarGz Sec.repoLayoutIndexTarGz
    cp Sec.cacheLayoutMirrors    Sec.repoLayoutMirrors
    cp Sec.cacheLayoutRoot       Sec.repoLayoutRoot
    cp Sec.cacheLayoutSnapshot   Sec.repoLayoutSnapshot
    cp Sec.cacheLayoutTimestamp  Sec.repoLayoutTimestamp
  where
    cp :: (Sec.CacheLayout -> Sec.CachePath)
       -> (Sec.RepoLayout  -> Sec.RepoPath)
       -> ContT r IO ()
    cp src dst = copyFileAtomic (cacheFP cache src) (repoFP repoRoot dst)

    copyFileAtomic :: FilePath -> FilePath -> ContT r IO ()
    copyFileAtomic src dst = ContT $ \callback -> do
      let (dir, template) = splitFileName dst
      bracket (openBinaryTempFileWithDefaultPermissions dir template)
              (\(temp, h) -> ignoreIOErrors (hClose h >> removeFile temp)) $
              (\(temp, h) -> do
                 BS.L.hPut h =<< BS.L.readFile src
                 hClose h
                 a <- callback ()
                 renameFile temp dst
                 return a)

    ignoreIOErrors :: IO () -> IO ()
    ignoreIOErrors = handle $ \(_ :: IOException) -> return ()

cacheTargetIndex :: Sec.Cache.Cache -> FilePath -> MirrorSession ()
cacheTargetIndex cache targetCache = liftIO $
     copyFile (cacheFP cache Sec.cacheLayoutIndexTarGz)
              (targetCachedIndexPath targetCache)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

cacheFP :: Sec.Cache.Cache -> (Sec.CacheLayout -> Sec.CachePath) -> FilePath
cacheFP Sec.Cache.Cache{..} f = Sec.toFilePath
                              $ Sec.anchorCachePath cacheRoot
                              $ f cacheLayout

repoFP :: Sec.Path Sec.Absolute -> (Sec.RepoLayout -> Sec.RepoPath) -> FilePath
repoFP repoRoot f = Sec.toFilePath
                  $ Sec.anchorRepoPathLocally repoRoot
                  $ f Sec.hackageRepoLayout

handleChecked :: Exception e
              => (e -> MirrorSession a)
              -> (Sec.Throws e => MirrorSession a)
              -> MirrorSession a
handleChecked handler act = do
    run <- askUnlift
    liftCont (Sec.catchChecked (unlift run act)) handler
