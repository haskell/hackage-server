{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.Mirror.Repo (
    -- * Repository types
    SourceRepo(..)
  , TargetRepo(..)
    -- ** Initialization
  , withSourceRepo
  , withTargetRepo
    -- ** Operations on source repos
  , downloadSourceIndex
  , downloadPackage
    -- ** Operations on target repos
  , readCachedTargetIndex
  , authenticate
  , uploadPackage
  , packageExists
    -- ** Finalizing a mirror
  , finalizeMirror
  , cacheTargetIndex
  ) where

-- stdlib
import Control.Exception
import Control.Monad
import System.Directory

-- Cabal
import Distribution.Package (PackageId)
import Distribution.Verbosity

-- hackage
import Distribution.Client hiding (downloadIndex)
import Distribution.Client.Mirror.Config
import Distribution.Client.Mirror.Session
import Distribution.Client.Mirror.Repo.Types
import Distribution.Client.Mirror.Repo.Util
import qualified Distribution.Client.Mirror.Repo.Hackage2 as Hackage2
import qualified Distribution.Client.Mirror.Repo.Local    as Local
import qualified Distribution.Client.Mirror.Repo.Secure   as Secure

-- hackage-security
import qualified Hackage.Security.Client.Repository.HttpLib as Sec

{-------------------------------------------------------------------------------
  Initialization

  TODO: Should really call validateHackageURI'.
-------------------------------------------------------------------------------}

withSourceRepo :: Verbosity
               -> Sec.HttpLib
               -> FilePath
               -> PreRepo
               -> (SourceRepo -> IO a) -> IO a
withSourceRepo verbosity httpLib cacheDir PreRepo{..} callback =
    case (preRepoType, preRepoURI) of
      (Nothing, _) ->
        repoError "Missing type"
      (_, Nothing) ->
        repoError "Missing URI"
      (Just "hackage2", Just uri) ->
        Hackage2.withSourceRepo uri cacheDir callback
      (Just "secure", Just uri) -> do
        Secure.withSourceRepo verbosity
                              httpLib
                              uri
                              cacheDir
                              preRepoThreshold
                              preRepoKeys
                              callback
      _otherwise ->
        repoError "Unknown repo type"
  where
    repoError :: String -> IO a
    repoError msg = throwIO $ userError $ "Repository " ++ show preRepoName
                                       ++ ": " ++ msg

withTargetRepo :: FilePath -> PreRepo -> (TargetRepo -> IO a) -> IO a
withTargetRepo cacheDir PreRepo{..} callback =
    case (preRepoType, preRepoURI) of
      (Nothing, _) ->
        repoError "missing type"
      (_, Nothing) ->
        repoError "Missing URI"
      (Just "hackage2", Just uri) ->
        Hackage2.withTargetRepo uri cacheDir callback
      (Just "local", Just uri) ->
        Local.withTargetRepo uri cacheDir callback
      _otherwise ->
        repoError "Unknown repo type"
  where
    repoError :: String -> IO a
    repoError msg = throwIO $ userError $ "Repository " ++ show preRepoName
                                       ++ ": " ++ msg

{-------------------------------------------------------------------------------
  Operations on source repositories
-------------------------------------------------------------------------------}

-- | Download the index from the source repo
downloadSourceIndex :: SourceRepo -> MirrorSession [PkgIndexInfo]
downloadSourceIndex SourceHackage2{..} =
    Hackage2.downloadIndex sourceRepoURI
                           sourceRepoCachePath
downloadSourceIndex SourceSecure{..} =
    Secure.downloadIndex sourceRepository
                         sourceRepoCache
                         sourceRepoRootKeys
                         sourceRepoThreshold

-- | Download a package
downloadPackage :: SourceRepo
                -> PackageId
                -> FilePath
                -> FilePath
                -> MirrorSession (Maybe GetError)
downloadPackage SourceHackage2{..} =
    Hackage2.downloadPackage sourceRepoURI
downloadPackage SourceSecure{..} =
    Secure.downloadPackage sourceRepository

{-------------------------------------------------------------------------------
  Operations on target repositories
-------------------------------------------------------------------------------}

-- | Read cached index from a target repo
--
-- (Download it if it's not available)
readCachedTargetIndex :: Verbosity -> TargetRepo -> MirrorSession [PkgIndexInfo]
readCachedTargetIndex verbosity targetRepo = do
    cachedExists <- liftIO $ doesFileExist cachedIndex
    if cachedExists
      then do
        when (verbosity >= verbose) $
          liftIO $ putStrLn "Reading cached index for target repo"
        readIndex "cached index" cachedIndex
      else do
        when (verbosity >= normal) $
          liftIO $ putStrLn "No cached index available for target repo"
        downloadTargetIndex targetRepo
  where
    cachedIndex :: FilePath
    cachedIndex = targetCachedIndexPath (targetRepoCachePath targetRepo)

-- | Download the index from a target repo
downloadTargetIndex :: TargetRepo -> MirrorSession [PkgIndexInfo]
downloadTargetIndex TargetHackage2{..} =
    Hackage2.downloadIndex targetRepoURI targetRepoCachePath
downloadTargetIndex TargetLocal{..} =
    Local.downloadIndex targetRepoPath targetRepoCachePath

-- | Authenticate (if required)
authenticate :: TargetRepo -> MirrorSession ()
authenticate TargetHackage2{..} =
    Hackage2.authenticate targetRepoURI
authenticate TargetLocal{..} =
    return () -- no authentication required

-- | Upload a package
uploadPackage :: TargetRepo
              -> Bool
              -> PkgIndexInfo
              -> FilePath
              -> FilePath
              -> MirrorSession ()
uploadPackage targetRepo doMirrorUploaders pkgInfo locCab locTgz =
     case targetRepo of
       TargetHackage2{..} ->
         Hackage2.uploadPackage targetRepoURI
                                doMirrorUploaders
                                pkgInfo
                                locCab
                                locTgz
       TargetLocal{..} ->
         -- doMirrorUploaders and locCab not relevant for local repo
         Local.uploadPackage targetRepoPath
                             pkgInfo
                             locTgz

-- | Check if a package already exists
--
-- Currently always returns 'False' for remote repos.
packageExists :: TargetRepo
              -> PkgIndexInfo
              -> MirrorSession Bool
packageExists targetRepo pkgInfo =
     case targetRepo of
       TargetHackage2{} ->
         return False
       TargetLocal{..} ->
         Local.packageExists targetRepoPath pkgInfo

{-------------------------------------------------------------------------------
  Finalizing
-------------------------------------------------------------------------------}

-- | Finalize the mirror
--
-- That is, now that the packages have been uploaded to the target repo,
-- update the index and securify files (if applicable). This is only necessary
-- "dumb" target repositories.
finalizeMirror :: SourceRepo -> TargetRepo -> MirrorSession ()
finalizeMirror _ TargetHackage2{} =
     return () -- Nothing to do
finalizeMirror SourceHackage2{..} TargetLocal{..} =
     Hackage2.finalizeLocalMirror sourceRepoCachePath targetRepoPath
finalizeMirror SourceSecure{..} TargetLocal{..} =
     Secure.finalizeLocalMirror sourceRepoCache targetRepoPath

-- | Cache the the index for the target repo
cacheTargetIndex :: SourceRepo -> TargetRepo -> MirrorSession ()
cacheTargetIndex SourceHackage2{..} targetRepo = do
     Hackage2.cacheTargetIndex sourceRepoCachePath (targetRepoCachePath targetRepo)
cacheTargetIndex SourceSecure{..} targetRepo = do
     Secure.cacheTargetIndex sourceRepoCache (targetRepoCachePath targetRepo)
