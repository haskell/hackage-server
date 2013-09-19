{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards, DoRec #-}
module Distribution.Server.Features.Core (
    CoreFeature(..),
    CoreResource(..),
    initCoreFeature,

    -- * Change events
    PackageChange(..),
    isPackageChangeAny,
    isPackageAdd,
    isPackageDelete,
    isPackageIndexChange,

    -- * Misc other utils
    packageExists,
    packageIdExists,
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.Core.State
import Distribution.Server.Features.Core.Backup

import Distribution.Server.Features.Users

import Distribution.Server.Packages.Types
import Distribution.Server.Users.Types (UserId)
import qualified Distribution.Server.Packages.Index as Packages.Index
import qualified Codec.Compression.GZip as GZip
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage

import Data.Time.Clock (UTCTime)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Lazy (ByteString)

import Distribution.Text (display)
import Distribution.Package
import Distribution.Version (Version(..))


data CoreFeature = CoreFeature {
    coreFeatureInterface :: HackageFeature,

    coreResource     :: CoreResource,

    -- queries
    queryGetPackageIndex :: MonadIO m => m (PackageIndex PkgInfo),

    -- update transactions
    updateAddPackage         :: MonadIO m => PackageId ->
                                CabalFileText -> UploadInfo ->
                                Maybe PkgTarball -> m Bool,
    updateDeletePackage      :: MonadIO m => PackageId -> m Bool,
    updateAddPackageRevision :: MonadIO m => PackageId ->
                                CabalFileText -> UploadInfo -> m (),
    updateAddPackageTarball  :: MonadIO m => PackageId ->
                                PkgTarball -> UploadInfo -> m Bool,
    updateSetPackageUploader :: MonadIO m => PackageId -> UserId -> m Bool,
    updateSetPackageUploadTime :: MonadIO m => PackageId -> UTCTime -> m Bool,

    -- | Set an entry in the 00-index.tar file.
    -- The 00-index.tar file contains all the package entries, but it is an
    -- extensible format and we can add more stuff. E.g. version preferences
    -- or crypto signatures.
    updateArchiveIndexEntry  :: MonadIO m => String -> (ByteString, UTCTime) -> m (),

    -- | Notification of package or index changes
    packageChangeHook :: Hook PackageChange (),

    -- | Notification of downloads
    packageDownloadHook :: Hook PackageId ()
}

instance IsHackageFeature CoreFeature where
    getFeatureInterface = coreFeatureInterface

-- | This is designed so that you can pattern match on just the kinds of
-- events you are interested in.
data PackageChange = PackageChangeAdd    PkgInfo
                   | PackageChangeDelete PkgInfo
                   | PackageChangeInfo   PkgInfo PkgInfo
                   | PackageChangeIndexExtra String ByteString UTCTime

isPackageChangeAny :: PackageChange -> Maybe (PackageId, Maybe PkgInfo)
isPackageChangeAny (PackageChangeAdd        pkginfo) = Just (packageId pkginfo, Just pkginfo)
isPackageChangeAny (PackageChangeDelete     pkginfo) = Just (packageId pkginfo, Nothing)
isPackageChangeAny (PackageChangeInfo     _ pkginfo) = Just (packageId pkginfo, Just pkginfo)
isPackageChangeAny  PackageChangeIndexExtra {}       = Nothing

isPackageAdd :: PackageChange -> Maybe PkgInfo
isPackageAdd (PackageChangeAdd pkginfo) = Just pkginfo
isPackageAdd _                          = Nothing

isPackageDelete :: PackageChange -> Maybe PkgInfo
isPackageDelete (PackageChangeDelete pkginfo) = Just pkginfo
isPackageDelete _                             = Nothing

isPackageIndexChange ::  PackageChange -> Maybe ()
isPackageIndexChange _ = Just ()

{-
-- Other examples we may want later...
isPackageAddVersion                :: Maybe PackageId,
isPackageDeleteVersion             :: Maybe PackageId,
isPackageChangeCabalFile           :: Maybe (PackageId, CabalFileText),
isPackageChangeCabalFileUploadInfo :: Maybe (PackageId, UploadInfo),
isPackageChangeTarball             :: Maybe (PackageId, PkgTarball),
isPackageIndexExtraChange          :: Maybe (String, ByteString, UTCTime)
-}

data CoreResource = CoreResource {
    corePackagesPage    :: Resource,
    corePackagePage     :: Resource,
    coreCabalFile       :: Resource,
    corePackageTarball  :: Resource,

    -- Render resources
    indexPackageUri    :: String -> String,
    corePackageIdUri   :: String -> PackageId -> String,
    corePackageNameUri :: String -> PackageName -> String,
    coreCabalUri       :: PackageId -> String,
    coreTarballUri     :: PackageId -> String,

    -- Find a PackageId or PackageName inside a path
    packageInPath :: (MonadPlus m, FromReqURI a) => DynamicPath -> m a,

    -- TODO: This is a rather ad-hoc function. Do we really need it?
    packageTarballInPath :: MonadPlus m => DynamicPath -> m PackageId,

    -- Check that a package exists (guard fails if version is empty)
    guardValidPackageId   :: PackageId   -> ServerPartE (),
    guardValidPackageName :: PackageName -> ServerPartE (),

    -- Find a package in the package DB
    lookupPackageName :: PackageName -> ServerPartE [PkgInfo],
    lookupPackageId   :: PackageId   -> ServerPartE PkgInfo
}

initCoreFeature :: ServerEnv -> UserFeature -> IO CoreFeature
initCoreFeature env@ServerEnv{serverStateDir, serverCacheDelay,
                              serverVerbosity = verbosity} users = do
    loginfo verbosity "Initialising core feature, start"

    -- Canonical state
    packagesState <- packagesStateComponent verbosity serverStateDir

    -- Ephemeral state
    -- Additional files to put in the index tarball like preferred-versions
    extraMap <- newMemStateWHNF Map.empty

    -- Hooks
    packageChangeHook   <- newHook
    packageDownloadHook <- newHook

    rec let (feature, getIndexTarball)
              = coreFeature env users
                            packagesState extraMap indexTar
                            packageChangeHook packageDownloadHook

        -- Caches
        -- The index.tar.gz file
        indexTar <- newAsyncCacheNF getIndexTarball
                      defaultAsyncCachePolicy {
                        asyncCacheName = "index tarball",
                        asyncCacheUpdateDelay  = serverCacheDelay,
                        asyncCacheSyncInit     = False,
                        asyncCacheLogVerbosity = verbosity
                      }

    registerHookJust packageChangeHook isPackageIndexChange $ \_ ->
      prodAsyncCache indexTar

    loginfo verbosity "Initialising core feature, end"
    return feature


packagesStateComponent :: Verbosity -> FilePath -> IO (StateComponent AcidState PackagesState)
packagesStateComponent verbosity stateDir = do
  let stateFile = stateDir </> "db" </> "PackagesState"
  st <- logTiming verbosity "Loaded PackagesState" $
          openLocalStateFrom stateFile initialPackagesState
  return StateComponent {
       stateDesc    = "Main package database"
     , stateHandle  = st
     , getState     = query st GetPackagesState
     , putState     = update st . ReplacePackagesState
     , backupState  = indexToAllVersions
     , restoreState = packagesBackup
     , resetState   = packagesStateComponent verbosity
     }

coreFeature :: ServerEnv
            -> UserFeature
            -> StateComponent AcidState PackagesState
            -> MemState (Map String (ByteString, UTCTime))
            -> AsyncCache ByteString
            -> Hook PackageChange ()
            -> Hook PackageId ()
            -> ( CoreFeature
               , IO ByteString )

coreFeature ServerEnv{serverBlobStore = store} UserFeature{..}
            packagesState indexExtras cacheIndexTarball
            packageChangeHook packageDownloadHook
  = (CoreFeature{..}, getIndexTarball)
  where
    coreFeatureInterface = (emptyHackageFeature "core") {
        featureDesc = "Core functionality"
      , featureResources = [
            coreIndexTarball
          , corePackagesPage
          , corePackagePage
          , corePackageRedirect
          , corePackageTarball
          , coreCabalFile
          ]
      , featureState    = [abstractAcidStateComponent packagesState]
      , featureCaches   = [
            CacheComponent {
              cacheDesc       = "main package index tarball",
              getCacheMemSize = memSize <$> readAsyncCache cacheIndexTarball
            }
          , CacheComponent {
              cacheDesc       = "package index extra files",
              getCacheMemSize = memSize <$> readMemState indexExtras
            }
          ]
      , featurePostInit = syncAsyncCache cacheIndexTarball
      }

    -- the rudimentary HTML resources are for when we don't want an additional HTML feature
    coreResource = CoreResource {..}
    coreIndexTarball = (resourceAt "/packages/index.tar.gz") {
        resourceDesc = [(GET, "tarball of package descriptions")]
      , resourceGet  = [("tarball", servePackagesIndex)]
      }
    corePackagesPage = (resourceAt "/packages/.:format") {
        resourceGet = [] -- have basic packages listing?
      }
    corePackagePage = resourceAt "/package/:package.:format"
    corePackageRedirect = (resourceAt "/package/") {
        resourceDesc = [(GET,  "Redirect to /packages/")]
      , resourceGet  = [("", \_ -> seeOther "/packages/" $ toResponse ())]
      }
    corePackageTarball = (resourceAt "/package/:package/:tarball.tar.gz") {
        resourceDesc = [(GET, "Get package tarball")]
      , resourceGet  = [("tarball", servePackageTarball)]
      }
    coreCabalFile = (resourceAt "/package/:package/:cabal.cabal") {
        resourceDesc = [(GET, "Get package .cabal file")]
      , resourceGet  = [("cabal", serveCabalFile)]
      }
    indexPackageUri = \format ->
      renderResource corePackagesPage [format]
    corePackageIdUri  = \format pkgid ->
      renderResource corePackagePage [display pkgid, format]
    corePackageNameUri = \format pkgname ->
      renderResource corePackagePage [display pkgname, format]
    coreCabalUri    = \pkgid ->
      renderResource coreCabalFile [display pkgid, display (packageName pkgid)]
    coreTarballUri  = \pkgid ->
      renderResource corePackageTarball [display pkgid, display pkgid]

    packageInPath dpath = maybe mzero return (lookup "package" dpath >>= fromReqURI)

    packageTarballInPath dpath = do
      PackageIdentifier name version <- packageInPath dpath
      case lookup "tarball" dpath >>= fromReqURI of
        Nothing -> mzero
        Just pkgid@(PackageIdentifier name' version') -> do
          -- rules:
          -- * the package name and tarball name must be the same
          -- * the tarball must specify a version
          -- * the package must either have no version or the same version as the tarball
          guard $ name == name' && version' /= Version [] [] && (version == version' || version == Version [] [])
          return pkgid

    guardValidPackageId pkgid = do
      guard (pkgVersion pkgid /= Version [] [])
      void $ lookupPackageId pkgid

    guardValidPackageName pkgname =
      void $ lookupPackageName pkgname

    -- Queries
    --
    queryGetPackageIndex :: MonadIO m => m (PackageIndex PkgInfo)
    queryGetPackageIndex = return . packageList =<< queryState packagesState GetPackagesState

    -- Update transactions
    --
    updateAddPackage :: MonadIO m => PackageId
                     -> CabalFileText -> UploadInfo
                     -> Maybe PkgTarball -> m Bool
    updateAddPackage pkgid cabalFile uploadinfo mtarball = do
      mpkginfo <- updateState packagesState
                   (AddPackage pkgid cabalFile uploadinfo mtarball)
      case mpkginfo of
        Nothing -> return False
        Just pkginfo -> do
          runHook_ packageChangeHook (PackageChangeAdd pkginfo)
          return True

    updateDeletePackage :: MonadIO m => PackageId -> m Bool
    updateDeletePackage pkgid = do
      mpkginfo <- updateState packagesState (DeletePackage pkgid)
      case mpkginfo of
        Nothing -> return False
        Just pkginfo -> do
          runHook_ packageChangeHook (PackageChangeDelete pkginfo)
          return True

    updateAddPackageRevision :: MonadIO m => PackageId -> CabalFileText -> UploadInfo -> m ()
    updateAddPackageRevision pkgid cabalfile uploadinfo = do
      (moldpkginfo, newpkginfo) <- updateState packagesState (AddPackageRevision pkgid cabalfile uploadinfo)
      case moldpkginfo of
        Nothing ->
          runHook_ packageChangeHook  (PackageChangeAdd newpkginfo)
        Just oldpkginfo ->
          runHook_ packageChangeHook  (PackageChangeInfo oldpkginfo newpkginfo)

    updateAddPackageTarball :: MonadIO m => PackageId -> PkgTarball -> UploadInfo -> m Bool
    updateAddPackageTarball pkgid tarball uploadinfo = do
      mpkginfo <- updateState packagesState (AddPackageTarball pkgid tarball uploadinfo)
      case mpkginfo of
        Nothing -> return False
        Just (oldpkginfo, newpkginfo) -> do
          runHook_ packageChangeHook  (PackageChangeInfo oldpkginfo newpkginfo)
          return True

    updateSetPackageUploader pkgid userid = do
      mpkginfo <- updateState packagesState (SetPackageUploader pkgid userid)
      case mpkginfo of
        Nothing -> return False
        Just (oldpkginfo, newpkginfo) -> do
          runHook_ packageChangeHook  (PackageChangeInfo oldpkginfo newpkginfo)
          return True

    updateSetPackageUploadTime pkgid time = do
      mpkginfo <- updateState packagesState (SetPackageUploadTime pkgid time)
      case mpkginfo of
        Nothing -> return False
        Just (oldpkginfo, newpkginfo) -> do
          runHook_ packageChangeHook  (PackageChangeInfo oldpkginfo newpkginfo)
          return True

    updateArchiveIndexEntry :: MonadIO m => String -> (ByteString, UTCTime) -> m ()
    updateArchiveIndexEntry entryName entryDetails@(entryData, entryTime) = do
      modifyMemState indexExtras (Map.insert entryName entryDetails)
      runHook_ packageChangeHook (PackageChangeIndexExtra entryName entryData entryTime)

    -- Cache updates
    --
    getIndexTarball :: IO ByteString
    getIndexTarball = do
      users  <- queryGetUserDb  -- note, changes here don't automatically propagate
      index  <- queryGetPackageIndex
      extras <- readMemState indexExtras
      let indexTarball' = GZip.compress (Packages.Index.write users extras index)
      return indexTarball'

    ------------------------------------------------------------------------------
    packageError :: [MessageSpan] -> ServerPartE a
    packageError = errNotFound "Package not found"

    lookupPackageName :: PackageName -> ServerPartE [PkgInfo]
    lookupPackageName pkgname = do
      pkgsIndex <- queryGetPackageIndex
      case PackageIndex.lookupPackageName pkgsIndex pkgname of
        []   -> packageError [MText "No such package in package index"]
        pkgs -> return pkgs

    lookupPackageId :: PackageId -> ServerPartE PkgInfo
    lookupPackageId (PackageIdentifier name (Version [] [])) = do
      pkgs <- lookupPackageName name
      -- pkgs is sorted by version number and non-empty
      return (last pkgs)
    lookupPackageId pkgid = do
      pkgsIndex <- queryGetPackageIndex
      case PackageIndex.lookupPackageId pkgsIndex pkgid of
        Just pkg -> return pkg
        _ -> packageError [MText $ "No such package version for " ++ display (packageName pkgid)]

    ------------------------------------------------------------------------

    servePackagesIndex :: DynamicPath -> ServerPartE Response
    servePackagesIndex _ = do
      indexTarball <- readAsyncCache cacheIndexTarball
      return $ toResponse (Resource.IndexTarball indexTarball)

    -- result: tarball or not-found error
    servePackageTarball :: DynamicPath -> ServerPartE Response
    servePackageTarball dpath = do
      pkgid <- packageTarballInPath dpath
      guard (pkgVersion pkgid /= Version [] [])
      pkg <- lookupPackageId pkgid
      case pkgTarball pkg of
          [] -> errNotFound "Tarball not found" [MText "No tarball exists for this package version."]
          ((tb, _):_) -> do
              let blobId = pkgTarballGz tb
              file <- liftIO $ BlobStorage.fetch store blobId
              runHook_ packageDownloadHook pkgid
              return $ toResponse $ Resource.PackageTarball file blobId (pkgUploadTime pkg)

    -- result: cabal file or not-found error
    serveCabalFile :: DynamicPath -> ServerPartE Response
    serveCabalFile dpath = do
      pkg <- packageInPath dpath >>= lookupPackageId
      -- check that the cabal name matches the package
      case lookup "cabal" dpath == Just (display $ packageName pkg) of
          True  -> return $ toResponse (Resource.CabalFile (cabalFileByteString (pkgData pkg)))
          False -> mzero

packageExists, packageIdExists :: (Package pkg, Package pkg') => PackageIndex pkg -> pkg' -> Bool
packageExists   pkgs pkg = not . null $ PackageIndex.lookupPackageName pkgs (packageName pkg)
packageIdExists pkgs pkg = maybe False (const True) $ PackageIndex.lookupPackageId pkgs (packageId pkg)

