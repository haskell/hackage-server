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
import Data.Digest.Pure.MD5 (md5)
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage

import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import Distribution.Text (display)
import Distribution.Package
import Distribution.Version (Version(..))

import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector         as Vector
import qualified Data.Text           as Text

-- | The core feature, responsible for the main package index and all access
-- and modifications of it.
--
-- All packages must have a Cabal file, uploader, and upload time, and may have
-- a source tarball.
data CoreFeature = CoreFeature {
    -- | The core `HackageFeature`.
    coreFeatureInterface :: HackageFeature,

    -- | Core package resources and combinators.
    coreResource :: CoreResource,

    -- Queries
    -- | Retrieves the entire main package index.
    queryGetPackageIndex :: MonadIO m => m (PackageIndex PkgInfo),

    -- Update transactions
    -- | Adds a version of a package which did not previously exist in the
    -- index. This requires a Cabal file and context, and optionally a
    -- reference to a tarball blob, and does not do any consistency checking
    -- of these.
    --
    -- If a package was able to be newly added, runs a `PackageChangeAdd` hook
    -- when done and returns True.
    updateAddPackage         :: MonadIO m => PackageId ->
                                CabalFileText -> UploadInfo ->
                                Maybe PkgTarball -> m Bool,
    -- | Deletes a version of an existing package, deleting the package if it
    -- was the last version.
    --
    -- If a package was found and deleted, runs a `PackageChangeDelete` hook
    -- when done and returns True.
    updateDeletePackage      :: MonadIO m => PackageId -> m Bool,

    -- | Adds a new Cabal file for this package version, creating it if
    -- necessary. Previous Cabal files are kept around.
    --
    -- Runs either a `PackageChangeAdd` or `PackageChangeInfo` hook, depending
    -- on whether a package with the given version already existed.
    updateAddPackageRevision :: MonadIO m => PackageId ->
                                CabalFileText -> UploadInfo -> m (),
    -- | Sets the source tarball for an existing package version. References to
    -- previous tarballs, if any, are kept around.
    --
    -- If this package was found, runs a `PackageChangeInfo` hook when done and
    -- returns True.
    updateAddPackageTarball  :: MonadIO m => PackageId ->
                                PkgTarball -> UploadInfo -> m Bool,
    -- | Sets the uploader of an existing package version.
    --
    -- If this package was found, runs a `PackageChangeInfo` hook when done and
    -- returns True.
    updateSetPackageUploader :: MonadIO m => PackageId -> UserId -> m Bool,
    -- | Sets the upload time of an existing package version.
    --
    -- If this package was found, runs a `PackageChangeInfo` hook when done and
    -- returns True.
    updateSetPackageUploadTime :: MonadIO m => PackageId -> UTCTime -> m Bool,

    -- | Set an entry in the 00-index.tar file.
    --
    -- The 00-index.tar file contains all the package entries, but it is an
    -- extensible format and we can add more stuff. E.g. version preferences
    -- or crypto signatures. This requires a file name, file contents, and
    -- modification time for the tar entry.
    --
    -- This runs a `PackageChangeIndexExtra` hook when done.
    updateArchiveIndexEntry  :: MonadIO m => String -> (ByteString, UTCTime) -> m (),

    -- | Notification of package or index changes.
    packageChangeHook :: Hook PackageChange (),

    -- | Notification of tarball downloads.
    packageDownloadHook :: Hook PackageId ()
}

instance IsHackageFeature CoreFeature where
    getFeatureInterface = coreFeatureInterface

-- | This is designed so that you can pattern match on just the kinds of
-- events you are interested in.
data PackageChange
    -- | A package was newly added with this `PkgInfo`.
    = PackageChangeAdd    PkgInfo
    -- | A package was deleted, and this `PkgInfo` is no longer accessible in
    -- the package index.
    | PackageChangeDelete PkgInfo
    -- | A package was updated from the first `PkgInfo` to the second.
    | PackageChangeInfo   PkgInfo PkgInfo
    -- | A file has changed in the package index tar not covered by any of the
    -- other change types.
    | PackageChangeIndexExtra String ByteString UTCTime

-- | A predicate to use with `packageChangeHook` and `registerHookJust` for
-- keeping other features synchronized with the main package index.
--
-- This indicates an update for a given `PackageId`, and the new `PkgInfo` if
-- a new one has been added (`Nothing` in the case of deletion).
isPackageChangeAny :: PackageChange -> Maybe (PackageId, Maybe PkgInfo)
isPackageChangeAny (PackageChangeAdd        pkginfo) = Just (packageId pkginfo, Just pkginfo)
isPackageChangeAny (PackageChangeDelete     pkginfo) = Just (packageId pkginfo, Nothing)
isPackageChangeAny (PackageChangeInfo     _ pkginfo) = Just (packageId pkginfo, Just pkginfo)
isPackageChangeAny  PackageChangeIndexExtra {}       = Nothing

-- | A predicate to use with `packageChangeHook` and `registerHookJust` for
-- newly added packages.
isPackageAdd :: PackageChange -> Maybe PkgInfo
isPackageAdd (PackageChangeAdd pkginfo) = Just pkginfo
isPackageAdd _                          = Nothing

-- | A predicate to use with `packageChangeHook` and `registerHookJust` for
-- deleted packages.
isPackageDelete :: PackageChange -> Maybe PkgInfo
isPackageDelete (PackageChangeDelete pkginfo) = Just pkginfo
isPackageDelete _                             = Nothing

-- | A predicate to use with `packageChangeHook` and `registerHookJust` for
-- any kind of change to packages or extras.
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
    -- | The collection all packages.
    corePackagesPage    :: Resource,
    -- | An individual package.
    corePackagePage     :: Resource,
    -- | A Cabal file for a package version.
    coreCabalFile       :: Resource,
    -- | A tarball for a package version.
    corePackageTarball  :: Resource,

    -- Rendering resources.
    -- | URI for `corePackagesPage`, given a format (blank for none).
    indexPackageUri    :: String -> String,
    -- | URI for `corePackagePage`, given a format and `PackageId`.
    corePackageIdUri   :: String -> PackageId -> String,
    -- | URI for `corePackagePage`, given a format and `PackageName`.
    corePackageNameUri :: String -> PackageName -> String,
    -- | URI for `coreCabalFile`, given a PackageId.
    coreCabalUri       :: PackageId -> String,
    -- | URI for `corePackageTarball`, given a PackageId.
    coreTarballUri     :: PackageId -> String,

    -- | Find a PackageId or PackageName inside a path.
    packageInPath :: (MonadPlus m, FromReqURI a) => DynamicPath -> m a,

    -- | Find a tarball's PackageId from inside a path, doing some checking
    -- for consistency between the package and tarball.
    --
    -- TODO: This is a rather ad-hoc function. Do we really need it?
    packageTarballInPath :: MonadPlus m => DynamicPath -> m PackageId,

    -- | Check that a particular version of a package exists (guard fails if
    -- version is empty)
    guardValidPackageId   :: PackageId   -> ServerPartE (),
    -- | Check that a package exists.
    guardValidPackageName :: PackageName -> ServerPartE (),

    -- | Find a package in the package DB, failing if not found. This uses the
    -- highest version number of a package.
    --
    -- In the presence of deprecation or preferred versions,
    -- `withPackagePreferred` should generally be used instead for user-facing
    -- version resolution.
    lookupPackageName :: PackageName -> ServerPartE [PkgInfo],
    -- | Find a package version in the package DB, failing if not found. Behaves
    -- like `lookupPackageName` if the version is empty.
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
     , backupState  = \_ -> indexToAllVersions
     , restoreState = packagesBackup
     , resetState   = packagesStateComponent verbosity
     }

coreFeature :: ServerEnv
            -> UserFeature
            -> StateComponent AcidState PackagesState
            -> MemState (Map String (ByteString, UTCTime))
            -> AsyncCache IndexTarball
            -> Hook PackageChange ()
            -> Hook PackageId ()
            -> ( CoreFeature
               , IO IndexTarball )

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
        resourceDesc = [(GET, "List of all packages")]
      , resourceGet  = [("json", servePackageList)]
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
    getIndexTarball :: IO IndexTarball
    getIndexTarball = do
      users  <- queryGetUserDb  -- note, changes here don't automatically propagate
      index  <- queryGetPackageIndex
      extras <- readMemState indexExtras
      time   <- getCurrentTime
      let indexTarball = GZip.compress (Packages.Index.write users extras index)
      return $! IndexTarball indexTarball (fromIntegral $ BS.length indexTarball)
                             (md5 indexTarball) time

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
      tarball@(IndexTarball _ _ tarballmd5 _) <- readAsyncCache cacheIndexTarball
      useETag (ETag (show tarballmd5))
      return (toResponse tarball)

    -- TODO: should we include more information here? description and
    -- category for instance (but they are not readily available as long
    -- as we don't keep the parsed cabal files in memory)
    servePackageList :: DynamicPath -> ServerPartE Response
    servePackageList _ = do
      pkgIndex <- queryGetPackageIndex
      let pkgs = PackageIndex.allPackagesByName pkgIndex
          list = [display . pkgName . pkgInfoId $ pkg | pkg <- map head pkgs]
      -- We construct the JSON manually so that we control what it looks like;
      -- in particular, we use objects for the packages so that we can add
      -- additional fields later without (hopefully) breaking clients
      let json = flip map list $ \str ->
            Object . HashMap.fromList $ [
                (Text.pack "packageName", String (Text.pack str))
              ]
      return . toResponse $ Array (Vector.fromList json)

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
              useETag (BlobStorage.blobETag blobId)
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
-- | Whether a package exists in the given package index.
packageExists   pkgs pkg = not . null $ PackageIndex.lookupPackageName pkgs (packageName pkg)
-- | Whether a particular package version exists in the given package index.
packageIdExists pkgs pkg = maybe False (const True) $ PackageIndex.lookupPackageId pkgs (packageId pkg)

