{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}
module Distribution.Server.Features.Core (
    CoreFeature(..),
    CoreResource(..),
    initCoreFeature,

    -- * Change events
    PackageUpdate(..),
    PackageChange(..),
    isPackageChangeAny,
    isPackageAdd,
    isPackageDelete,
    isPackageIndexChange,

    -- * Misc other utils
    packageExists,
    packageIdExists,
  ) where

-- stdlib
import qualified Codec.Compression.GZip                             as GZip
import           Data.Aeson                                         (Value (..))
import           Data.ByteString.Lazy                               (ByteString)
import qualified Data.Foldable                                      as Foldable
import qualified Data.HashMap.Strict                                as HashMap
import qualified Data.Text                                          as Text
import           Data.Time.Clock                                    (UTCTime, getCurrentTime)
import           Data.Time.Format                                   (formatTime)
import           Data.Time.Locale.Compat                            (defaultTimeLocale)
import qualified Data.Vector                                        as Vec

-- hackage
import           Distribution.Server.Prelude

import           Distribution.Server.Features.Core.Backup
import           Distribution.Server.Features.Core.State
import           Distribution.Server.Features.Security.Migration
import           Distribution.Server.Features.Users
import           Distribution.Server.Framework
import qualified Distribution.Server.Framework.BlobStorage          as BlobStorage
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource
import           Distribution.Server.Packages.Index                 (TarIndexEntry (..))
import qualified Distribution.Server.Packages.Index                 as Packages.Index
import           Distribution.Server.Packages.PackageIndex          (PackageIndex)
import qualified Distribution.Server.Packages.PackageIndex          as PackageIndex
import           Distribution.Server.Packages.Types
import           Distribution.Server.Users.Types                    (UserId,
                                                                     userName)
import           Distribution.Server.Users.Users                    (lookupUserId,
                                                                     userIdToName)

-- Cabal
import           Distribution.Package
import           Distribution.Text                                  (display)
import           Distribution.Version                               (nullVersion)

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
    queryGetPackageIndex :: forall m. MonadIO m => m (PackageIndex PkgInfo),

    -- | Retrieve the raw tarball info
    queryGetIndexTarballInfo :: forall m. MonadIO m => m IndexTarballInfo,

    -- Update transactions
    -- | Adds a version of a package which did not previously exist in the
    -- index. This requires a Cabal file and context, and optionally a
    -- reference to a tarball blob, and does not do any consistency checking
    -- of these.
    --
    -- If a package was able to be newly added, runs a `PackageChangeAdd` hook
    -- when done and returns True.
    updateAddPackage         :: forall m. MonadIO m => PackageId ->
                                CabalFileText -> UploadInfo ->
                                Maybe PkgTarball -> m Bool,
    -- | Deletes a version of an existing package, deleting the package if it
    -- was the last version.
    --
    -- If a package was found and deleted, runs a `PackageChangeDelete` hook
    -- when done and returns True.
    updateDeletePackage      :: forall m. MonadIO m => PackageId -> m Bool,

    -- | Adds a new Cabal file for this package version, creating it if
    -- necessary. Previous Cabal files are kept around.
    --
    -- Runs either a `PackageChangeAdd` or `PackageChangeInfo` hook, depending
    -- on whether a package with the given version already existed.
    updateAddPackageRevision :: forall m. MonadIO m => PackageId ->
                                CabalFileText -> UploadInfo -> m (),
    -- | Sets the source tarball for an existing package version. References to
    -- previous tarballs, if any, are kept around.
    --
    -- If this package was found, runs a `PackageChangeInfo` hook when done and
    -- returns True.
    updateAddPackageTarball  :: forall m. MonadIO m => PackageId ->
                                PkgTarball -> UploadInfo -> m Bool,
    -- | Sets the uploader of an existing package version.
    --
    -- If this package was found, runs a `PackageChangeInfo` hook when done and
    -- returns True.
    updateSetPackageUploader :: forall m. MonadIO m => PackageId -> UserId -> m Bool,
    -- | Sets the upload time of an existing package version.
    --
    -- If this package was found, runs a `PackageChangeInfo` hook when done and
    -- returns True.
    updateSetPackageUploadTime :: forall m. MonadIO m => PackageId -> UTCTime -> m Bool,

    -- | Set an entry in the 00-index.tar file.
    --
    -- The 00-index.tar file contains all the package entries, but it is an
    -- extensible format and we can add more stuff. E.g. version preferences
    -- or crypto signatures. This requires a file name, file contents, and
    -- modification time for the tar entry.
    --
    -- This runs a `PackageChangeIndexExtra` hook when done.
    updateArchiveIndexEntry :: forall m. MonadIO m => FilePath -> ByteString -> UTCTime -> m (),

    -- | Notification of package or index changes.
    packageChangeHook :: Hook PackageChange (),

    -- | Additional entries to be added before the index is updated/prodded
    --
    -- NOTE: Unlike a call to 'updateArchiveIndexEntry', this does NOT call
    -- any additional hooks.
    preIndexUpdateHook :: Hook PackageChange [TarIndexEntry],

    -- | Notification of tarball downloads.
    packageDownloadHook :: Hook PackageId (),

    -- | Notification that the index was updated
    -- The hook will be called when the index cache was actually updated.
    indexUpdatedHook :: Hook IndexTarballInfo ()
}

instance IsHackageFeature CoreFeature where
    getFeatureInterface = coreFeatureInterface

-- | How was a package updated?
data PackageUpdate
    -- | Cabal file was updated
    = PackageUpdatedCabalFile
    -- | A new tarball was uploaded
    | PackageUpdatedTarball
    -- | Package uploader was modified
    | PackageUpdatedUploader
    -- | Package upload time was modified
    | PackageUpdatedUploadTime

-- | This is designed so that you can pattern match on just the kinds of
-- events you are interested in.
data PackageChange
    -- | A package was newly added with this `PkgInfo`.
    = PackageChangeAdd    PkgInfo
    -- | A package was deleted, and this `PkgInfo` is no longer accessible in
    -- the package index.
    | PackageChangeDelete PkgInfo
    -- | A package was updated from the first `PkgInfo` to the second.
    | PackageChangeInfo PackageUpdate PkgInfo PkgInfo
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
isPackageChangeAny (PackageChangeInfo   _ _ pkginfo) = Just (packageId pkginfo, Just pkginfo)
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
isPackageIndexChange ::  PackageChange -> Maybe PackageChange
isPackageIndexChange = Just

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
    packageInPath :: forall m a. (MonadPlus m, FromReqURI a) => DynamicPath -> m a,

    -- | Find a tarball's PackageId from inside a path, doing some checking
    -- for consistency between the package and tarball.
    --
    -- TODO: This is a rather ad-hoc function. Do we really need it?
    packageTarballInPath :: forall m. MonadPlus m => DynamicPath -> m PackageId,

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

initCoreFeature :: ServerEnv -> IO (UserFeature -> IO CoreFeature)
initCoreFeature env@ServerEnv{serverStateDir, serverCacheDelay,
                              serverVerbosity = verbosity} = do
    -- Canonical state
    packagesState <- packagesStateComponent verbosity False serverStateDir

    -- Hooks
    packageChangeHook   <- newHook
    preIndexUpdateHook  <- newHook
    packageDownloadHook <- newHook

    return $ \users -> do

      -- One-off complex migration
      --
      -- As part of the support for TUF we made two changes to the state:
      --
      -- * We made the index append-only, and added a package update log
      --   to support this.
      -- * We changed the PkgTarball data structure to contain BlobInfo rather
      --   rather than BlobId; that is, we additionally record the length and
      --   SHA256 hash for all blobs.
      --
      -- Additionally, we now need `package.json` files for all versions of all
      -- packages. For new packages we add these when the package is uploaded,
      -- but for previously uploaded packages we need to add them.
      --
      -- Migrating the package tarball info and introducing metadata for
      -- pre-existing packages requires a full search through the package DB.
      -- Fortunately, since all these changes were introduced at the same time,
      -- we can use the check for the existence of the update log to see if we
      -- need any other kind of migration.

      migrateUpdateLog <- (isLeft . packageUpdateLog) <$>
                             queryState packagesState GetPackagesState
      when migrateUpdateLog $ do
        -- Migrate PackagesState (introduce package update log)
        logTiming verbosity "migrating package update log" $ do
          userdb <- queryGetUserDb users
          updateState packagesState (MigrateAddUpdateLog userdb)

        -- Migrate PkgTarball
        logTiming verbosity "migrating PkgTarball" $
          migratePkgTarball_v1_to_v2 env packagesState

        -- Create a checkpoint
        --
        -- Creating a checkpoint after the migration is important for two
        -- reasons: one, the migration is expensive and we don't want to  repeat
        -- it. But there is a second, more important reason. Until we have
        -- migrated we do not have a package log. This means that if we have a
        -- DB with no checkpoints at all but some old style (pre introduction of
        -- the package log) transactions as well as some new style transactions
        -- (post introduction of the package log), those new transactions will
        -- not be updating the package log. Since migration does not happen
        -- until _after_ replaying all those transactions, this means we would
        -- reconstruct the package log rather than use the package log as it was
        -- constructed in the first place, and we might potentially lose
        -- information.
        createCheckpoint (stateHandle packagesState)

      rec let (feature, getIndexTarball)
                = coreFeature env users
                              packagesState indexTar
                              packageChangeHook
                              preIndexUpdateHook
                              packageDownloadHook

          -- Caches
          -- The index.tar.gz file
          indexTar <- newAsyncCacheNF getIndexTarball
                        defaultAsyncCachePolicy {
                          asyncCacheName = "index tarball",
                          asyncCacheUpdateDelay  = serverCacheDelay,
                          asyncCacheSyncInit     = False,
                          asyncCacheLogVerbosity = verbosity
                        }

      registerHookJust packageChangeHook isPackageIndexChange $ \packageChange -> do
        -- NOTE: Adding a package adds the additional entries _atomically_ with a package
        -- This makes sure we never get a successful upload with no attendant package.json file.
        -- In all other cases, entries are allowed to be added nonatomically with the main index change.
        -- We may wish to refactor in the future, but as of this comment, the preIndexUpdateHook is effectively a
        -- no-op in all other significant cases.
        case packageChange of
             PackageChangeAdd _ -> return ()
             _ -> do
                     additionalEntries <- concat <$> runHook preIndexUpdateHook packageChange
                     forM_ additionalEntries $ updateState packagesState . AddOtherIndexEntry
        prodAsyncCache indexTar "package change"

      return feature

packagesStateComponent :: Verbosity -> Bool -> FilePath -> IO (StateComponent AcidState PackagesState)
packagesStateComponent verbosity freshDB stateDir = do
  let stateFile = stateDir </> "db" </> "PackagesState"
  st <- logTiming verbosity "Loaded PackagesState" $
          openLocalStateFrom stateFile (initialPackagesState freshDB)
  return StateComponent {
       stateDesc    = "Main package database"
     , stateHandle  = st
     , getState     = query st GetPackagesState
     , putState     = update st . ReplacePackagesState
     , backupState  = \_ -> indexToAllVersions
     , restoreState = packagesBackup
     , resetState   = packagesStateComponent verbosity True
     }

coreFeature :: ServerEnv
            -> UserFeature
            -> StateComponent AcidState PackagesState
            -> AsyncCache IndexTarballInfo
            -> Hook PackageChange ()
            -> Hook PackageChange [TarIndexEntry]
            -> Hook PackageId ()
            -> ( CoreFeature
               , IO IndexTarballInfo )

coreFeature ServerEnv{serverBlobStore = store} UserFeature{..}
            packagesState cacheIndexTarball
            packageChangeHook
            preIndexUpdateHook
            packageDownloadHook
  = (CoreFeature{..}, getIndexTarball)
  where
    coreFeatureInterface = (emptyHackageFeature "core") {
        featureDesc = "Core functionality"
      , featureResources = [
            coreLegacyIndexTarballTarGz
          , coreIncremIndexTarballTarGz
          , coreIncremIndexTarballTar
          , corePackagesPage
          , corePackagePage
          , corePackageRedirect
          , corePackageTarball
          , coreCabalFile
          , coreCabalFileRevs
          , coreCabalFileRev
          , coreUserDeauth
          , coreAdminDeauth
          , corePackUserDeauth
          ]
      , featureState    = [abstractAcidStateComponent packagesState]
      , featureCaches   = [
            CacheComponent {
              cacheDesc       = "main package index tarball",
              getCacheMemSize = memSize <$> readAsyncCache cacheIndexTarball
            }
          ]
      , featurePostInit = syncAsyncCache cacheIndexTarball
      }

    -- the rudimentary HTML resources are for when we don't want an additional HTML feature
    coreResource = CoreResource {..}
    coreLegacyIndexTarballTarGz = (resourceAt "/packages/index.tar.gz") {
        resourceDesc = [(GET, "tarball of package descriptions (legacy format, not incremental)")]
      , resourceGet  = [("tarball", serveLegacyPackagesIndexTarGz)]
      }
    coreIncremIndexTarballTarGz = (resourceAt "/01-index.tar.gz") {
        resourceDesc = [(GET, "tarball of package descriptions (incremental, compressed)")]
      , resourceGet  = [("tarball", serveIncremPackagesIndexTarGz)]
      }
    coreIncremIndexTarballTar = (resourceAt "/01-index.tar") {
        resourceDesc = [(GET, "tarball of package descriptions (incremental, uncompressed)")]
      , resourceGet  = [("tarball", serveIncremPackagesIndexTar)]
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
    coreCabalFileRevs = (resourceAt "/package/:package/revisions/.:format") {
        resourceDesc = [(GET, "List all package .cabal file revisions")]
      , resourceGet  = [("json", serveCabalFileRevisionsList)]
      }
    coreCabalFileRev = (resourceAt "/package/:package/revision/:revision.:format") {
        resourceDesc = [(GET, "Get package .cabal file revision")]
      , resourceGet  = [("cabal", serveCabalFileRevision)]
      }

    coreUserDeauth = (resourceAt "/packages/deauth") {
        resourceDesc = [(GET,  "Deauth Package user")]
      , resourceGet  = [("", deauth)]
      }
    coreAdminDeauth = (resourceAt "/admin/deauth") {
        resourceDesc = [(GET,  "Deauth Admin")]
      , resourceGet  = [("", deauth)]
      }
    corePackUserDeauth = (resourceAt "/user/:user/deauth") {
        resourceDesc = [(GET,  "Deauth User")]
      , resourceGet  = [("", deauth)]
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
          guard $ name == name' && version' /= nullVersion && (version == version' || version == nullVersion)
          return pkgid

    guardValidPackageId pkgid = do
      guard (pkgVersion pkgid /= nullVersion)
      void $ lookupPackageId pkgid

    guardValidPackageName pkgname =
      void $ lookupPackageName pkgname

    -- Queries
    --
    queryGetPackageIndex :: MonadIO m => m (PackageIndex PkgInfo)
    queryGetPackageIndex = return . packageIndex =<< queryState packagesState GetPackagesState

    queryGetIndexTarballInfo :: MonadIO m => m IndexTarballInfo
    queryGetIndexTarballInfo = readAsyncCache cacheIndexTarball

    -- Hooks
    indexUpdatedHook :: Hook IndexTarballInfo ()
    indexUpdatedHook = asyncCacheUpdatedHook cacheIndexTarball

    -- Update transactions
    --
    updateAddPackage :: MonadIO m => PackageId
                     -> CabalFileText -> UploadInfo
                     -> Maybe PkgTarball -> m Bool
    updateAddPackage pkgid cabalFile uploadinfo@(_, uid) mtarball = logTiming maxBound ("updateAddPackage " ++ display pkgid) $ do
      usersdb <- queryGetUserDb
      let Just userInfo = lookupUserId uid usersdb

      let pkginfo = mkPackageInfo pkgid cabalFile uploadinfo mtarball
      additionalEntries <- concat `liftM` runHook preIndexUpdateHook  (PackageChangeAdd pkginfo)

      successFlag <- updateState packagesState $
        AddPackage3
          pkginfo
          uploadinfo
          (userName userInfo)
          additionalEntries

      loginfo maxBound ("updateState(AddPackage3," ++ display pkgid ++ ") -> " ++ show successFlag)
      if successFlag
        then runHook_ packageChangeHook (PackageChangeAdd pkginfo)
        else return ()
      return successFlag

    updateDeletePackage :: MonadIO m => PackageId -> m Bool
    updateDeletePackage pkgid = logTiming maxBound ("updateDeletePackage " ++ display pkgid) $ do
      mpkginfo <- updateState packagesState (DeletePackage pkgid)
      case mpkginfo of
        Nothing -> return False
        Just pkginfo -> do
          runHook_ packageChangeHook (PackageChangeDelete pkginfo)
          return True

    updateAddPackageRevision :: MonadIO m => PackageId -> CabalFileText -> UploadInfo -> m ()
    updateAddPackageRevision pkgid cabalfile uploadinfo@(_, uid) = logTiming maxBound ("updateAddPackageRevision " ++ display pkgid) $ do
      usersdb <- queryGetUserDb
      let Just userInfo = lookupUserId uid usersdb
      (moldpkginfo, newpkginfo) <- updateState packagesState $
        AddPackageRevision2
          pkgid
          cabalfile
          uploadinfo
          (userName userInfo)
      loginfo maxBound ("updateState(AddPackageRevision2," ++ display pkgid ++ ") -> " ++ maybe "Nothing" (const "Just _") moldpkginfo)
      case moldpkginfo of
        Nothing ->
          runHook_ packageChangeHook  (PackageChangeAdd newpkginfo)
        Just oldpkginfo ->
          runHook_ packageChangeHook  (PackageChangeInfo PackageUpdatedCabalFile oldpkginfo newpkginfo)

    updateAddPackageTarball :: MonadIO m => PackageId -> PkgTarball -> UploadInfo -> m Bool
    updateAddPackageTarball pkgid tarball uploadinfo = logTiming maxBound ("updateAddPackageTarball " ++ display pkgid) $ do
      mpkginfo <- updateState packagesState (AddPackageTarball pkgid tarball uploadinfo)

      case mpkginfo of
        Nothing -> return False
        Just (oldpkginfo, newpkginfo) -> do
          runHook_ packageChangeHook  (PackageChangeInfo PackageUpdatedTarball oldpkginfo newpkginfo)
          return True

    updateSetPackageUploader pkgid userid = do
      mpkginfo <- updateState packagesState (SetPackageUploader pkgid userid)
      case mpkginfo of
        Nothing -> return False
        Just (oldpkginfo, newpkginfo) -> do
          runHook_ packageChangeHook  (PackageChangeInfo PackageUpdatedUploader oldpkginfo newpkginfo)
          return True

    updateSetPackageUploadTime pkgid time = do
      mpkginfo <- updateState packagesState (SetPackageUploadTime pkgid time)
      case mpkginfo of
        Nothing -> return False
        Just (oldpkginfo, newpkginfo) -> do
          runHook_ packageChangeHook  (PackageChangeInfo PackageUpdatedUploadTime oldpkginfo newpkginfo)
          return True

    updateArchiveIndexEntry :: MonadIO m => FilePath -> ByteString -> UTCTime -> m ()
    updateArchiveIndexEntry entryName entryData entryTime = logTiming maxBound ("updateArchiveIndexEntry " ++ show entryName) $ do
      updateState packagesState $
        AddOtherIndexEntry $ ExtraEntry entryName entryData entryTime
      runHook_ packageChangeHook (PackageChangeIndexExtra entryName entryData entryTime)

    -- Cache updates
    --
    getIndexTarball :: IO IndexTarballInfo
    getIndexTarball = do
      users <- queryGetUserDb  -- note, changes here don't automatically propagate
      time  <- getCurrentTime
      PackagesState index (Right updateSeq) <- queryState packagesState GetPackagesState
      let updateLog     = Foldable.toList updateSeq
          legacyTarball = Packages.Index.writeLegacy
                            users
                            (Packages.Index.legacyExtras updateLog)
                            index
          incremTarball = Packages.Index.writeIncremental
                            index
                            updateLog
          -- We use the standard compression for the legacy tarball so that the
          -- legacy tarball is identical to what we served before the intro of
          -- the incremental tarball. For the incremental tarball however we
          -- use maximum compression; this is more important here because the
          -- ordering of the incremental tarball is less compression friendly.
          legacyTarballGz = GZip.compress
                              legacyTarball
          incremTarballGz = GZip.compressWith
                              GZip.defaultCompressParams {
                                  GZip.compressLevel = GZip.bestCompression
                                }
                              incremTarball
          -- lazy construction, since it's forced by the async cache
          resourceInfo = IndexTarballInfo {
              indexTarballLegacyGz = mkTarballCompressed   time legacyTarballGz
            , indexTarballIncremUn = mkTarballUncompressed time incremTarball
            , indexTarballIncremGz = mkTarballCompressed   time incremTarballGz
            }
      return resourceInfo

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
    lookupPackageId (PackageIdentifier name v) | nullVersion == v = do
      pkgs <- lookupPackageName name
      -- pkgs is sorted by version number and non-empty
      return (last pkgs)
    lookupPackageId pkgid = do
      pkgsIndex <- queryGetPackageIndex
      case PackageIndex.lookupPackageId pkgsIndex pkgid of
        Just pkg -> return pkg
        _ -> packageError [MText $ "No such package version for " ++ display (packageName pkgid)]

    ------------------------------------------------------------------------

    serveLegacyPackagesIndexTarGz :: DynamicPath -> ServerPartE Response
    serveLegacyPackagesIndexTarGz _ = do
      tarball <- indexTarballLegacyGz <$> readAsyncCache cacheIndexTarball
      let tarballmd5 = show $ tarGzHashMD5 tarball
      cacheControl [Public, NoTransform, maxAgeMinutes 5] (ETag tarballmd5)
      enableRange
      return $ toResponse tarball

    serveIncremPackagesIndexTarGz :: DynamicPath -> ServerPartE Response
    serveIncremPackagesIndexTarGz _ = do
      tarball <- indexTarballIncremGz <$> readAsyncCache cacheIndexTarball
      let tarballmd5 = show $ tarGzHashMD5 tarball
      cacheControl [Public, NoTransform, maxAgeMinutes 5] (ETag tarballmd5)
      enableRange
      return $ toResponse tarball

    serveIncremPackagesIndexTar :: DynamicPath -> ServerPartE Response
    serveIncremPackagesIndexTar _ = do
      tarball <- indexTarballIncremUn <$> readAsyncCache cacheIndexTarball
      let tarballmd5 = show $ tarHashMD5 tarball
      cacheControl [Public, NoTransform, maxAgeMinutes 5] (ETag tarballmd5)
      enableRange
      return $ toResponse tarball

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
      return . toResponse $ Array (Vec.fromList json)

    -- result: tarball or not-found error
    -- note: this has a redirect gimmick so that we can cache the real
    -- tarball in the CDN and also hit the redirect to trigger the download hook
    servePackageTarball :: DynamicPath -> ServerPartE Response
    servePackageTarball dpath = do
      pkgid <- packageTarballInPath dpath
      guard (pkgVersion pkgid /= nullVersion)
      pkg <- lookupPackageId pkgid
      rq <- askRq
      case pkgLatestTarball pkg of
         Nothing -> errNotFound "Tarball not found"
                    [MText "No tarball exists for this package version."]
         Just (tarball, (uploadtime, _uid), _revNo) ->
           if not (isJust . lookup "real" . rqInputsQuery $ rq)
             then do
               runHook_ packageDownloadHook pkgid
               seeOther (rqUri rq ++ "?real=true")  $ toResponse ()
             else do
               let blobId = blobInfoId $ pkgTarballGz tarball
               cacheControl [Public, NoTransform, maxAgeDays 30]
                            (BlobStorage.blobETag blobId)
               file <- liftIO $ BlobStorage.fetch store blobId
               return $ toResponse $ Resource.PackageTarball file blobId uploadtime

    -- result: cabal file or not-found error
    serveCabalFile :: DynamicPath -> ServerPartE Response
    serveCabalFile dpath = do
      pkginfo <- packageInPath dpath >>= lookupPackageId
      -- check that the cabal name matches the package
      guard (lookup "cabal" dpath == Just (display $ packageName pkginfo))
      let (fileRev, (utime, _uid)) = pkgLatestRevision pkginfo
          cabalfile = Resource.CabalFile (cabalFileByteString fileRev) utime
      return $ toResponse cabalfile

    serveCabalFileRevisionsList :: DynamicPath -> ServerPartE Response
    serveCabalFileRevisionsList dpath = do
      pkginfo <- packageInPath dpath >>= lookupPackageId
      users   <- queryGetUserDb
      let revisions = pkgMetadataRevisions pkginfo
          revisionToObj rev (_, (utime, uid)) =
            let uname = userIdToName users uid in
            Object $ HashMap.fromList
              [ (Text.pack "number", Number (fromIntegral rev))
              , (Text.pack "user", String (Text.pack (display uname)))
              , (Text.pack "time", String (Text.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" utime)))
              ]
          revisionsJson = Array $ Vec.imap revisionToObj revisions
      return (toResponse revisionsJson)

    serveCabalFileRevision :: DynamicPath -> ServerPartE Response
    serveCabalFileRevision dpath = do
      pkginfo <- packageInPath dpath >>= lookupPackageId
      let mrev      = lookup "revision" dpath >>= fromReqURI
          revisions = pkgMetadataRevisions pkginfo
      case mrev >>= \rev -> revisions Vec.!? rev of
        Just (fileRev, (utime, _uid)) -> return $ toResponse cabalfile
          where
            cabalfile = Resource.CabalFile (cabalFileByteString fileRev) utime
        Nothing -> errNotFound "Package revision not found"
                     [MText "Cannot parse revision, or revision out of range."]
    
    
    deauth :: DynamicPath -> ServerPartE Response
    deauth _ = do
      return $ (toResponse ("<script>window.location='/'</script>"::String)) { 
          rsCode = 401 
        , rsHeaders   = mkHeaders ([("Content-Type",  "text/html")])
      }

packageExists, packageIdExists :: (Package pkg, Package pkg') => PackageIndex pkg -> pkg' -> Bool
-- | Whether a package exists in the given package index.
packageExists   pkgs pkg = not . null $ PackageIndex.lookupPackageName pkgs (packageName pkg)
-- | Whether a particular package version exists in the given package index.
packageIdExists pkgs pkg = maybe False (const True) $ PackageIndex.lookupPackageId pkgs (packageId pkg)
