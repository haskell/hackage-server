module Distribution.Server.Features.Core.Acid
  ( acidStore
  , packagesStateComponent
  ) where

import Distribution.Server.Features.Core.Backup
import Distribution.Server.Features.Core.Store
import qualified Distribution.Server.Features.Core.State as Acid
import Distribution.Server.Features.Security.Migration
import Distribution.Server.Framework
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types (PkgInfo, pkgLatestUploadTime, pkgNumRevisions, pkgOriginalUploadTime)
import Distribution.Server.Users.Users (Users)

import Distribution.Package (packageId)

import Control.Monad (when)
import Data.Either (isLeft)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Ord (Down(Down))

acidStore :: ServerEnv -> Verbosity -> Bool -> FilePath -> IO Backend
acidStore env verbosity freshDB stateDir = do
  packagesState <- packagesStateComponent verbosity freshDB stateDir
  let withIndex :: MonadIO m => (PackageIndex.PackageIndex PkgInfo -> a) -> m a
      withIndex f = f . Acid.packageIndex <$> queryState packagesState Acid.GetPackagesState
  pure Backend {
      backendStore = Store {
          getPackagesState       = queryState packagesState Acid.GetPackagesState
        , lookupPackageName      = \pkgname -> withIndex (`PackageIndex.lookupPackageName` pkgname)
        , lookupPackageId        = \pkgid -> withIndex (`PackageIndex.lookupPackageId` pkgid)
        , lookupPackageNames     = \pkgnames -> withIndex $ \index ->
                                     Map.fromList [ (pkgname, pkgs)
                                                  | pkgname <- pkgnames
                                                  , let pkgs = PackageIndex.lookupPackageName index pkgname
                                                  , not (null pkgs) ]
        , latestPackages         = withIndex (map NE.last . PackageIndex.allPackagesByNameNE)
        , allPackageNames        = withIndex PackageIndex.allPackageNames
        , allPackageIds          = withIndex (map packageId . PackageIndex.allPackages)
        , countPackages          = withIndex PackageIndex.indexSize
        , latestUploadsBetween   = \earlier later ->
                                     let isBetween pkginfo =
                                           let t = pkgLatestUploadTime pkginfo
                                           in t > earlier && t <= later
                                     in withIndex (filter isBetween . PackageIndex.allPackages)
        , recentPackageVersions  = withIndex $ \index ->
                                     let versions = PackageIndex.allPackages index
                                     in RecentPackageVersions {
                                          recentPackageVersionsUploads =
                                            sortOn (Down . pkgOriginalUploadTime) versions
                                        , recentPackageVersionsRevisions =
                                            sortOn (Down . pkgLatestUploadTime)
                                              . filter ((> 1) . pkgNumRevisions)
                                              $ versions
                                        }
        , addPackage             = \pkginfo uploadinfo username entries ->
                                     updateState packagesState (Acid.AddPackage3 pkginfo uploadinfo username entries)
        , deletePackage          = \pkgid ->
                                     updateState packagesState (Acid.DeletePackage pkgid)
        , addPackageRevision     = \pkgid cabalfile uploadinfo username ->
                                     updateState packagesState (Acid.AddPackageRevision2 pkgid cabalfile uploadinfo username)
        , addPackageTarball      = \pkgid tarball uploadinfo ->
                                     updateState packagesState (Acid.AddPackageTarball pkgid tarball uploadinfo)
        , setPackageUploader     = \pkgid userid ->
                                     updateState packagesState (Acid.SetPackageUploader pkgid userid)
        , setPackageUploadTime   = \pkgid time ->
                                     updateState packagesState (Acid.SetPackageUploadTime pkgid time)
        , addOtherIndexEntry     = \entry ->
                                     updateState packagesState (Acid.AddOtherIndexEntry entry)
        }
    , backendState   = [abstractAcidStateComponent packagesState]
    , backendMigrate = migrate env verbosity packagesState
    }

packagesStateComponent :: Verbosity -> Bool -> FilePath -> IO (StateComponent AcidState Acid.PackagesState)
packagesStateComponent verbosity freshDB stateDir = do
  let stateFile = stateDir </> "db" </> "PackagesState"
  st <- logTiming verbosity "Loaded PackagesState" $
          openLocalStateFrom stateFile (Acid.initialPackagesState freshDB)
  return StateComponent {
       stateDesc    = "Main package database"
     , stateHandle  = st
     , getState     = query st Acid.GetPackagesState
     , putState     = update st . Acid.ReplacePackagesState
     , backupState  = \_ -> indexToAllVersions
     , restoreState = packagesBackup
     , resetState   = packagesStateComponent verbosity True
     }

-- | One-off complex migration
--
-- As part of the support for TUF we made two changes to the state:
--
-- \* We made the index append-only, and added a package update log
--   to support this.
-- \* We changed the PkgTarball data structure to contain BlobInfo rather
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
migrate :: ServerEnv -> Verbosity -> StateComponent AcidState Acid.PackagesState -> IO Users -> IO ()
migrate env verbosity packagesState getUserDb = do
    migrateUpdateLog <- (isLeft . Acid.packageUpdateLog) <$>
                           queryState packagesState Acid.GetPackagesState
    when migrateUpdateLog $ do
      -- Migrate Acid.PackagesState (introduce package update log)
      logTiming verbosity "migrating package update log" $ do
        userdb <- getUserDb
        updateState packagesState (Acid.MigrateAddUpdateLog userdb)

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
