module Distribution.Server.Features.Core.Acid
  ( acidStore
  , packagesStateComponent
  ) where

import Distribution.Server.Features.Core.Backup
import Distribution.Server.Features.Core.Store
import qualified Distribution.Server.Features.Core.State as Acid
import Distribution.Server.Features.Security.Migration
import Distribution.Server.Framework

acidStore :: ServerEnv -> Verbosity -> Bool -> FilePath -> IO Backend
acidStore env verbosity freshDB stateDir = do
  packagesState <- packagesStateComponent verbosity freshDB stateDir
  pure Backend {
      backendStore = Store {
          getPackagesState       = queryState packagesState Acid.GetPackagesState
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
        , migrateAddUpdateLog    = \userdb ->
                                     updateState packagesState (Acid.MigrateAddUpdateLog userdb)
        , migratePackageTarballs = migratePkgTarball_v1_to_v2 env packagesState
        , createStoreCheckpoint  = createCheckpoint (stateHandle packagesState)
        }
    , backendState = [abstractAcidStateComponent packagesState]
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
