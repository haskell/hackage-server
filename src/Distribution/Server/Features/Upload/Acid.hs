module Distribution.Server.Features.Upload.Acid
  ( trusteesStateComponent
  , uploadersStateComponent
  , maintainersStateComponent
  , acidStore
  ) where

import qualified Distribution.Server.Features.Upload.Store as Store
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Features.Upload.Backup (maintToExport, maintainerBackup)
import qualified Distribution.Server.Features.Upload.State as Acid
import Distribution.Server.Users.Backup

acidStore :: FilePath -> IO Store.Backend
acidStore stateDir = do
  trusteesState <- trusteesStateComponent stateDir
  uploadersState <- uploadersStateComponent stateDir
  maintainersState <- maintainersStateComponent stateDir
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getTrustees = queryState trusteesState Acid.GetTrusteesList
        , Store.addTrustee = updateState trusteesState . Acid.AddHackageTrustee
        , Store.removeTrustee = updateState trusteesState . Acid.RemoveHackageTrustee
        , Store.getUploaders = queryState uploadersState Acid.GetUploadersList
        , Store.addUploader = updateState uploadersState . Acid.AddHackageUploader
        , Store.removeUploader = updateState uploadersState . Acid.RemoveHackageUploader
        , Store.getPackageMaintainers = \name -> queryState maintainersState (Acid.GetPackageMaintainers name)
        , Store.addPackageMaintainer = \name -> updateState maintainersState . Acid.AddPackageMaintainer name
        , Store.removePackageMaintainer = \name -> updateState maintainersState . Acid.RemovePackageMaintainer name
        }
    , Store.backendState = [ abstractAcidStateComponent trusteesState
                           , abstractAcidStateComponent uploadersState
                           , abstractAcidStateComponent maintainersState
                           ]
    }

trusteesStateComponent :: FilePath -> IO (StateComponent AcidState Acid.HackageTrustees)
trusteesStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "HackageTrustees") Acid.initialHackageTrustees
  return StateComponent {
      stateDesc    = "Trustees"
    , stateHandle  = st
    , getState     = query st Acid.GetHackageTrustees
    , putState     = update st . Acid.ReplaceHackageTrustees . Acid.trusteeList
    , backupState  = \_ (Acid.HackageTrustees trustees) -> [csvToBackup ["trustees.csv"] $ groupToCSV trustees]
    , restoreState = Acid.HackageTrustees <$> groupBackup ["trustees.csv"]
    , resetState   = trusteesStateComponent
    }

uploadersStateComponent :: FilePath -> IO (StateComponent AcidState Acid.HackageUploaders)
uploadersStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "HackageUploaders") Acid.initialHackageUploaders
  return StateComponent {
      stateDesc    = "Uploaders"
    , stateHandle  = st
    , getState     = query st Acid.GetHackageUploaders
    , putState     = update st . Acid.ReplaceHackageUploaders . Acid.uploaderList
    , backupState  = \_ (Acid.HackageUploaders uploaders) -> [csvToBackup ["uploaders.csv"] $ groupToCSV uploaders]
    , restoreState = Acid.HackageUploaders <$> groupBackup ["uploaders.csv"]
    , resetState   = uploadersStateComponent
    }

maintainersStateComponent :: FilePath -> IO (StateComponent AcidState Acid.PackageMaintainers)
maintainersStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "PackageMaintainers") Acid.initialPackageMaintainers
  return StateComponent {
      stateDesc    = "Package maintainers"
    , stateHandle  = st
    , getState     = query st Acid.AllPackageMaintainers
    , putState     = update st . Acid.ReplacePackageMaintainers
    , backupState  = \_ (Acid.PackageMaintainers mains) -> [maintToExport mains]
    , restoreState = maintainerBackup
    , resetState   = maintainersStateComponent
    }
