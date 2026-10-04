module Distribution.Server.Features.AdminLog.Acid
  ( acidStore
  ) where

import Distribution.Server.Features.AdminLog.Backup
import qualified Distribution.Server.Features.AdminLog.State as State
import Distribution.Server.Features.AdminLog.Store
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

acidStore :: FilePath -> IO Backend
acidStore stateDir = do
  adminLogState <- adminLogStateComponent stateDir
  pure Backend {
      backendStore = Store {
          getAdminLog = State.adminLog <$> queryState adminLogState State.GetAdminLog
        , addAdminLog = \entry -> updateState adminLogState (State.AddAdminLog entry)
        }
    , backendState = [abstractAcidStateComponent adminLogState]
    }

adminLogStateComponent :: FilePath -> IO (StateComponent AcidState State.AdminLog)
adminLogStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "AdminLog") State.initialAdminLog
  return StateComponent {
      stateDesc    = "AdminLog"
    , stateHandle  = st
    , getState     = query st State.GetAdminLog
    , putState     = update st . State.ReplaceAdminLog
    , backupState  = \_ (State.AdminLog xs) ->
                      [BackupByteString ["adminLog.txt"] . backupLogEntries $ xs]
    , restoreState = restoreAdminLogBackup
    , resetState   = adminLogStateComponent
    }
