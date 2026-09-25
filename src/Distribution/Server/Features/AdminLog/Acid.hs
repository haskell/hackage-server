module Distribution.Server.Features.AdminLog.Acid
  ( adminLogStateComponent
  , module Distribution.Server.Features.AdminLog.State
  ) where

import Distribution.Server.Features.AdminLog.Backup
import Distribution.Server.Features.AdminLog.State
import qualified Distribution.Server.Features.AdminLog.State as State
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

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
