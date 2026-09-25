module Distribution.Server.Features.UserNotify.Acid.Component
  ( notifyStateComponent
  ) where

import qualified Distribution.Server.Features.UserNotify.Acid as Acid
import Distribution.Server.Features.UserNotify.Backup
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump

notifyStateComponent :: FilePath -> IO (StateComponent AcidState Acid.NotifyData)
notifyStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "UserNotify") =<< Acid.emptyNotifyData
  return StateComponent {
      stateDesc    = "State to keep track of revision notifications"
    , stateHandle  = st
    , getState     = query st Acid.GetNotifyData
    , putState     = update st . Acid.ReplaceNotifyData
    , backupState  = \backuptype tbl ->
        [csvToBackup ["notifydata.csv"] (notifyDataToCSV backuptype tbl)]
    , restoreState = userNotifyBackup
    , resetState   = notifyStateComponent
    }
