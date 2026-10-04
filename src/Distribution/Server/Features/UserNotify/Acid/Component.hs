module Distribution.Server.Features.UserNotify.Acid.Component
  ( notifyStateComponent
  , acidStore
  ) where

import qualified Distribution.Server.Features.UserNotify.Acid as Acid
import Distribution.Server.Features.UserNotify.Backup
import qualified Distribution.Server.Features.UserNotify.Store as Store
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump

acidStore :: FilePath -> IO Store.Backend
acidStore stateDir = do
  notifyState <- notifyStateComponent stateDir
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.lookupNotifyPref = \uid -> queryState notifyState (Acid.LookupNotifyPref uid)
        , Store.addNotifyPref = \uid pref -> updateState notifyState (Acid.AddNotifyPref uid pref)
        , Store.getNotificationData = Acid.unNotifyData <$> queryState notifyState Acid.GetNotifyData
        , Store.setNotifyTime = \time -> updateState notifyState (Acid.SetNotifyTime time)
        }
    , Store.backendState = [abstractAcidStateComponent notifyState]
    }

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
