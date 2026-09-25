module Distribution.Server.Features.Security.Acid
  ( securityStateComponent
  , acidStore
  ) where

import qualified Distribution.Server.Features.Security.State as State
import Distribution.Server.Features.Security.Backup (securityBackup, securityRestore)
import qualified Distribution.Server.Features.Security.Store as Store
import Distribution.Server.Framework

acidStore :: ServerEnv -> FilePath -> IO Store.Backend
acidStore env stateDir = do
  securityState <- securityStateComponent env stateDir
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getSecurityState = queryState securityState State.GetSecurityState
        , Store.getSecurityFiles = queryState securityState State.GetSecurityFiles
        , Store.setRootMirrorsAndKeys = \root mirrors snapshotKey timestampKey now ->
            updateState securityState (State.SetRootMirrorsAndKeys root mirrors snapshotKey timestampKey now)
        , Store.setTarGzFileInfo = \tarGzInfo tarInfo now ->
            updateState securityState (State.SetTarGzFileInfo tarGzInfo tarInfo now)
        , Store.resignSnapshotAndTimestamp = \maxAge now ->
            updateState securityState (State.ResignSnapshotAndTimestamp maxAge now)
        }
    , Store.backendState = [abstractAcidStateComponent securityState]
    }

securityStateComponent :: ServerEnv
                       -> FilePath
                       -> IO (StateComponent AcidState State.SecurityState)
securityStateComponent env stateDir = do
    let stateFile = stateDir </> "db" </> "TUF"
    st <- logTiming (serverVerbosity env) "Loaded SecurityState" $
            openLocalStateFrom stateFile State.initialSecurityState
    return StateComponent {
        stateDesc    = "TUF specific state"
      , stateHandle  = st
      , getState     = query st State.GetSecurityState
      , putState     = update st . State.ReplaceSecurityState
      , resetState   = securityStateComponent env
      , backupState  = \_ -> securityBackup
      , restoreState = securityRestore
      }
