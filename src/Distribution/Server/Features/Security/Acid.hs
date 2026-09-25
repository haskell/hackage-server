module Distribution.Server.Features.Security.Acid
  ( securityStateComponent
  ) where

import qualified Distribution.Server.Features.Security.State as State
import Distribution.Server.Features.Security.Backup (securityBackup, securityRestore)
import Distribution.Server.Framework

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
