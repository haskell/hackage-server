module Distribution.Server.Features.PreferredVersions.Acid
  ( preferredStateComponent
  ) where

import qualified Distribution.Server.Features.PreferredVersions.State as State
import Distribution.Server.Features.PreferredVersions.Backup
import Distribution.Server.Framework

preferredStateComponent :: Bool -> FilePath -> IO (StateComponent AcidState State.PreferredVersions)
preferredStateComponent freshDB stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "PreferredVersions")
                           (State.initialPreferredVersions freshDB)
  return StateComponent {
      stateDesc    = "Preferred package versions"
    , stateHandle  = st
    , getState     = query st State.GetPreferredVersions
    , putState     = update st . State.ReplacePreferredVersions
    , resetState   = preferredStateComponent True
    , backupState  = \_ -> backupPreferredVersions
    , restoreState = restorePreferredVersions
    }
