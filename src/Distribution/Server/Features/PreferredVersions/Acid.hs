module Distribution.Server.Features.PreferredVersions.Acid
  ( preferredStateComponent
  , acidStore
  ) where

import qualified Distribution.Server.Features.PreferredVersions.State as State
import Distribution.Server.Features.PreferredVersions.Backup
import qualified Distribution.Server.Features.PreferredVersions.Store as Store
import Distribution.Server.Framework

acidStore :: FilePath -> IO Store.Backend
acidStore stateDir = do
  preferredState <- preferredStateComponent False stateDir
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.getPreferredInfo = \name -> queryState preferredState (State.GetPreferredInfo name)
        , Store.getDeprecatedFor = \name -> queryState preferredState (State.GetDeprecatedFor name)
        , Store.getPreferredVersions = queryState preferredState State.GetPreferredVersions
        , Store.setDeprecatedFor = \name deprs -> updateState preferredState (State.SetDeprecatedFor name deprs)
        , Store.setPreferredInfo = \name ranges versions -> updateState preferredState (State.SetPreferredInfo name ranges versions)
        , Store.setMigratedEphemeralPrefs = updateState preferredState State.SetMigratedEphemeralPrefs
        }
    , Store.backendState = [abstractAcidStateComponent preferredState]
    }

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
