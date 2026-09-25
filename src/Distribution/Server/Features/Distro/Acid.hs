module Distribution.Server.Features.Distro.Acid
  ( distrosStateComponent
  ) where

import qualified Distribution.Server.Features.Distro.State as State
import Distribution.Server.Features.Distro.Backup (dumpBackup, restoreBackup)
import Distribution.Server.Framework

distrosStateComponent :: FilePath -> IO (StateComponent AcidState State.Distros)
distrosStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Distros") State.initialDistros
  return StateComponent {
      stateDesc    = ""
    , stateHandle  = st
    , getState     = query st State.GetDistributions
    , putState     = \(State.Distros dists versions) -> update st (State.ReplaceDistributions dists versions)
    , backupState  = \_ -> dumpBackup
    , restoreState = restoreBackup
    , resetState   = distrosStateComponent
    }
