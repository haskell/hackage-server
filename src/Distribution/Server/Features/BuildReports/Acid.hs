module Distribution.Server.Features.BuildReports.Acid
  ( reportsStateComponent
  ) where

import qualified Distribution.Server.Features.BuildReports.State as State
import Distribution.Server.Features.BuildReports.Backup (dumpBackup, restoreBackup)
import Distribution.Server.Features.BuildReports.BuildReports (BuildReports)
import Distribution.Server.Framework

reportsStateComponent :: String -> FilePath -> IO (StateComponent AcidState BuildReports)
reportsStateComponent name stateDir = do
  st  <- openLocalStateFrom (stateDir </> "db" </> name) State.initialBuildReports
  return StateComponent {
      stateDesc    = "Build reports"
    , stateHandle  = st
    , getState     = query st State.GetBuildReports
    , putState     = update st . State.ReplaceBuildReports
    , backupState  = \_ -> dumpBackup
    , restoreState = restoreBackup
    , resetState   = reportsStateComponent name
    }
