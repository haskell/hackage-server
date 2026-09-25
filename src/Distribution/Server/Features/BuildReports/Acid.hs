module Distribution.Server.Features.BuildReports.Acid
  ( reportsStateComponent
  , acidStore
  ) where

import qualified Distribution.Server.Features.BuildReports.State as State
import Distribution.Server.Features.BuildReports.Backup (dumpBackup, restoreBackup)
import Distribution.Server.Features.BuildReports.BuildReports (BuildReports)
import qualified Distribution.Server.Features.BuildReports.Store as Store
import Distribution.Server.Framework

acidStore :: String -> FilePath -> IO Store.Backend
acidStore name stateDir = do
  reportsState <- reportsStateComponent name stateDir
  pure Store.Backend {
      Store.backendStore = Store.Store {
          Store.lookupReportCovg = \pkgid reportId -> queryState reportsState (State.LookupReportCovg pkgid reportId)
        , Store.lookupPackageReports = \pkgid -> do
            reports <- queryState reportsState (State.LookupPackageReports pkgid)
            pure $ map (\(reportId, (report, _, _, _)) -> (reportId, report)) reports
        , Store.lookupFailCount = \pkgid -> queryState reportsState (State.LookupFailCount pkgid)
        , Store.lookupLatestReport = \pkgid -> queryState reportsState (State.LookupLatestReport pkgid)
        , Store.lookupRunTests = \pkgid -> queryState reportsState (State.LookupRunTests pkgid)
        , Store.addReport = \pkgid report -> updateState reportsState (State.AddReport pkgid report)
        , Store.deleteReport = \pkgid reportId -> updateState reportsState (State.DeleteReport pkgid reportId)
        , Store.setBuildLog = \pkgid reportId buildLog -> updateState reportsState (State.SetBuildLog pkgid reportId buildLog)
        , Store.setTestLog = \pkgid reportId testLog -> updateState reportsState (State.SetTestLog pkgid reportId testLog)
        , Store.resetFailCount = \pkgid -> updateState reportsState (State.ResetFailCount pkgid)
        , Store.setRunTests = \pkgid enabled -> updateState reportsState (State.SetRunTests pkgid enabled)
        , Store.setFailStatus = \pkgid status -> updateState reportsState (State.SetFailStatus pkgid status)
        , Store.addRptAllLogsCovg = \pkgid report -> updateState reportsState (State.AddRptAllLogsCovg pkgid report)
        }
    , Store.backendState = [abstractAcidStateComponent reportsState]
    }

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
