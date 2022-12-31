{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Server.Features.BuildReports.State where

import Distribution.Server.Features.BuildReports.BuildReports
                (BuildReportId, BuildLog, TestLog, BuildReport, BuildReports,BuildCovg, BuildStatus)
import qualified Distribution.Server.Features.BuildReports.BuildReports as BuildReports

import Distribution.Package

import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Acid     (Query, Update, makeAcidic)

initialBuildReports :: BuildReports
initialBuildReports = BuildReports.emptyReports

-- and defined methods
addReport :: PackageId -> (BuildReport, Maybe BuildLog) -> Update BuildReports BuildReportId
addReport pkgid (bRpt, blog) = do
    buildReports <- State.get
    let (reports, reportId) = BuildReports.addReport pkgid (bRpt, blog, Nothing) buildReports
    State.put reports
    return reportId

setBuildLog :: PackageId -> BuildReportId -> Maybe BuildLog -> Update BuildReports Bool
setBuildLog pkgid reportId buildLog = do
    buildReports <- State.get
    case BuildReports.setBuildLog pkgid reportId buildLog buildReports of
        Nothing -> return False
        Just reports -> State.put reports >> return True

deleteReport :: PackageId -> BuildReportId -> Update BuildReports Bool --Maybe BuildReports
deleteReport pkgid reportId = do
    buildReports <- State.get
    case BuildReports.deleteReport pkgid reportId buildReports of
        Nothing -> return False
        Just reports -> State.put reports >> return True

lookupReport :: PackageId -> BuildReportId -> Query BuildReports (Maybe (BuildReport, Maybe BuildLog, Maybe TestLog))
lookupReport pkgid reportId = asks (BuildReports.lookupReport pkgid reportId)

lookupPackageReports :: PackageId -> Query BuildReports [(BuildReportId, (BuildReport, Maybe BuildLog, Maybe TestLog))]
lookupPackageReports pkgid = asks (BuildReports.lookupPackageReports pkgid)

getBuildReports :: Query BuildReports BuildReports
getBuildReports = ask

replaceBuildReports :: BuildReports -> Update BuildReports ()
replaceBuildReports = State.put

addRptLogCovg :: PackageId -> (BuildReport, Maybe BuildLog, Maybe BuildCovg ) -> Update BuildReports BuildReportId
addRptLogCovg pkgid (bRpt, blog, bcovg) = do
    buildReports <- State.get
    let (reports, reportId) = BuildReports.addRptLogTestCovg pkgid (bRpt, blog, Nothing, bcovg) buildReports
    State.put reports
    return reportId

lookupReportCovg :: PackageId -> BuildReportId -> Query BuildReports (Maybe (BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg))
lookupReportCovg pkgid reportId = asks (BuildReports.lookupReportCovg pkgid reportId)

setFailStatus :: PackageId -> Bool -> Update BuildReports ()
setFailStatus pkgid status = do
    buildReports <- State.get
    let reports = BuildReports.setFailStatus pkgid status buildReports
    State.put reports

resetFailCount :: PackageId -> Update BuildReports Bool
resetFailCount pkgid = do
    buildReports <- State.get
    case BuildReports.resetFailCount pkgid buildReports of
        Nothing       -> return False
        Just reports  -> State.put reports >> return True

lookupFailCount :: PackageId -> Query BuildReports (Maybe BuildStatus)
lookupFailCount pkgid = asks (BuildReports.lookupFailCount pkgid)

lookupLatestReport :: PackageId -> Query BuildReports (Maybe (BuildReportId, BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg))
lookupLatestReport pkgid = asks (BuildReports.lookupLatestReport pkgid)

addRptLogTestCovg :: PackageId -> (BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg ) -> Update BuildReports BuildReportId
addRptLogTestCovg pkgid (bRpt, blog, btest, bcovg) = do
    buildReports <- State.get
    let (reports, reportId) = BuildReports.addRptLogTestCovg pkgid (bRpt, blog, btest, bcovg) buildReports
    State.put reports
    return reportId

setTestLog :: PackageId -> BuildReportId -> Maybe TestLog -> Update BuildReports Bool
setTestLog pkgid reportId testLog = do
    buildReports <- State.get
    case BuildReports.setTestLog pkgid reportId testLog buildReports of
        Nothing -> return False
        Just reports -> State.put reports >> return True

lookupRunTests :: PackageId -> Query BuildReports (Bool)
lookupRunTests pkgid = asks (BuildReports.lookupRunTests pkgid)

setRunTests :: PackageId -> Bool -> Update BuildReports Bool
setRunTests pkgid b = do
    buildReports <- State.get
    case BuildReports.setRunTests pkgid b buildReports of
        Nothing      -> pure False
        Just reports -> State.put reports >> pure True

makeAcidic ''BuildReports ['addReport
                          ,'setBuildLog
                          ,'deleteReport
                          ,'lookupReport
                          ,'lookupPackageReports
                          ,'getBuildReports
                          ,'replaceBuildReports
                          ,'addRptLogCovg
                          ,'lookupReportCovg
                          ,'setFailStatus
                          ,'resetFailCount
                          ,'lookupFailCount
                          ,'lookupLatestReport
                          ,'addRptLogTestCovg
                          ,'setTestLog
                          ,'lookupRunTests
                          ,'setRunTests
                          ]
