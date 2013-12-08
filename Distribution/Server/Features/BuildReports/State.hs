{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Server.Features.BuildReports.State where

import Distribution.Server.Features.BuildReports.BuildReports (BuildReportId, BuildLog, BuildReport, BuildReports)
import qualified Distribution.Server.Features.BuildReports.BuildReports as BuildReports

import Distribution.Package

import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Acid     (Query, Update, makeAcidic)

initialBuildReports :: BuildReports
initialBuildReports = BuildReports.emptyReports

-- and defined methods
addReport :: PackageId -> (BuildReport, Maybe BuildLog) -> Update BuildReports BuildReportId
addReport pkgid report = do
    buildReports <- State.get
    let (reports, reportId) = BuildReports.addReport pkgid report buildReports
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

lookupReport :: PackageId -> BuildReportId -> Query BuildReports (Maybe (BuildReport, Maybe BuildLog))
lookupReport pkgid reportId = asks (BuildReports.lookupReport pkgid reportId)

lookupPackageReports :: PackageId -> Query BuildReports [(BuildReportId, (BuildReport, Maybe BuildLog))]
lookupPackageReports pkgid = asks (BuildReports.lookupPackageReports pkgid)

getBuildReports :: Query BuildReports BuildReports
getBuildReports = ask

replaceBuildReports :: BuildReports -> Update BuildReports ()
replaceBuildReports = State.put

makeAcidic ''BuildReports ['addReport
                          ,'setBuildLog
                          ,'deleteReport
                          ,'lookupReport
                          ,'lookupPackageReports
                          ,'getBuildReports
                          ,'replaceBuildReports
                          ]

