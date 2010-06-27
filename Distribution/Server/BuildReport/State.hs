{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}
module Distribution.Server.BuildReport.State where

--import qualified Distribution.Server.BuildReport.BuildReport as BuildReport
import Distribution.Server.BuildReport.BuildReports (BuildReportId, BuildLog, BuildReport, BuildReports, PkgBuildReports)
import qualified Distribution.Server.BuildReport.BuildReports as BuildReports

import Distribution.Package
import Happstack.State
import Happstack.Data.Serialize

import qualified Data.Binary as Binary
import Control.Monad.Reader
import qualified Control.Monad.State as State

-- BuildReportId
instance Version BuildReportId where
    mode = Versioned 0 Nothing

instance Serialize BuildReportId where
    putCopy = contain . Binary.put
    getCopy = contain Binary.get

-- BuildLog
instance Version BuildLog where
    mode = Versioned 0 Nothing

instance Serialize BuildLog where
    putCopy = contain . Binary.put
    getCopy = contain Binary.get

-- BuildReport
instance Version BuildReport where
    mode = Versioned 0 Nothing

instance Serialize BuildReport where
    putCopy = contain . Binary.put
    getCopy = contain Binary.get

-- PkgBuildReports
instance Version PkgBuildReports where
    mode = Versioned 0 Nothing

instance Serialize PkgBuildReports where
    putCopy = contain . Binary.put
    getCopy = contain Binary.get

-- BuildReports
instance Version BuildReports where
  mode = Versioned 0 Nothing

instance Serialize BuildReports where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get

instance Component BuildReports where
  type Dependencies BuildReports = End
  initialValue = BuildReports.emptyReports

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

$(mkMethods ''BuildReports ['addReport
                           ,'setBuildLog
                           ,'deleteReport
                           ,'lookupReport
                           ,'lookupPackageReports
                           ,'getBuildReports
                           ,'replaceBuildReports
                           ])

