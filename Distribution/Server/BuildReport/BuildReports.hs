{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Distribution.Server.BuildReport.BuildReports (
    BuildReport(..),
    BuildReports(..),
    BuildReportId(..),
    PkgBuildReports(..),
    BuildLog(..),
    emptyReports,
    emptyPkgReports,
    addReport,
    deleteReport,
    setBuildLog,
    lookupReport,
    lookupPackageReports,
  ) where

import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import qualified Distribution.Server.BuildReport.BuildReport as BuildReport
import Distribution.Server.BuildReport.BuildReport (BuildReport)

import Distribution.Package (PackageId)
import Distribution.Text (Text(..))

import Happstack.Data.Serialize
import Distribution.Server.Instances ()

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>))

import qualified Distribution.Server.Util.Parse as Parse
import qualified Text.PrettyPrint          as Disp

newtype BuildReportId = BuildReportId Int
  deriving (Eq, Ord, Binary, Typeable, Show)

incrementReportId :: BuildReportId -> BuildReportId
incrementReportId (BuildReportId n) = BuildReportId (n+1)

instance Text BuildReportId where
  disp (BuildReportId n) = Disp.int n
  parse = BuildReportId <$> Parse.int

newtype BuildLog = BuildLog BlobStorage.BlobId
  deriving (Eq, Binary, Typeable, Show)

data PkgBuildReports = PkgBuildReports {
    -- for each report, other useful information: Maybe UserId, UTCTime
    -- perhaps deserving its own data structure (SubmittedReport?)
    reports      :: !(Map BuildReportId (BuildReport, Maybe BuildLog)),
    nextReportId :: !BuildReportId
} deriving (Typeable, Show)

data BuildReports = BuildReports {
    reportsIndex :: !(Map.Map PackageId PkgBuildReports)
} deriving (Typeable, Show)

emptyPkgReports :: PkgBuildReports
emptyPkgReports = PkgBuildReports {
    reports = Map.empty,
    nextReportId = BuildReportId 0
}

emptyReports :: BuildReports
emptyReports = BuildReports {
    reportsIndex = Map.empty
}

lookupReport :: BuildReports -> PackageId -> BuildReportId -> Maybe (BuildReport, Maybe BuildLog)
lookupReport buildReports pkgid reportId = Map.lookup reportId . reports =<< Map.lookup pkgid (reportsIndex buildReports)

lookupPackageReports :: BuildReports -> PackageId -> [(BuildReportId, (BuildReport, Maybe BuildLog))]
lookupPackageReports buildReports pkgid = case Map.lookup pkgid (reportsIndex buildReports) of
    Nothing -> []
    Just rs -> Map.toList (reports rs)

-------------------------

addReport :: BuildReports -> PackageId -> (BuildReport, Maybe BuildLog) -> (BuildReports, BuildReportId)
addReport buildReports pkgid report = 
    let pkgReports  = Map.findWithDefault emptyPkgReports pkgid (reportsIndex buildReports)
        reportId    = nextReportId pkgReports
        pkgReports' = PkgBuildReports { reports = Map.insert reportId report (reports pkgReports)
                                      , nextReportId = incrementReportId reportId }
    in (buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }, reportId)

deleteReport :: BuildReports -> PackageId -> BuildReportId -> Maybe BuildReports
deleteReport buildReports pkgid reportId = case Map.lookup pkgid (reportsIndex buildReports) of
    Nothing -> Nothing
    Just pkgReports -> case Map.lookup reportId (reports pkgReports) of
        Nothing -> Nothing
        Just {} -> let pkgReports' = pkgReports { reports = Map.delete reportId (reports pkgReports) }
                   in Just $ buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }

setBuildLog :: BuildReports -> PackageId -> BuildReportId -> Maybe BuildLog -> Maybe BuildReports
setBuildLog buildReports pkgid reportId buildLog = case Map.lookup pkgid (reportsIndex buildReports) of
    Nothing -> Nothing
    Just pkgReports -> case Map.lookup reportId (reports pkgReports) of
        Nothing -> Nothing
        Just (rlog, _) -> let pkgReports' = pkgReports { reports = Map.insert reportId (rlog, buildLog) (reports pkgReports) }
                         in Just $ buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }

-------------------
-- Binary instances
--

instance Binary BuildReport where
  put = Binary.put . BS.Char8.pack . BuildReport.show
  get = (BuildReport.read . BS.Char8.unpack) `fmap` Binary.get

instance Binary BuildReports where
  put (BuildReports index) = Binary.put index
  get = do
    rs <- Binary.get
    return BuildReports {
      reportsIndex = rs
    }

instance Binary PkgBuildReports where
    put (PkgBuildReports listing _) = Binary.put listing
    get = do
        listing <- Binary.get
        return PkgBuildReports {
            reports = listing,
            nextReportId = if Map.null listing
                              then BuildReportId 0
                              else incrementReportId (fst $ Map.findMax listing)
        } 

