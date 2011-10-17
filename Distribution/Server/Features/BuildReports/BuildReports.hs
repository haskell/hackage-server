{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Distribution.Server.Features.BuildReports.BuildReports (
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
    unsafeSetReport
  ) where

import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import qualified Distribution.Server.Features.BuildReports.BuildReport as BuildReport
import Distribution.Server.Features.BuildReports.BuildReport (BuildReport)

import Distribution.Package (PackageId)
import Distribution.Text (Text(..))

import Distribution.Server.Framework.Instances ()

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>))

import qualified Distribution.Server.Util.Parse as Parse
import qualified Text.PrettyPrint          as Disp

newtype BuildReportId = BuildReportId Int
  deriving (Eq, Ord, Serialize, Typeable, Show)

incrementReportId :: BuildReportId -> BuildReportId
incrementReportId (BuildReportId n) = BuildReportId (n+1)

instance Text BuildReportId where
  disp (BuildReportId n) = Disp.int n
  parse = BuildReportId <$> Parse.int

newtype BuildLog = BuildLog BlobStorage.BlobId
  deriving (Eq, Serialize, Typeable, Show)

data PkgBuildReports = PkgBuildReports {
    -- for each report, other useful information: Maybe UserId, UTCTime
    -- perhaps deserving its own data structure (SubmittedReport?)
    -- When a report was submitted is very useful information.
    -- also, use IntMap instead of Map BuildReportId?
    reports      :: !(Map BuildReportId (BuildReport, Maybe BuildLog)),
    -- one more than the maximum report id used
    nextReportId :: !BuildReportId
} deriving (Eq, Typeable, Show)

data BuildReports = BuildReports {
    reportsIndex :: !(Map.Map PackageId PkgBuildReports)
} deriving (Eq, Typeable, Show)

emptyPkgReports :: PkgBuildReports
emptyPkgReports = PkgBuildReports {
    reports = Map.empty,
    nextReportId = BuildReportId 1
}

emptyReports :: BuildReports
emptyReports = BuildReports {
    reportsIndex = Map.empty
}

lookupReport :: PackageId -> BuildReportId -> BuildReports -> Maybe (BuildReport, Maybe BuildLog)
lookupReport pkgid reportId buildReports = Map.lookup reportId . reports =<< Map.lookup pkgid (reportsIndex buildReports)

lookupPackageReports :: PackageId -> BuildReports -> [(BuildReportId, (BuildReport, Maybe BuildLog))]
lookupPackageReports pkgid buildReports = case Map.lookup pkgid (reportsIndex buildReports) of
    Nothing -> []
    Just rs -> Map.toList (reports rs)

-------------------------
-- PackageIds should /not/ have empty Versions. Caller should ensure this.
addReport :: PackageId -> (BuildReport, Maybe BuildLog) -> BuildReports -> (BuildReports, BuildReportId)
addReport pkgid report buildReports = 
    let pkgReports  = Map.findWithDefault emptyPkgReports pkgid (reportsIndex buildReports)
        reportId    = nextReportId pkgReports
        pkgReports' = PkgBuildReports { reports = Map.insert reportId report (reports pkgReports)
                                      , nextReportId = incrementReportId reportId }
    in (buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }, reportId)

unsafeSetReport :: PackageId -> BuildReportId -> (BuildReport, Maybe BuildLog) -> BuildReports -> BuildReports
unsafeSetReport pkgid reportId report buildReports =
    let pkgReports  = Map.findWithDefault emptyPkgReports pkgid (reportsIndex buildReports)
        pkgReports' = PkgBuildReports { reports = Map.insert reportId report (reports pkgReports)
                                      , nextReportId = max (incrementReportId reportId) (nextReportId pkgReports) }
    in buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }

deleteReport :: PackageId -> BuildReportId -> BuildReports -> Maybe BuildReports
deleteReport pkgid reportId buildReports = case Map.lookup pkgid (reportsIndex buildReports) of
    Nothing -> Nothing
    Just pkgReports -> case Map.lookup reportId (reports pkgReports) of
        Nothing -> Nothing
        Just {} -> let pkgReports' = pkgReports { reports = Map.delete reportId (reports pkgReports) }
                   in Just $ buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }

setBuildLog :: PackageId -> BuildReportId -> Maybe BuildLog -> BuildReports -> Maybe BuildReports
setBuildLog pkgid reportId buildLog buildReports = case Map.lookup pkgid (reportsIndex buildReports) of
    Nothing -> Nothing
    Just pkgReports -> case Map.lookup reportId (reports pkgReports) of
        Nothing -> Nothing
        Just (rlog, _) -> let pkgReports' = pkgReports { reports = Map.insert reportId (rlog, buildLog) (reports pkgReports) }
                         in Just $ buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }

-------------------
-- Serialize instances
--

instance Serialize BuildReport where
  put = Serialize.put . BS.Char8.pack . BuildReport.show
  get = (BuildReport.read . BS.Char8.unpack) `fmap` Serialize.get

instance Serialize BuildReports where
  put (BuildReports index) = Serialize.put index
  get = do
    rs <- Serialize.get
    return BuildReports {
      reportsIndex = rs
    }

-- note: if the set of report ids is [1, 2, 3], then nextReportId = 4
-- after calling deleteReport for 3, the set is [1, 2] and nextReportId is still 4.
-- however, upon importing, nextReportId will = 3, one more than the maximum present
-- this is also a problem in ReportsBackup.hs. but it's not a major issue I think.
instance Serialize PkgBuildReports where
    put (PkgBuildReports listing _) = Serialize.put listing
    get = do
        listing <- Serialize.get
        return PkgBuildReports {
            reports = listing,
            nextReportId = if Map.null listing
                              then BuildReportId 1
                              else incrementReportId (fst $ Map.findMax listing)
        } 

