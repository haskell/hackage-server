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
    unsafeSetReport
  ) where

import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import qualified Distribution.Server.BuildReport.BuildReport as BuildReport
import Distribution.Server.BuildReport.BuildReport (BuildReport)

import Distribution.Package (PackageId)
import Distribution.Text (Text(..))

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
    -- also, use IntMap (though with the public interface using BuildReportId)?
    reports      :: !(Map BuildReportId (BuildReport, Maybe BuildLog)),
    -- one more than the maximum report id used
    nextReportId :: !BuildReportId
} deriving (Typeable, Show)

data BuildReports = BuildReports {
    reportsIndex :: !(Map.Map PackageId PkgBuildReports)
} deriving (Typeable, Show)

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
-- PackageIds should /not/ have empty versions. Caller should ensure this somehow.
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

-- note: if the set of report ids is [1, 2, 3], then nextReportId = 4
-- after calling deleteReport for 3, the set is [1, 2] and nextReportId is still 4.
-- however, upon importing, nextReportId will = 3, one more than the maximum present
-- this is also a problem in ReportsBackup.hs. but it's not a major issue I think.
instance Binary PkgBuildReports where
    put (PkgBuildReports listing _) = Binary.put listing
    get = do
        listing <- Binary.get
        return PkgBuildReports {
            reports = listing,
            nextReportId = if Map.null listing
                              then BuildReportId 1
                              else incrementReportId (fst $ Map.findMax listing)
        } 

