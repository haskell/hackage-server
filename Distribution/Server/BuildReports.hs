{-# LANGUAGE DeriveDataTypeable #-}
module Distribution.Server.BuildReports (
    BuildReports,
    BuildReportId,
    BuildLog(..),
    empty,
    addReport,
    addBuildLog,
    lookupReport,
    lookupBuildLog,
    lookupPackageReports,
  ) where

import Distribution.Server.Types
         ( UserName )
import qualified Distribution.Server.BlobStorage as BlobStorage
import qualified Distribution.Server.BuildReport as BuildReport
import Distribution.Server.BuildReport (BuildReport)

import Distribution.Package
         ( PackageIdentifier )

import qualified Data.Map as Map
--import qualified Data.Binary as Binary
--import Data.Binary (Binary)
import Data.Time.Clock (UTCTime)

newtype BuildReportId = BuildReportId Int
  deriving (Eq, Ord)

data BuildLog = BuildLog BlobStorage.BlobId UTCTime UserName

--instance Binary BuildLog where

data BuildReports = BuildReports {
    reports :: !(Map.Map BuildReportId BuildReport),
    logs    :: !(Map.Map BuildReportId BuildLog),
    index   :: !(Map.Map PackageIdentifier [(BuildReportId, BuildReport)]),
    nextId  :: !BuildReportId
  }

--instance Binary BuildReports where

empty :: BuildReports
empty = BuildReports {
    reports = Map.empty,
    logs    = Map.empty,
    index   = Map.empty,
    nextId  = BuildReportId 0
  }

addReport :: BuildReports -> BuildReport -> (BuildReports, BuildReportId)
addReport buildReports report = (buildReports', nextId')
  where
    nextId'       = case nextId buildReports of
                      BuildReportId n -> BuildReportId (n + 1)
    pkgid         = BuildReport.package report
    buildReports' = buildReports {
      reports     = Map.insert  nextId'          report  (reports buildReports),
      index       =     prepend pkgid  (nextId', report) (index   buildReports),
      nextId      = nextId'
    }
    prepend k v = Map.insertWith (\_ vs -> v:vs) k [v]

addBuildLog :: BuildReports -> BuildReportId -> BuildLog -> Maybe BuildReports
addBuildLog buildReports reportId buildLog =
  case Map.lookup reportId (reports buildReports) of
    Nothing -> Nothing
    Just _  -> Just buildReports {
                 logs = Map.insert reportId buildLog (logs buildReports)
               }

lookupReport :: BuildReports -> BuildReportId -> Maybe BuildReport
lookupReport buildReports reportId =
  Map.lookup reportId (reports buildReports)

lookupBuildLog :: BuildReports -> BuildReportId -> Maybe BuildLog
lookupBuildLog buildReports reportId =
  Map.lookup reportId (logs buildReports)

lookupPackageReports :: BuildReports -> PackageIdentifier
                     -> [(BuildReportId, BuildReport)]
lookupPackageReports buildReports pkgid =
  case Map.lookup pkgid (index buildReports) of
    Nothing -> []
    Just rs -> rs
