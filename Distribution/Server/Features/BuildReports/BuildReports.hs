{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell,
             TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import Distribution.Server.Features.BuildReports.BuildReport
         (BuildReport(..), BuildReport_v1)

import Distribution.Package (PackageId)
import Distribution.Text (Text(..), display)

import Distribution.Server.Framework.MemSize
import Distribution.Server.Framework.Instances ()

import Data.Map (Map)
import qualified Data.Map as Map
import Data.SafeCopy
import Data.Typeable (Typeable)
import Control.Applicative ((<$>))

import qualified Distribution.Server.Util.Parse as Parse
import qualified Text.PrettyPrint          as Disp
import Text.StringTemplate (ToSElem(..))


newtype BuildReportId = BuildReportId Int
  deriving (Eq, Ord, Typeable, Show, MemSize)

incrementReportId :: BuildReportId -> BuildReportId
incrementReportId (BuildReportId n) = BuildReportId (n+1)

instance Text BuildReportId where
  disp (BuildReportId n) = Disp.int n
  parse = BuildReportId <$> Parse.int

newtype BuildLog = BuildLog BlobStorage.BlobId
  deriving (Eq, Typeable, Show, MemSize)

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
-- HStringTemplate instances
--

instance ToSElem BuildReportId where
    toSElem = toSElem . display

-------------------
-- SafeCopy instances
--

deriveSafeCopy 2 'base      ''BuildReportId
deriveSafeCopy 2 'base      ''BuildLog
deriveSafeCopy 3 'extension ''BuildReports

-- note: if the set of report ids is [1, 2, 3], then nextReportId = 4
-- after calling deleteReport for 3, the set is [1, 2] and nextReportId is still 4.
-- however, upon importing, nextReportId will = 3, one more than the maximum present
-- this is also a problem in ReportsBackup.hs. but it's not a major issue I think.
instance SafeCopy PkgBuildReports where
    version = 2
    kind    = extension
    putCopy (PkgBuildReports x _) = contain $ safePut x
    getCopy = contain $ mkReports <$> safeGet
      where
        mkReports rs = PkgBuildReports rs
                         (if Map.null rs
                            then BuildReportId 1
                            else incrementReportId (fst $ Map.findMax rs))

instance MemSize BuildReports where
    memSize (BuildReports a) = memSize1 a

instance MemSize PkgBuildReports where
    memSize (PkgBuildReports a b) = memSize2 a b


-------------------
-- Old SafeCopy versions
--

data BuildReports_v1 = BuildReports_v1
                         !(Map.Map PackageId PkgBuildReports_v1)

instance Migrate BuildReports where
     type MigrateFrom BuildReports = BuildReports_v1
     migrate (BuildReports_v1 m) =
       BuildReports (Map.map migrate m)

---

data PkgBuildReports_v1 = PkgBuildReports_v1
                           !(Map BuildReportId (BuildReport_v1, Maybe BuildLog))
                           !BuildReportId

instance Migrate PkgBuildReports where
     type MigrateFrom PkgBuildReports = PkgBuildReports_v1
     migrate (PkgBuildReports_v1 m n) =
         PkgBuildReports (migrateMap m) n
       where
         migrateMap = Map.map (\(br, l) -> (migrate br, l))

---

deriveSafeCopy 2 'base ''PkgBuildReports_v1
deriveSafeCopy 2 'base ''BuildReports_v1
