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
         (BuildReport(..), BuildReport_v0)

import Distribution.Package (PackageId)
import Distribution.Text (display)
import Distribution.Pretty (Pretty(..))
import Distribution.Parsec (Parsec(..))
import qualified Distribution.Parsec as P
import qualified Distribution.Compat.Parsing as P
import qualified Distribution.Compat.CharParsing as P

import Distribution.Server.Framework.MemSize
import Distribution.Server.Framework.Instances

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.SafeCopy
import Data.Typeable (Typeable)
import Control.Applicative ((<$>))
import qualified Data.List as L
import qualified Data.Char as Char

import qualified Distribution.Server.Util.Parse as Parse
import qualified Text.PrettyPrint          as Disp
import Text.StringTemplate (ToSElem(..))


newtype BuildReportId = BuildReportId Int
  deriving (Eq, Ord, Typeable, Show, MemSize, Pretty)

incrementReportId :: BuildReportId -> BuildReportId
incrementReportId (BuildReportId n) = BuildReportId (n+1)

-- TODO: factor out common code
instance Parsec BuildReportId where
  -- parse a non-negative integer. No redundant leading zeros allowed.
  -- (this is effectively a relabeled versionDigitParser)
  parsec = (P.some d >>= (fmap BuildReportId . toNumber)) P.<?> "BuildReportId (natural number without redunant leading zeroes)"
    where
      toNumber :: P.CabalParsing m => [Int] -> m Int
      toNumber [0]   = return 0
      toNumber (0:_) = P.unexpected "BuildReportId with redundant leading zero"
      -- TODO: Add sanity check this doesn't overflow
      toNumber xs    = return $ L.foldl' (\a b -> a * 10 + b) 0 xs

      d :: P.CharParsing m => m Int
      d = f <$> P.satisfyRange '0' '9'
      f c = Char.ord c - Char.ord '0'

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

deriveSafeCopy 2 'extension ''BuildReportId
deriveSafeCopy 2 'extension ''BuildLog
deriveSafeCopy 2 'extension ''BuildReports

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

newtype BuildReportId_v0 = BuildReportId_v0 Int deriving (Serialize, Enum, Eq, Ord)
instance SafeCopy BuildReportId_v0

instance Migrate BuildReportId where
    type MigrateFrom BuildReportId = BuildReportId_v0
    migrate (BuildReportId_v0 bid) = BuildReportId bid

---

newtype BuildLog_v0 = BuildLog_v0 BlobStorage.BlobId_v0 deriving Serialize
instance SafeCopy BuildLog_v0

instance Migrate BuildLog where
    type MigrateFrom BuildLog = BuildLog_v0
    migrate (BuildLog_v0 bl) = BuildLog (migrate bl)

---

data BuildReports_v0 = BuildReports_v0
                         !(Map.Map PackageIdentifier_v0 PkgBuildReports_v0)

instance SafeCopy  BuildReports_v0
instance Serialize BuildReports_v0 where
    put (BuildReports_v0 index) = Serialize.put index
    get = BuildReports_v0 <$> Serialize.get

instance Migrate BuildReports where
     type MigrateFrom BuildReports = BuildReports_v0
     migrate (BuildReports_v0 m) =
       BuildReports (Map.mapKeys migrate $ Map.map migrate m)

---

data PkgBuildReports_v0 = PkgBuildReports_v0
                           !(Map BuildReportId_v0 (BuildReport_v0, Maybe BuildLog_v0))
                           !BuildReportId_v0

instance SafeCopy  PkgBuildReports_v0
instance Serialize PkgBuildReports_v0 where
    put (PkgBuildReports_v0 listing _) = Serialize.put listing
    get = mkReports <$> Serialize.get
      where
        mkReports rs = PkgBuildReports_v0 rs
                         (if Map.null rs
                            then BuildReportId_v0 1
                            else succ (fst $ Map.findMax rs))

instance Migrate PkgBuildReports where
     type MigrateFrom PkgBuildReports = PkgBuildReports_v0
     migrate (PkgBuildReports_v0 m n) =
         PkgBuildReports (migrateMap m) (migrate n)
       where
         migrateMap :: Map BuildReportId_v0 (BuildReport_v0, Maybe BuildLog_v0)
                    -> Map BuildReportId    (BuildReport,    Maybe BuildLog)
         migrateMap = Map.mapKeys migrate
                    . Map.map (\(br, l) -> (migrate (migrate br),
                                            fmap migrate  l))
