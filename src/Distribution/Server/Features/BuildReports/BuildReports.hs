{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell,
             TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Server.Features.BuildReports.BuildReports (
    BuildReport(..),
    BuildReports(..),
    BuildReportId(..),
    PkgBuildReports(..),
    BuildLog(..),
    BuildCovg(..),
    BuildStatus(..),
    addRptLogCovg,
    lookupReportCovg,
    emptyReports,
    emptyPkgReports,
    addReport,
    deleteReport,
    setBuildLog,
    lookupReport,
    lookupPackageReports,
    unsafeSetReport,
    setFailStatus,
    resetFailCount,
    lookupLatestReport,
    lookupFailCount
  ) where

import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import Distribution.Server.Features.BuildReports.BuildReport
         (BuildReport(..), BuildReport_v0,BuildStatus(..), BuildCovg(..))

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
import Data.Serialize (Serialize, get, put)
import Data.SafeCopy
import Data.Typeable (Typeable)
import qualified Data.List as L
import qualified Data.Char as Char

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
    reports      :: !(Map BuildReportId (BuildReport, Maybe BuildLog, Maybe BuildCovg )),
    -- one more than the maximum report id used
    nextReportId :: !BuildReportId,
    buildStatus :: !BuildStatus
} deriving (Eq, Typeable, Show)

data BuildReports = BuildReports {
    reportsIndex :: !(Map.Map PackageId PkgBuildReports)
} deriving (Eq, Typeable, Show)

emptyPkgReports :: PkgBuildReports
emptyPkgReports = PkgBuildReports {
    reports = Map.empty,
    nextReportId = BuildReportId 1,
    buildStatus = BuildFailCnt 0
}

emptyReports :: BuildReports
emptyReports = BuildReports {
    reportsIndex = Map.empty
}

lookupReport :: PackageId -> BuildReportId -> BuildReports -> Maybe (BuildReport, Maybe BuildLog)
lookupReport pkgid reportId buildReports = remCvg.Map.lookup reportId . reports =<< Map.lookup pkgid (reportsIndex buildReports)
    where
        remCvg Nothing = Nothing
        remCvg (Just (brpt,blog,_)) = Just (brpt,blog)

lookupPackageReports :: PackageId -> BuildReports -> [(BuildReportId, (BuildReport, Maybe BuildLog))]
lookupPackageReports pkgid buildReports = case Map.lookup pkgid (reportsIndex buildReports) of
    Nothing -> []
    Just rs -> map removeCovg $Map.toList (reports rs)
    where
        removeCovg (brid,(brpt,blog,_)) = (brid,(brpt,blog))

-------------------------
-- PackageIds should /not/ have empty Versions. Caller should ensure this.
addReport :: PackageId -> (BuildReport, Maybe BuildLog) -> BuildReports -> (BuildReports, BuildReportId)
addReport pkgid (brpt,blog) buildReports =
    let pkgReports  = Map.findWithDefault emptyPkgReports pkgid (reportsIndex buildReports)
        reportId    = nextReportId pkgReports
        pkgReports' = PkgBuildReports { reports = Map.insert reportId (brpt,blog,Nothing) (reports pkgReports)
                                      , nextReportId = incrementReportId reportId
                                      , buildStatus = buildStatus pkgReports }
    in (buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }, reportId)

unsafeSetReport :: PackageId -> BuildReportId -> (BuildReport, Maybe BuildLog) -> BuildReports -> BuildReports
unsafeSetReport pkgid reportId (brpt,blog) buildReports =
    let pkgReports  = Map.findWithDefault emptyPkgReports pkgid (reportsIndex buildReports)
        pkgReports' = PkgBuildReports { reports = Map.insert reportId (brpt,blog,Nothing) (reports pkgReports)
                                      , nextReportId = max (incrementReportId reportId) (nextReportId pkgReports)
                                      , buildStatus = buildStatus pkgReports }
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
        Just (rlog, _, covg) -> let pkgReports' = pkgReports { reports = Map.insert reportId (rlog, buildLog, covg) (reports pkgReports) }
                         in Just $ buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }

addRptLogCovg :: PackageId -> (BuildReport, Maybe BuildLog, Maybe BuildCovg ) -> BuildReports -> (BuildReports, BuildReportId)
addRptLogCovg pkgid report buildReports =
    let pkgReports  = Map.findWithDefault emptyPkgReports pkgid (reportsIndex buildReports)
        reportId    = nextReportId pkgReports
        pkgReports' = PkgBuildReports { reports = Map.insert reportId report (reports pkgReports)
                                      , nextReportId = incrementReportId reportId
                                      , buildStatus = buildStatus pkgReports }
    in (buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }, reportId)

lookupReportCovg :: PackageId -> BuildReportId -> BuildReports -> Maybe (BuildReport, Maybe BuildLog, Maybe BuildCovg )
lookupReportCovg pkgid reportId buildReports = Map.lookup reportId . reports =<< Map.lookup pkgid (reportsIndex buildReports)

setFailStatus :: PackageId -> Bool -> BuildReports -> BuildReports
setFailStatus pkgid fStatus buildReports =
    let pkgReports  = Map.findWithDefault emptyPkgReports pkgid (reportsIndex buildReports)
        pkgReports' = PkgBuildReports { reports = (reports pkgReports)
                                      , nextReportId = (nextReportId pkgReports)
                                      , buildStatus = (getfst fStatus (buildStatus pkgReports)) }
    in buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }
    where
      getfst nfst cfst = do
        case cfst of
          (BuildFailCnt n) | nfst -> BuildFailCnt (n+1)
          _ -> BuildOK


resetFailCount :: PackageId -> BuildReports -> Maybe BuildReports
resetFailCount pkgid buildReports = case Map.lookup pkgid (reportsIndex buildReports) of
    Nothing -> Nothing
    Just pkgReports -> do
      let pkgReports' = PkgBuildReports { reports = (reports pkgReports)
                                      , nextReportId = (nextReportId pkgReports)
                                      , buildStatus = BuildFailCnt 0 }
      return buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }

lookupFailCount :: PackageId -> BuildReports -> Maybe BuildStatus
lookupFailCount pkgid buildReports = do
  rp <- Map.lookup pkgid (reportsIndex buildReports)
  return $ buildStatus rp

lookupLatestReport :: PackageId -> BuildReports -> Maybe (BuildReport, Maybe BuildLog, Maybe BuildCovg)
lookupLatestReport pkgid buildReports = do
  rp <- Map.lookup pkgid (reportsIndex buildReports)
  let rs = reports rp
  a  <- if Map.null rs
          then Nothing
          else Just $ fst $ Map.findMax rs
  Map.lookup a rs

-- addPkg::`
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
deriveSafeCopy 3 'extension ''BuildReports

-- note: if the set of report ids is [1, 2, 3], then nextReportId = 4
-- after calling deleteReport for 3, the set is [1, 2] and nextReportId is still 4.
-- however, upon importing, nextReportId will = 3, one more than the maximum present
-- this is also a problem in ReportsBackup.hs. but it's not a major issue I think.
instance SafeCopy PkgBuildReports where
    version = 3
    kind    = extension
    putCopy (PkgBuildReports x _ y) = contain $ safePut (x,y)
    getCopy = contain $ mkReports <$> safeGet
      where
        mkReports (rs,f) = PkgBuildReports rs
                         (if Map.null rs
                            then BuildReportId 1
                            else incrementReportId (fst $ Map.findMax rs))
                          f
instance MemSize BuildReports where
    memSize (BuildReports a) = memSize1 a

instance MemSize PkgBuildReports where
    memSize (PkgBuildReports a b c) = memSize3 a b c

-------------------
-- Old V2 SafeCopy versions
--
data PkgBuildReports_v2 = PkgBuildReports_v2 {
    reports_v2      :: !(Map BuildReportId (BuildReport, Maybe BuildLog)),
    nextReportId_v2 :: !BuildReportId
} deriving (Eq, Typeable, Show)


instance SafeCopy PkgBuildReports_v2 where
    version = 2
    kind    = extension
    putCopy (PkgBuildReports_v2 x _) = contain $ safePut x
    getCopy = contain $ mkReports <$> safeGet
      where
        mkReports rs = PkgBuildReports_v2 rs
                         (if Map.null rs
                            then BuildReportId 1
                            else incrementReportId (fst $ Map.findMax rs))

instance MemSize PkgBuildReports_v2 where
    memSize (PkgBuildReports_v2 a b) = memSize2 a b


instance Migrate PkgBuildReports where
     type MigrateFrom PkgBuildReports = PkgBuildReports_v2
     migrate (PkgBuildReports_v2 m n) =
         PkgBuildReports (migrateMap m) n BuildOK
       where
         migrateMap :: Map BuildReportId (BuildReport, Maybe BuildLog)
                    -> Map BuildReportId (BuildReport, Maybe BuildLog, Maybe BuildCovg)
         migrateMap = Map.mapKeys (\x->x)
                    . Map.map (\(br, l) -> (br, l, Nothing))

---

data BuildReports_v2 = BuildReports_v2 {
    reportsIndex_v2 :: !(Map.Map PackageId PkgBuildReports_v2)
} deriving (Eq, Typeable, Show)

deriveSafeCopy 2 'extension ''BuildReports_v2

instance MemSize BuildReports_v2 where
    memSize (BuildReports_v2 a) = memSize1 a

instance Migrate BuildReports where
     type MigrateFrom BuildReports = BuildReports_v2
     migrate (BuildReports_v2 m) =
       BuildReports (Map.mapKeys (\x->x) $ Map.map migrate m)
-------------------
-- Old SafeCopy versions
--

newtype BuildReportId_v0 = BuildReportId_v0 Int deriving (Serialize, Enum, Eq, Ord)
instance SafeCopy BuildReportId_v0 where
    getCopy = contain get
    putCopy = contain . put

instance Migrate BuildReportId where
    type MigrateFrom BuildReportId = BuildReportId_v0
    migrate (BuildReportId_v0 bid) = BuildReportId bid

---

newtype BuildLog_v0 = BuildLog_v0 BlobStorage.BlobId_v0 deriving Serialize
instance SafeCopy BuildLog_v0 where
    getCopy = contain get
    putCopy = contain . put


instance Migrate BuildLog where
    type MigrateFrom BuildLog = BuildLog_v0
    migrate (BuildLog_v0 bl) = BuildLog (migrate bl)

---

data BuildReports_v0 = BuildReports_v0
                         !(Map.Map PackageIdentifier_v0 PkgBuildReports_v0)

instance SafeCopy  BuildReports_v0 where
    getCopy = contain get
    putCopy = contain . put

instance Serialize BuildReports_v0 where
    put (BuildReports_v0 index) = Serialize.put index
    get = BuildReports_v0 <$> Serialize.get

instance Migrate BuildReports_v2 where
     type MigrateFrom BuildReports_v2 = BuildReports_v0
     migrate (BuildReports_v0 m) =
       BuildReports_v2 (Map.mapKeys migrate $ Map.map migrate m)

---

data PkgBuildReports_v0 = PkgBuildReports_v0
                           !(Map BuildReportId_v0 (BuildReport_v0, Maybe BuildLog_v0))
                           !BuildReportId_v0

instance SafeCopy  PkgBuildReports_v0 where
    getCopy = contain get
    putCopy = contain . put

instance Serialize PkgBuildReports_v0 where
    put (PkgBuildReports_v0 listing _) = Serialize.put listing
    get = mkReports <$> Serialize.get
      where
        mkReports rs = PkgBuildReports_v0 rs
                         (if Map.null rs
                            then BuildReportId_v0 1
                            else succ (fst $ Map.findMax rs))

instance Migrate PkgBuildReports_v2 where
     type MigrateFrom PkgBuildReports_v2 = PkgBuildReports_v0
     migrate (PkgBuildReports_v0 m n) =
         PkgBuildReports_v2 (migrateMap m) (migrate n)
       where
         migrateMap :: Map BuildReportId_v0 (BuildReport_v0, Maybe BuildLog_v0)
                    -> Map BuildReportId    (BuildReport,    Maybe BuildLog)
         migrateMap = Map.mapKeys migrate
                    . Map.map (\(br, l) -> (migrate (migrate br),
                                            fmap migrate  l))
