{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell,
             TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Server.Features.BuildReports.BuildReports (
    BuildReport(..),
    BuildReports(..),
    BuildReports_v3,
    BuildReportId(..),
    PkgBuildReports(..),
    BuildLog(..),
    TestLog(..),
    BuildCovg(..),
    BuildStatus(..),
    addRptLogTestCovg,
    lookupReportCovg,
    emptyReports,
    emptyPkgReports,
    addReport,
    deleteReport,
    setBuildLog,
    setTestLog,
    lookupReport,
    lookupPackageReports,
    unsafeSetReport,
    setFailStatus,
    resetFailCount,
    lookupLatestReport,
    lookupFailCount,
    lookupRunTests,
    setRunTests
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
import Data.Maybe (fromMaybe)

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

newtype TestLog = TestLog BlobStorage.BlobId
  deriving (Eq, Typeable, Show, MemSize)

data PkgBuildReports = PkgBuildReports {
    -- for each report, other useful information: Maybe UserId, UTCTime
    -- perhaps deserving its own data structure (SubmittedReport?)
    -- When a report was submitted is very useful information.
    -- also, use IntMap instead of Map BuildReportId?
    reports      :: !(Map BuildReportId (BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg )),
    -- one more than the maximum report id used
    nextReportId :: !BuildReportId,
    buildStatus :: !BuildStatus,
    runTests     :: !Bool
} deriving (Eq, Typeable, Show)

data BuildReports = BuildReports {
    reportsIndex :: !(Map.Map PackageId PkgBuildReports)

} deriving (Eq, Typeable, Show)

emptyPkgReports :: PkgBuildReports
emptyPkgReports = PkgBuildReports {
    reports = Map.empty,
    nextReportId = BuildReportId 1,
    buildStatus = BuildFailCnt 0,
    runTests = True
}

emptyReports :: BuildReports
emptyReports = BuildReports {
    reportsIndex = Map.empty
}

lookupReport :: PackageId -> BuildReportId -> BuildReports -> Maybe (BuildReport, Maybe BuildLog, Maybe TestLog)
lookupReport pkgid reportId buildReports = remCvg.Map.lookup reportId . reports =<< Map.lookup pkgid (reportsIndex buildReports)
    where
        remCvg Nothing = Nothing
        remCvg (Just (brpt,blog,btest,_)) = Just (brpt,blog,btest)

lookupPackageReports :: PackageId -> BuildReports -> [(BuildReportId, (BuildReport, Maybe BuildLog, Maybe TestLog))]
lookupPackageReports pkgid buildReports = case Map.lookup pkgid (reportsIndex buildReports) of
    Nothing -> []
    Just rs -> map removeCovg $ Map.toList (reports rs)
    where
        removeCovg (brid,(brpt,blog,btest,_)) = (brid,(brpt,blog,btest))

-------------------------
-- PackageIds should /not/ have empty Versions. Caller should ensure this.
addReport :: PackageId -> (BuildReport, Maybe BuildLog, Maybe TestLog) -> BuildReports -> (BuildReports, BuildReportId)
addReport pkgid (brpt,blog,btest) buildReports =
    let pkgReports  = Map.findWithDefault emptyPkgReports pkgid (reportsIndex buildReports)
        reportId    = nextReportId pkgReports
        pkgReports' = PkgBuildReports { reports = Map.insert reportId (brpt,blog,btest,Nothing) (reports pkgReports)
                                      , nextReportId = incrementReportId reportId
                                      , buildStatus = buildStatus pkgReports
                                      , runTests = runTests pkgReports }
    in (buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }, reportId)

unsafeSetReport :: PackageId -> BuildReportId -> (BuildReport, Maybe BuildLog) -> BuildReports -> BuildReports
unsafeSetReport pkgid reportId (brpt,blog) buildReports =
    let pkgReports  = Map.findWithDefault emptyPkgReports pkgid (reportsIndex buildReports)
        pkgReports' = PkgBuildReports { reports = Map.insert reportId (brpt,blog,Nothing,Nothing) (reports pkgReports)
                                      , nextReportId = max (incrementReportId reportId) (nextReportId pkgReports)
                                      , buildStatus = buildStatus pkgReports
                                      , runTests = runTests pkgReports }
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
        Just (rlog, _, btest, covg) -> let pkgReports' = pkgReports { reports = Map.insert reportId (rlog, buildLog, btest, covg) (reports pkgReports) }
                         in Just $ buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }

setTestLog :: PackageId -> BuildReportId -> Maybe TestLog -> BuildReports -> Maybe BuildReports
setTestLog pkgid reportId testLog buildReports = case Map.lookup pkgid (reportsIndex buildReports) of
    Nothing -> Nothing
    Just pkgReports -> case Map.lookup reportId (reports pkgReports) of
        Nothing -> Nothing
        Just (rlog, blog, _, covg) -> let pkgReports' = pkgReports { reports = Map.insert reportId (rlog, blog, testLog, covg) (reports pkgReports) }
                         in Just $ buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }

addRptLogTestCovg :: PackageId -> (BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg ) -> BuildReports -> (BuildReports, BuildReportId)
addRptLogTestCovg pkgid report buildReports =
    let pkgReports  = Map.findWithDefault emptyPkgReports pkgid (reportsIndex buildReports)
        reportId    = nextReportId pkgReports
        pkgReports' = PkgBuildReports { reports = Map.insert reportId report (reports pkgReports)
                                      , nextReportId = incrementReportId reportId
                                      , buildStatus = buildStatus pkgReports
                                      , runTests = runTests pkgReports }
    in (buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }, reportId)

lookupReportCovg :: PackageId -> BuildReportId -> BuildReports -> Maybe (BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg )
lookupReportCovg pkgid reportId buildReports = Map.lookup reportId . reports =<< Map.lookup pkgid (reportsIndex buildReports)

setFailStatus :: PackageId -> Bool -> BuildReports -> BuildReports
setFailStatus pkgid fStatus buildReports =
    let pkgReports  = Map.findWithDefault emptyPkgReports pkgid (reportsIndex buildReports)
        pkgReports' = PkgBuildReports { reports = (reports pkgReports)
                                      , nextReportId = (nextReportId pkgReports)
                                      , buildStatus = (getfst fStatus (buildStatus pkgReports))
                                      , runTests = runTests pkgReports }
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
                                      , buildStatus = BuildFailCnt 0
                                      , runTests = runTests pkgReports }
      return buildReports { reportsIndex = Map.insert pkgid pkgReports' (reportsIndex buildReports) }

lookupFailCount :: PackageId -> BuildReports -> Maybe BuildStatus
lookupFailCount pkgid buildReports = do
  rp <- Map.lookup pkgid (reportsIndex buildReports)
  return $ buildStatus rp

lookupLatestReport :: PackageId -> BuildReports -> Maybe (BuildReportId, BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg)
lookupLatestReport pkgid buildReports = do
  rp <- Map.lookup pkgid (reportsIndex buildReports)
  let rs = reports rp
  (maxKey, (rep, buildLog, testLog, covg)) <-
    if Map.null rs
      then Nothing
      else Just $ Map.findMax rs
  Just (maxKey, rep, buildLog, testLog, covg)

lookupRunTests :: PackageId -> BuildReports -> Bool
lookupRunTests pkgid buildReports = maybe True runTests $ Map.lookup pkgid (reportsIndex buildReports)

setRunTests :: PackageId -> Bool -> BuildReports -> Maybe BuildReports
setRunTests pkgid b buildReports =
  let rp = fromMaybe emptyPkgReports $ Map.lookup pkgid (reportsIndex buildReports)
  in Just $ BuildReports (Map.insert pkgid rp{runTests = b} (reportsIndex buildReports))

-- addPkg::`
-------------------
-- HStringTemplate instances
--

instance ToSElem BuildReportId where
    toSElem = toSElem . display

-------------------
-- SafeCopy instances
--


newtype BuildReportId_v0 = BuildReportId_v0 Int
    deriving (Serialize, Enum, Eq, Ord)

instance SafeCopy BuildReportId_v0 where
    getCopy = contain get
    putCopy = contain . put

instance Migrate BuildReportId where
    type MigrateFrom BuildReportId = BuildReportId_v0
    migrate (BuildReportId_v0 bid) = BuildReportId bid

deriveSafeCopy 2 'extension ''BuildReportId

newtype BuildLog_v0 = BuildLog_v0 BlobStorage.BlobId_v0
    deriving Serialize

instance SafeCopy BuildLog_v0 where
    getCopy = contain get
    putCopy = contain . put

instance Migrate BuildLog where
    type MigrateFrom BuildLog = BuildLog_v0
    migrate (BuildLog_v0 bl) = BuildLog (migrate bl)

deriveSafeCopy 2 'extension ''BuildLog

deriveSafeCopy 0 'base ''TestLog

-- note: if the set of report ids is [1, 2, 3], then nextReportId = 4
-- after calling deleteReport for 3, the set is [1, 2] and nextReportId is still 4.
-- however, upon importing, nextReportId will = 3, one more than the maximum present
-- this is also a problem in ReportsBackup.hs. but it's not a major issue I think.
instance SafeCopy PkgBuildReports where
    version = 4
    kind    = extension
    putCopy (PkgBuildReports x _ y z) = contain $ safePut (x,y,z)
    getCopy = contain $ mkReports <$> safeGet
      where
        mkReports (rs,f,b) = PkgBuildReports rs
                         (if Map.null rs
                            then BuildReportId 1
                            else incrementReportId (fst $ Map.findMax rs))
                          f b

instance MemSize PkgBuildReports where
    memSize (PkgBuildReports a b c d) = memSize4 a b c d


data PkgBuildReports_v3 = PkgBuildReports_v3 {
    reports_v3      :: !(Map BuildReportId (BuildReport, Maybe BuildLog, Maybe BuildCovg )),
    nextReportId_v3 :: !BuildReportId,
    buildStatus_v3 :: !BuildStatus
} deriving (Eq, Typeable, Show)

instance SafeCopy PkgBuildReports_v3 where
    version = 3
    kind    = extension
    putCopy (PkgBuildReports_v3 x _ y) = contain $ safePut (x,y)
    getCopy = contain $ mkReports <$> safeGet
      where
        mkReports (rs,f) = PkgBuildReports_v3 rs
                         (if Map.null rs
                            then BuildReportId 1
                            else incrementReportId (fst $ Map.findMax rs))
                          f

instance MemSize PkgBuildReports_v3 where
    memSize (PkgBuildReports_v3 a b c) = memSize3 a b c

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

instance Migrate PkgBuildReports_v3 where
     type MigrateFrom PkgBuildReports_v3 = PkgBuildReports_v2
     migrate (PkgBuildReports_v2 m n) =
         PkgBuildReports_v3 (migrateMap m) n BuildOK
       where
         migrateMap :: Map BuildReportId (BuildReport, Maybe BuildLog)
                    -> Map BuildReportId (BuildReport, Maybe BuildLog, Maybe BuildCovg)
         migrateMap = Map.mapKeys (\x->x)
                    . Map.map (\(br, l) -> (br, l, Nothing))

instance Migrate PkgBuildReports where
     type MigrateFrom PkgBuildReports = PkgBuildReports_v3
     migrate (PkgBuildReports_v3 m n o) =
         PkgBuildReports (migrateMap m) n o True
       where
         migrateMap :: Map BuildReportId (BuildReport, Maybe BuildLog, Maybe BuildCovg)
                    -> Map BuildReportId (BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg)
         migrateMap = Map.mapKeys id
                    . Map.map (\(br, l, c) -> (br, l, Nothing, c))

data BuildReports_v0 = BuildReports_v0
                         !(Map.Map PackageIdentifier_v0 PkgBuildReports_v0)

instance SafeCopy  BuildReports_v0 where
    getCopy = contain get
    putCopy = contain . put

instance Serialize BuildReports_v0 where
    put (BuildReports_v0 index) = Serialize.put index
    get = BuildReports_v0 <$> Serialize.get

data BuildReports_v2 = BuildReports_v2
  { reportsIndex_v2 :: !(Map.Map PackageId PkgBuildReports_v2)
  } deriving (Eq, Typeable, Show)

instance Migrate BuildReports_v2 where
     type MigrateFrom BuildReports_v2 = BuildReports_v0
     migrate (BuildReports_v0 m) =
       BuildReports_v2 (Map.mapKeys migrate $ Map.map migrate m)

instance MemSize BuildReports_v2 where
    memSize (BuildReports_v2 a) = memSize1 a

deriveSafeCopy 2 'extension ''BuildReports_v2

data BuildReports_v3 = BuildReports_v3
  { reportsIndex_v3 :: !(Map.Map PackageId PkgBuildReports_v3)
  } deriving (Eq, Typeable, Show)

instance Migrate BuildReports_v3 where
    type MigrateFrom BuildReports_v3 = BuildReports_v2
    migrate (BuildReports_v2 m) =
      BuildReports_v3 (Map.mapKeys id $ Map.map migrate m)

instance MemSize BuildReports_v3 where
    memSize (BuildReports_v3 a) = memSize1 a

deriveSafeCopy 3 'extension ''BuildReports_v3

instance Migrate BuildReports where
    type MigrateFrom BuildReports = BuildReports_v3
    migrate (BuildReports_v3 m) =
      BuildReports (Map.mapKeys id $ Map.map migrate m)

instance MemSize BuildReports where
    memSize (BuildReports a) = memSize1 a

deriveSafeCopy 4 'extension ''BuildReports
