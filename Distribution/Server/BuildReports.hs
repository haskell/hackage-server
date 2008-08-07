{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
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

import qualified Distribution.Server.BlobStorage as BlobStorage
import qualified Distribution.Server.BuildReport as BuildReport
import Distribution.Server.BuildReport (BuildReport)

import Distribution.Package
         ( PackageIdentifier )
import Distribution.Text
         ( Text(..) )

import HAppS.Data.Serialize

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import qualified Data.Char as Char

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint          as Disp

newtype BuildReportId = BuildReportId Int
  deriving (Eq, Ord, Binary, Typeable)

instance Version BuildReportId where
    mode = Versioned 0 Nothing

instance Serialize BuildReportId where
    putCopy = contain . Binary.put
    getCopy = contain Binary.get

instance Text BuildReportId where
  disp (BuildReportId n) = Disp.int n
  parse = do
    n <- digits
    return (BuildReportId n)
    where
      digits = do
        first <- Parse.satisfy Char.isDigit
        if first == '0'
          then return 0
          else do rest <- Parse.munch Char.isDigit
                  return (read (first : rest))

newtype BuildLog = BuildLog BlobStorage.BlobId
  deriving (Eq, Binary, Typeable)

instance Version BuildLog where
    mode = Versioned 0 Nothing

instance Serialize BuildLog where
    putCopy = contain . Binary.put
    getCopy = contain Binary.get

data BuildReports = BuildReports {
    reports :: !(Map.Map BuildReportId BuildReport),
    logs    :: !(Map.Map BuildReportId BuildLog),
    index   :: !(Map.Map PackageIdentifier [(BuildReportId, BuildReport)]),
    nextId  :: !BuildReportId
  }
  deriving Typeable

empty :: BuildReports
empty = BuildReports {
    reports = Map.empty,
    logs    = Map.empty,
    index   = Map.empty,
    nextId  = BuildReportId 0
  }

addReport :: BuildReports -> BuildReport -> (BuildReports, BuildReportId)
addReport buildReports report = (buildReports', curid)
  where
    curid         = nextId buildReports
    pkgid         = BuildReport.package report
    buildReports' = buildReports {
      reports     = Map.insert  curid         report  (reports buildReports),
      index       =     prepend pkgid (curid, report) (index   buildReports),
      nextId      = case curid of BuildReportId n -> BuildReportId (n + 1)
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


-------------------
-- Binary instances
--

instance Binary BuildReport where
  put = Binary.put . BS.Char8.pack . BuildReport.show
  get = (BuildReport.read . BS.Char8.unpack) `fmap` Binary.get

instance Version BuildReport where
    mode = Versioned 0 Nothing

instance Serialize BuildReport where
    putCopy = contain . Binary.put
    getCopy = contain Binary.get

instance Binary BuildReports where
  put (BuildReports rs ls _ _) = do
    Binary.put rs
    Binary.put ls
  get = do
    rs <- Binary.get
    ls <- Binary.get
    return BuildReports {
      reports = rs,
      logs    = ls,
      index   = Map.fromListWith (++)
                  [ (BuildReport.package r, [e])
                  | e@(_, r) <- Map.toList rs ],
      nextId  = if Map.null rs
                  then BuildReportId 0
                  else case maximum (Map.keys rs) of
                         BuildReportId n -> BuildReportId (n + 1)
    }
