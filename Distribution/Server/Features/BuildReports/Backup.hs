{-# LANGUAGE PatternGuards #-}
module Distribution.Server.Features.BuildReports.Backup (
    dumpBackup,
    restoreBackup,
    buildReportsToExport,
    packageReportsToExport
  ) where

import Distribution.Server.Features.BuildReports.BuildReport (BuildReport)
import qualified Distribution.Server.Features.BuildReports.BuildReport as Report
import Distribution.Server.Features.BuildReports.BuildReports (BuildReports(..), PkgBuildReports(..), BuildReportId(..), BuildLog(..))
import qualified Distribution.Server.Features.BuildReports.BuildReports as Reports

import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Util.Parse (packUTF8)

import Distribution.Package
import Distribution.Text (display, simpleParse)
import Distribution.Version

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import System.FilePath (splitExtension)
import Data.ByteString.Lazy (ByteString, toStrict)

dumpBackup  :: BuildReports -> [BackupEntry]
dumpBackup = buildReportsToExport

restoreBackup :: RestoreBackup BuildReports
restoreBackup = updateReports Reports.emptyReports Map.empty

-- when logs are encountered before their corresponding build reports
type PartialLogs = Map (PackageId, BuildReportId) BuildLog

updateReports :: BuildReports -> PartialLogs -> RestoreBackup BuildReports
updateReports buildReports partialLogs = RestoreBackup {
    restoreEntry = \entry -> do
      case entry of
        BackupByteString ["package", pkgStr, reportItem] bs
          | Just pkgId <- simpleParse pkgStr
          , (num, ".txt") <- splitExtension reportItem ->
              do checkPackageVersion pkgStr pkgId
                 (buildReports', partialLogs') <- importReport pkgId num bs buildReports partialLogs
                 return (updateReports buildReports' partialLogs')
        BackupBlob ["package", pkgStr, reportItem] blobId
          | Just pkgId <- simpleParse pkgStr
          , (num, ".log") <- splitExtension reportItem ->
              do checkPackageVersion pkgStr pkgId
                 (buildReports', partialLogs') <- importLog pkgId num blobId buildReports partialLogs
                 return (updateReports buildReports' partialLogs')
        _ ->
          return (updateReports buildReports partialLogs)
  , restoreFinalize =
      foldM insertLog buildReports (Map.toList partialLogs)
  }

insertLog :: BuildReports -> ((PackageId, BuildReportId), BuildLog) -> Restore BuildReports
insertLog buildReps ((pkgId, reportId), buildLog) =
  case Reports.setBuildLog pkgId reportId (Just buildLog) buildReps of
    Just buildReps' -> return buildReps'
    Nothing -> fail $ "Build log #" ++ display reportId ++ " exists for " ++ display pkgId ++ " but report itself does not"

checkPackageVersion :: String -> PackageIdentifier -> Restore ()
checkPackageVersion pkgStr pkgId
  | packageVersion pkgId == nullVersion
  = fail $ "Build report package id " ++ show pkgStr ++ " must specify a version"
  | otherwise
  = return ()

importReport :: PackageId -> String -> ByteString -> BuildReports -> PartialLogs -> Restore (BuildReports, PartialLogs)
importReport pkgId repIdStr contents buildReps partialLogs = do
  reportId <- parseText "report id" repIdStr
  report   <- either fail return $ Report.parse (toStrict contents)
  let (mlog, partialLogs') = Map.updateLookupWithKey (\_ _ -> Nothing) (pkgId, reportId) partialLogs
      buildReps' = Reports.unsafeSetReport pkgId reportId (report, mlog) buildReps --doesn't check for duplicates
  return (buildReps', partialLogs')

importLog :: PackageId -> String -> BlobStorage.BlobId -> BuildReports -> PartialLogs -> Restore (BuildReports, PartialLogs)
importLog pkgId repIdStr blobId buildReps logs = do
  reportId <- parseText "report id" repIdStr
  let buildLog = BuildLog blobId
  case Reports.setBuildLog pkgId reportId (Just buildLog) buildReps of
    Nothing -> return (buildReps, Map.insert (pkgId, reportId) buildLog logs)
    Just buildReps' -> return (buildReps', logs)

------------------------------------------------------------------------------
buildReportsToExport :: BuildReports -> [BackupEntry]
buildReportsToExport buildReports = concatMap (uncurry packageReportsToExport) (Map.toList $ Reports.reportsIndex buildReports)

packageReportsToExport :: PackageId -> PkgBuildReports -> [BackupEntry]
packageReportsToExport pkgId pkgReports = concatMap (uncurry $ reportToExport prefix) (Map.toList $ Reports.reports pkgReports)
    where prefix = ["package", display pkgId]

reportToExport :: [FilePath] -> BuildReportId -> (BuildReport, Maybe BuildLog) -> [BackupEntry]
reportToExport prefix reportId (report, mlog) = BackupByteString (getPath ".txt") (packUTF8 $ Report.show report) :
    case mlog of Nothing -> []; Just (BuildLog blobId) -> [blobToBackup (getPath ".log") blobId]
  where
    getPath ext = prefix ++ [display reportId ++ ext]

