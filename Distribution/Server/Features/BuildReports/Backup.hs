{-# LANGUAGE PatternGuards #-}
module Distribution.Server.Features.BuildReports.Backup (
    dumpBackup,
    restoreBackup,
    buildReportsToExport,
    packageReportsToExport
  ) where

import Data.Acid (AcidState, update)
import Distribution.Server.Features.BuildReports.BuildReport (BuildReport)
import qualified Distribution.Server.Features.BuildReports.BuildReport as Report
import Distribution.Server.Features.BuildReports.BuildReports (BuildReports(..), PkgBuildReports(..), BuildReportId(..), BuildLog(..))
import qualified Distribution.Server.Features.BuildReports.BuildReports as Reports
import Distribution.Server.Features.BuildReports.State

import Distribution.Server.Framework.BlobStorage (BlobStorage)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Util.Parse (unpackUTF8)

import Distribution.Package
import Distribution.Text (display, simpleParse)
import Distribution.Version

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans (liftIO)
import Control.Monad.State (get, put)
import Data.Monoid (mempty)
import System.FilePath (splitExtension)
import Data.ByteString.Lazy.Char8 (ByteString)

dumpBackup  :: BuildReports -> [BackupEntry]
dumpBackup = buildReportsToExport

restoreBackup :: AcidState BuildReports -> BlobStorage -> RestoreBackup
restoreBackup reportsState storage = updateReports reportsState storage (Reports.emptyReports, Map.empty)

-- when logs are encountered before their corresponding build reports
type PartialLogs = Map (PackageId, BuildReportId) BuildLog

updateReports :: AcidState BuildReports -> BlobStorage -> (BuildReports, PartialLogs) -> RestoreBackup
updateReports reportsState storage reportLogs@(buildReports, partialLogs) = RestoreBackup
  { restoreEntry = \entry bs -> do
        res <- runImport reportLogs $ case entry of
            ["package", pkgStr, reportItem] | Just pkgid <- simpleParse pkgStr -> case packageVersion pkgid of
                Version [] [] -> fail $ "Build report package id " ++ show pkgStr ++ " must specify a version"
                _ -> case splitExtension reportItem of
                        (num, "txt") -> importReport pkgid num bs
                        (num, "log") -> importLog storage pkgid num bs
                        _ -> return ()
            _ -> return ()
        return $ fmap (updateReports reportsState storage) res
  , restoreFinalize = do
        let insertLog buildReps ((pkgid, reportId), buildLog) = case Reports.setBuildLog pkgid reportId (Just buildLog) buildReps of
                Just buildReps' -> Right buildReps'
                Nothing -> Left $ "Build log #" ++ display reportId ++ " exists for " ++ display pkgid ++ " but report itself does not"
        case foldM insertLog buildReports (Map.toList partialLogs) of
            Right theReports -> return . Right $ finalizeReports reportsState theReports
            Left err -> return . Left $ err
  , restoreComplete = return ()
  }

finalizeReports :: AcidState BuildReports -> BuildReports -> RestoreBackup
finalizeReports reportsState buildReports = mempty
  { restoreComplete = update reportsState $ ReplaceBuildReports buildReports
  }

importReport :: PackageId -> String -> ByteString -> Import (BuildReports, PartialLogs) ()
importReport pkgid repIdStr contents = do
    reportId <- parseText "report id" repIdStr
    case Report.parse (unpackUTF8 contents) of
        Left err -> fail err
        Right report -> do
            (buildReps, partialLogs) <- get
            let (mlog, partialLogs') = Map.updateLookupWithKey (\_ _ -> Nothing) (pkgid, reportId) partialLogs
                buildReps' = Reports.unsafeSetReport pkgid reportId (report, mlog) buildReps --doesn't check for duplicates
            put (buildReps', partialLogs')

importLog :: BlobStorage -> PackageId -> String -> ByteString -> Import (BuildReports, PartialLogs) ()
importLog storage pkgid repIdStr contents = do
    reportId <- parseText "report id" repIdStr
    blobId <- liftIO $ BlobStorage.add storage contents
    let buildLog = BuildLog blobId
    (buildReps, logs) <- get
    case Reports.setBuildLog pkgid reportId (Just buildLog) buildReps of
        Nothing -> put (buildReps, Map.insert (pkgid, reportId) buildLog logs)
        Just buildReps' -> put (buildReps', logs)

------------------------------------------------------------------------------
buildReportsToExport :: BuildReports -> [BackupEntry]
buildReportsToExport buildReports = concatMap (uncurry packageReportsToExport) (Map.toList $ Reports.reportsIndex buildReports)

packageReportsToExport :: PackageId -> PkgBuildReports -> [BackupEntry]
packageReportsToExport pkgid pkgReports = concatMap (uncurry $ reportToExport prefix) (Map.toList $ Reports.reports pkgReports)
    where prefix = ["package", display pkgid]

reportToExport :: [FilePath] -> BuildReportId -> (BuildReport, Maybe BuildLog) -> [BackupEntry]
reportToExport prefix reportId (report, mlog) = BackupByteString (getPath ".txt") (stringToBytes $ Report.show report) :
    case mlog of Nothing -> []; Just (BuildLog blobId) -> [blobToBackup (getPath ".log") blobId]
  where
    getPath ext = prefix ++ [display reportId ++ ext]

