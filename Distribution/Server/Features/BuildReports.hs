module Distribution.Server.Features.BuildReports (
    ReportsFeature,
    reportsResource,
    ReportsResource(..),
    initBuildReportsFeature
  ) where

import Distribution.Server.Acid (update, query)
import Distribution.Server.Framework hiding (BuildLog)
import Distribution.Server.Features.Core

import Distribution.Server.Features.BuildReports.Backup
import Distribution.Server.Features.BuildReports.State
import qualified Distribution.Server.Features.BuildReports.BuildReport as BuildReport
import Distribution.Server.Features.BuildReports.BuildReport (BuildReport(..))
import Distribution.Server.Features.BuildReports.BuildReports (BuildReportId(..), BuildLog(..))
import Distribution.Server.Users.State (GetUserDb(..))
import qualified Distribution.Server.Framework.ResourceTypes as Resource
import qualified Distribution.Server.Auth.Basic as Auth

import Distribution.Server.Packages.Types
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BlobStorage (BlobStorage)

import Distribution.Text
import Distribution.Package

import Data.Function (fix)
import Control.Applicative (optional)
import Control.Monad.Trans
import Control.Monad (mzero)
import Data.ByteString.Lazy.Char8 (unpack)

-- TODO: 
-- 1. Put the HTML view for this module in the HTML feature; get rid of the text view
-- 2. Decide build report upload policy (anonymous and authenticated)
data ReportsFeature = ReportsFeature {
    featureInterface :: HackageFeature,
    reportsResource :: ReportsResource
}

data ReportsResource = ReportsResource {
    reportsList :: Resource,
    reportsPage :: Resource,
    reportsLog  :: Resource,
    reportsListUri :: String -> PackageId -> String,
    reportsPageUri :: String -> PackageId -> BuildReportId -> String,
    reportsLogUri  :: PackageId -> BuildReportId -> String
}

instance IsHackageFeature ReportsFeature where
    getFeatureInterface = featureInterface

initBuildReportsFeature :: ServerEnv -> CoreFeature -> IO ReportsFeature
initBuildReportsFeature env _ = do
    let store = serverBlobStore env
        resources = ReportsResource
          { reportsList = (resourceAt "/package/:package/reports/.:format") {
                            resourceGet =  [("txt", textPackageReports)],
                            resourcePost = [("",    submitBuildReport resources store)]
                          }
          , reportsPage = (resourceAt "/package/:package/reports/:id.:format") {
                            resourceGet    = [("txt", textPackageReport)],
                            resourceDelete = [("",    deleteBuildReport resources)]
                          }
          , reportsLog  = (resourceAt "/package/:package/reports/:id/log") {
                            resourceGet    = [("txt", serveBuildLog store)],
                            resourceDelete = [("",    deleteBuildLog resources)],
                            resourcePut    = [("",    putBuildLog resources store)]
                          }

          , reportsListUri = \format pkgid -> renderResource (reportsList resources) [display pkgid, format]
          , reportsPageUri = \format pkgid repid -> renderResource (reportsPage resources) [display pkgid, display repid, format]
          , reportsLogUri  = \pkgid repid -> renderResource (reportsLog resources) [display pkgid, display repid]
          }
    return ReportsFeature {
        featureInterface = (emptyHackageFeature "packages") {
          featureResources   = map ($ resources) [reportsList, reportsPage, reportsLog],
          featureDumpRestore = Just (dumpBackup store, restoreBackup store)
        }
      , reportsResource = resources
      }
  where
    textPackageReports dpath =
      runServerPartE $
      withPackageVersionPath dpath $ \pkg -> do
        reportList <- query $ LookupPackageReports (packageId pkg)
        return . toResponse $ show reportList
    textPackageReport dpath =
      runServerPartE $
      withPackageVersionPath dpath $ \pkg ->
      withPackageReport dpath (packageId pkg) $ \reportId (report, mlog) ->
        return . toResponse $ unlines [ "Report #" ++ display reportId, show report
                                      , maybe "No build log" (const "Build log exists") mlog]

-- result: not-found error or text file
serveBuildLog :: BlobStorage -> DynamicPath -> ServerPart Response
serveBuildLog store dpath =
  runServerPartE $
  withPackageVersionPath dpath $ \pkg -> 
  withPackageReport dpath (pkgInfoId pkg) $ \repid (_, mlog) -> case mlog of
    Nothing -> errNotFound "Log not found" [MText $ "Build log for report " ++ display repid ++ " not found"]
    Just (BuildLog blobId) -> do
        file <- liftIO $ BlobStorage.fetch store blobId
        return . toResponse $ Resource.BuildLog file

-- result: auth error, not-found error, parse error, or redirect
submitBuildReport :: ReportsResource -> BlobStorage -> DynamicPath -> ServerPart Response
submitBuildReport r store dpath =
  runServerPartE $
  withPackageVersionPath dpath $ \pkg -> do
    users <- query GetUserDb
    -- require logged-in user
    Auth.withHackageAuth users Nothing $ \_ _ -> do
        let pkgid = pkgInfoId pkg
        Body reportbody <- consumeRequestBody
        case BuildReport.parse $ unpack reportbody of
            Left err -> errBadRequest "Error submitting report" [MText err]
            Right report -> do
                reportId <- update $ AddReport pkgid (report, Nothing)
                -- redirect to new reports page
                seeOther (reportsPageUri r "" pkgid reportId) $ toResponse ()

-- result: auth error, not-found error or redirect
deleteBuildReport :: ReportsResource -> DynamicPath -> ServerPart Response
deleteBuildReport r dpath =
  runServerPartE $
  withPackageVersionPath dpath $ \pkg ->
  withReportId dpath $ \reportId -> do
    users <- query GetUserDb
    -- restrict this to whom? currently logged in users.. a bad idea
    Auth.withHackageAuth users Nothing $ \_ _ -> do
        let pkgid = pkgInfoId pkg
        success <- update $ DeleteReport pkgid reportId
        if success
            then seeOther (reportsListUri r "" pkgid) $ toResponse ()
            else errNotFound "Build report not found" [MText $ "Build report #" ++ display reportId ++ " not found"]

-- result: auth error, not-found error, or redirect
putBuildLog :: ReportsResource -> BlobStorage -> DynamicPath -> ServerPart Response
putBuildLog r store dpath =
  runServerPartE $
  withPackageVersionPath dpath $ \pkg ->
  withReportId dpath $ \reportId -> do
    users <- query GetUserDb
    -- logged in users
    Auth.withHackageAuth users Nothing $ \_ _ -> do
        let pkgid = pkgInfoId pkg
        Body blogbody <- consumeRequestBody
        buildLog <- liftIO $ BlobStorage.add store blogbody
        update $ SetBuildLog pkgid reportId (Just $ BuildLog buildLog)
        -- go to report page (linking the log)
        seeOther (reportsPageUri r "" pkgid reportId) $ toResponse ()

-- result: auth error, not-found error or redirect
deleteBuildLog :: ReportsResource -> DynamicPath -> ServerPart Response
deleteBuildLog r dpath =
  runServerPartE $
  withPackageVersionPath dpath $ \pkg ->
  withReportId dpath $ \reportId -> do
    users <- query GetUserDb
    -- again, restrict this to whom?
    Auth.withHackageAuth users Nothing $ \_ _ -> do
        let pkgid = pkgInfoId pkg
        update $ SetBuildLog pkgid reportId Nothing
        -- go to report page (which should no longer link the log)
        seeOther (reportsPageUri r "" pkgid reportId) $ toResponse ()

---------------------------------------------------------------------------

withReportId :: Monad m => DynamicPath -> (BuildReportId -> ServerPartT m a) -> ServerPartT m a
withReportId dpath func =
  case simpleParse =<< lookup "id" dpath of
    Nothing -> mzero
    Just reportId -> func reportId

withPackageReport :: DynamicPath -> PackageId -> (BuildReportId -> (BuildReport, Maybe BuildLog) -> ServerPartE a) -> ServerPartE a
withPackageReport dpath pkgid func =
  withReportId dpath $ \reportId -> do
    mreport <- query $ LookupReport pkgid reportId
    case mreport of
        Nothing -> errNotFound "Report not found" [MText "Build report does not exist"]
        Just report -> func reportId report

