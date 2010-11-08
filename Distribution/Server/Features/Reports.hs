module Distribution.Server.Features.Reports (
    ReportsFeature(..),
    ReportsResource(..),
    initReportsFeature
  ) where

import Distribution.Server.Feature
import Distribution.Server.Resource
import Distribution.Server.Features.Core
import Distribution.Server.Types
import Distribution.Server.Error

import Distribution.Server.BuildReport.Backup
import Distribution.Server.BuildReport.State
import qualified Distribution.Server.BuildReport.BuildReport as BuildReport
import Distribution.Server.BuildReport.BuildReport (BuildReport(..))
import Distribution.Server.BuildReport.BuildReports (BuildReportId(..), BuildLog(..))
import Distribution.Server.Users.State (GetUserDb(..))
import qualified Distribution.Server.ResourceTypes as Resource
import qualified Distribution.Server.Auth.Basic as Auth

import Distribution.Server.Packages.Types
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import Distribution.Server.Backup.Export
import Distribution.Server.Util.BlobStorage (BlobStorage)

import Distribution.Text
import Distribution.Package

import Happstack.Server
import Happstack.State (update, query)
import Data.Function (fix)
import Control.Applicative (optional)
import Control.Monad.Trans
import Control.Monad (mzero)
import Data.ByteString.Lazy.Char8 (unpack)

-- TODO: 
-- 1. Put the HTML view for this module in the HTML feature; get rid of the text view
-- 2. Decide build report upload policy (anonymous and authenticated)
data ReportsFeature = ReportsFeature {
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

instance HackageFeature ReportsFeature where
    getFeature reports = HackageModule
      { featureName = "packages"
      , resources   = map ($reportsResource reports) [reportsList, reportsPage, reportsLog]
      , dumpBackup    = Just $ \storage -> do
            buildReps <- query GetBuildReports
            exports <- readExportBlobs storage (buildReportsToExport buildReps)
            return exports
      , restoreBackup = Just $ \storage -> reportsBackup storage
      }

initReportsFeature :: Config -> CoreFeature -> IO ReportsFeature
initReportsFeature config _ = do
    let store = serverStore config
    return ReportsFeature
      { reportsResource = fix $ \r -> ReportsResource
          { reportsList = (resourceAt "/package/:package/reports/.:format") { resourceGet = [("txt", textPackageReports)], resourcePost = [("", textResponse . submitBuildReport r store)] }
          , reportsPage = (resourceAt "/package/:package/reports/:id.:format") { resourceGet = [("txt", textPackageReport)], resourceDelete = [("", textResponse . deleteBuildReport r)] }
          , reportsLog  = (resourceAt "/package/:package/reports/:id/log") { resourceGet = [("txt", textResponse . serveBuildLog store)], resourceDelete = [("", textResponse . deleteBuildLog r)], resourcePut = [("", textResponse . putBuildLog r store)] }

          , reportsListUri = \format pkgid -> renderResource (reportsList r) [display pkgid, format]
          , reportsPageUri = \format pkgid repid -> renderResource (reportsPage r) [display pkgid, display repid, format]
          , reportsLogUri  = \pkgid repid -> renderResource (reportsLog r) [display pkgid, display repid]
          }
      }
  where
    textPackageReports dpath = textResponse $
                               withPackageVersionPath dpath $ \pkg -> do
        reportList <- query $ LookupPackageReports (packageId pkg)
        returnOk . toResponse $ show reportList
    textPackageReport dpath = textResponse $
                              withPackageVersionPath dpath $ \pkg ->
                              withPackageReport dpath (packageId pkg) $ \reportId (report, mlog) -> do
        returnOk . toResponse $ unlines ["Report #" ++ display reportId, show report, maybe "No build log" (const "Build log exists") mlog]

-- result: not-found error or text file
serveBuildLog :: BlobStorage -> DynamicPath -> MServerPart Response
serveBuildLog store dpath = withPackageVersionPath dpath $ \pkg -> withPackageReport dpath (pkgInfoId pkg) $ \repid (_, mlog) -> case mlog of
    Nothing -> returnError 404 "Log not found" [MText $ "Build log for report " ++ display repid ++ " not found"]
    Just (BuildLog blobId) -> do
        file <- liftIO $ BlobStorage.fetch store blobId
        returnOk . toResponse $ Resource.BuildLog file

-- result: auth error, not-found error, parse error, or redirect
submitBuildReport :: ReportsResource -> BlobStorage -> DynamicPath -> MServerPart Response
submitBuildReport r store dpath = withPackageVersionPath dpath $ \pkg -> do
    users <- query GetUserDb
    -- require logged-in user
    Auth.withHackageAuth users Nothing Nothing $ \_ _ -> do
        let pkgid = pkgInfoId pkg
        Body body <- rqBody `fmap` askRq
        case BuildReport.parse $ unpack body of
            Left err -> returnError 400 "Error submitting report" [MText err]
            Right report -> do
                reportId <- update $ AddReport pkgid (report, Nothing)
                -- redirect to new reports page
                fmap Right $ seeOther (reportsPageUri r "" pkgid reportId) $ toResponse ()

-- result: auth error, not-found error or redirect
deleteBuildReport :: ReportsResource -> DynamicPath -> MServerPart Response
deleteBuildReport r dpath = withPackageVersionPath dpath $ \pkg -> withReportId dpath $ \reportId -> do
    users <- query GetUserDb
    -- restrict this to whom? currently logged in users.. a bad idea
    Auth.withHackageAuth users Nothing Nothing $ \_ _ -> do
        let pkgid = pkgInfoId pkg
        success <- update $ DeleteReport pkgid reportId
        if success
            then fmap Right $ seeOther (reportsListUri r "" pkgid) $ toResponse ()
            else returnError 404 "Build report not found" [MText $ "Build report #" ++ display reportId ++ " not found"]

-- result: auth error, not-found error, or redirect
putBuildLog :: ReportsResource -> BlobStorage -> DynamicPath -> MServerPart Response
putBuildLog r store dpath = withPackageVersionPath dpath $ \pkg -> withReportId dpath $ \reportId -> do
    users <- query GetUserDb
    -- logged in users
    Auth.withHackageAuth users Nothing Nothing $ \_ _ -> do
        let pkgid = pkgInfoId pkg
        mRqBody <- takeRequestBody =<< askRq
        case mRqBody of
          Nothing -> returnError 500 "Missing body" [MText "putBuildLog could not be completed because the request body was already consumed."]
          (Just (Body blog)) -> do
                       buildLog <- liftIO $ BlobStorage.add store blog
                       update $ SetBuildLog pkgid reportId (Just $ BuildLog buildLog)
                       -- go to report page (linking the log)
                       fmap Right $ seeOther (reportsPageUri r "" pkgid reportId) $ toResponse ()

-- result: auth error, not-found error or redirect
deleteBuildLog :: ReportsResource -> DynamicPath -> MServerPart Response
deleteBuildLog r dpath = withPackageVersionPath dpath $ \pkg -> withReportId dpath $ \reportId -> do
    users <- query GetUserDb
    -- again, restrict this to whom?
    Auth.withHackageAuth users Nothing Nothing $ \_ _ -> do
        let pkgid = pkgInfoId pkg
        update $ SetBuildLog pkgid reportId Nothing
        -- go to report page (which should no longer link the log)
        fmap Right $ seeOther (reportsPageUri r "" pkgid reportId) $ toResponse ()

---------------------------------------------------------------------------

withReportId :: DynamicPath -> (BuildReportId -> ServerPart a) -> ServerPart a
withReportId dpath func = case simpleParse =<< lookup "id" dpath of
    Nothing -> mzero
    Just reportId -> func reportId

withPackageReport :: DynamicPath -> PackageId -> (BuildReportId -> (BuildReport, Maybe BuildLog) -> MServerPart a) -> MServerPart a
withPackageReport dpath pkgid func = withReportId dpath $ \reportId -> do
    mreport <- query $ LookupReport pkgid reportId
    case mreport of
        Nothing -> returnError 404 "Report not found" [MText "Build report does not exist"]
        Just report -> func reportId report

