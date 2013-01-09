{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.BuildReports (
    ReportsFeature(..),
    ReportsResource(..),
    initBuildReportsFeature
  ) where

import Distribution.Server.Framework hiding (BuildLog)

import Distribution.Server.Features.Users
import Distribution.Server.Features.Core

import Distribution.Server.Features.BuildReports.Backup
import Distribution.Server.Features.BuildReports.State
import qualified Distribution.Server.Features.BuildReports.BuildReport as BuildReport
import Distribution.Server.Features.BuildReports.BuildReport (BuildReport(..))
import Distribution.Server.Features.BuildReports.BuildReports (BuildReports, BuildReportId(..), BuildLog(..))
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource

import Distribution.Server.Packages.Types
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage

import Distribution.Text
import Distribution.Package

import Data.ByteString.Lazy.Char8 (unpack)


-- TODO:
-- 1. Put the HTML view for this module in the HTML feature; get rid of the text view
-- 2. Decide build report upload policy (anonymous and authenticated)
data ReportsFeature = ReportsFeature {
    reportsFeatureInterface :: HackageFeature,
    reportsResource :: ReportsResource
}

instance IsHackageFeature ReportsFeature where
    getFeatureInterface = reportsFeatureInterface


data ReportsResource = ReportsResource {
    reportsList :: Resource,
    reportsPage :: Resource,
    reportsLog  :: Resource,
    reportsListUri :: String -> PackageId -> String,
    reportsPageUri :: String -> PackageId -> BuildReportId -> String,
    reportsLogUri  :: PackageId -> BuildReportId -> String
}


initBuildReportsFeature :: ServerEnv -> UserFeature -> CoreFeature -> IO ReportsFeature
initBuildReportsFeature env@ServerEnv{serverStateDir} user core = do
    reportsState <- reportsStateComponent serverStateDir
    return $ buildReportsFeature env user core reportsState

reportsStateComponent :: FilePath -> IO (StateComponent BuildReports)
reportsStateComponent stateDir = do
  st  <- openLocalStateFrom (stateDir </> "db" </> "BuildReports") initialBuildReports
  return StateComponent {
      stateDesc    = "Build reports"
    , acidState    = st
    , getState     = query st GetBuildReports
    , putState     = update st . ReplaceBuildReports
    , backupState  = dumpBackup
    , restoreState = restoreBackup
    , resetState   = reportsStateComponent
    , getStateSize = memSize <$> query st GetBuildReports
    }

buildReportsFeature :: ServerEnv
                    -> UserFeature
                    -> CoreFeature
                    -> StateComponent BuildReports
                    -> ReportsFeature
buildReportsFeature ServerEnv{serverBlobStore = store}
                    UserFeature{..} CoreFeature{..}
                    reportsState
  = ReportsFeature{..}
  where
    reportsFeatureInterface = (emptyHackageFeature "reports") {
        featureResources =
          map ($ reportsResource) [
              reportsList
            , reportsPage
            , reportsLog
            ]
      , featureState = [abstractStateComponent reportsState]
      }

    reportsResource = ReportsResource
          { reportsList = (resourceAt "/package/:package/reports/.:format") {
                            resourceGet =  [("txt", textPackageReports)],
                            resourcePost = [("",    submitBuildReport)]
                          }
          , reportsPage = (resourceAt "/package/:package/reports/:id.:format") {
                            resourceGet    = [("txt", textPackageReport)],
                            resourceDelete = [("",    deleteBuildReport)]
                          }
          , reportsLog  = (resourceAt "/package/:package/reports/:id/log") {
                            resourceGet    = [("txt", serveBuildLog)],
                            resourceDelete = [("",    deleteBuildLog)],
                            resourcePut    = [("",    putBuildLog)]
                          }

          , reportsListUri = \format pkgid -> renderResource (reportsList reportsResource) [display pkgid, format]
          , reportsPageUri = \format pkgid repid -> renderResource (reportsPage reportsResource) [display pkgid, display repid, format]
          , reportsLogUri  = \pkgid repid -> renderResource (reportsLog reportsResource) [display pkgid, display repid]
          }

    textPackageReports dpath =
      runServerPartE $
      withPackageVersionPath dpath $ \pkg -> do
        reportList <- queryState reportsState $ LookupPackageReports (packageId pkg)
        return . toResponse $ show reportList

    textPackageReport dpath =
      runServerPartE $
      withPackageVersionPath dpath $ \pkg ->
      withPackageReport dpath (packageId pkg) $ \reportId (report, mlog) ->
        return . toResponse $ unlines [ "Report #" ++ display reportId, show report
                                      , maybe "No build log" (const "Build log exists") mlog]

    -- result: not-found error or text file
    serveBuildLog :: DynamicPath -> ServerPart Response
    serveBuildLog dpath =
      runServerPartE $
      withPackageVersionPath dpath $ \pkg ->
      withPackageReport dpath (pkgInfoId pkg) $ \repid (_, mlog) -> case mlog of
        Nothing -> errNotFound "Log not found" [MText $ "Build log for report " ++ display repid ++ " not found"]
        Just (BuildLog blobId) -> do
            file <- liftIO $ BlobStorage.fetch store blobId
            return . toResponse $ Resource.BuildLog file

    -- result: auth error, not-found error, parse error, or redirect
    submitBuildReport :: DynamicPath -> ServerPart Response
    submitBuildReport dpath =
      runServerPartE $
      withPackageVersionPath dpath $ \pkg -> do
        users <- queryGetUserDb
        -- require logged-in user
        void $ guardAuthenticated hackageRealm users
        let pkgid = pkgInfoId pkg
        reportbody <- expectTextPlain
        case BuildReport.parse $ unpack reportbody of
            Left err -> errBadRequest "Error submitting report" [MText err]
            Right report -> do
                reportId <- updateState reportsState $ AddReport pkgid (report, Nothing)
                -- redirect to new reports page
                seeOther (reportsPageUri reportsResource "" pkgid reportId) $ toResponse ()

    -- result: auth error, not-found error or redirect
    deleteBuildReport :: DynamicPath -> ServerPart Response
    deleteBuildReport dpath =
      runServerPartE $
      withPackageVersionPath dpath $ \pkg ->
      withReportId dpath $ \reportId -> do
        users <- queryGetUserDb
        -- restrict this to whom? currently logged in users.. a bad idea
        void $ guardAuthenticated hackageRealm users
        let pkgid = pkgInfoId pkg
        success <- updateState reportsState $ DeleteReport pkgid reportId
        if success
            then seeOther (reportsListUri reportsResource "" pkgid) $ toResponse ()
            else errNotFound "Build report not found" [MText $ "Build report #" ++ display reportId ++ " not found"]

    -- result: auth error, not-found error, or redirect
    putBuildLog :: DynamicPath -> ServerPart Response
    putBuildLog dpath =
      runServerPartE $
      withPackageVersionPath dpath $ \pkg ->
      withReportId dpath $ \reportId -> do
        users <- queryGetUserDb
        -- logged in users
        void $ guardAuthenticated hackageRealm users
        let pkgid = pkgInfoId pkg
        blogbody <- expectTextPlain
        buildLog <- liftIO $ BlobStorage.add store blogbody
        void $ updateState reportsState $ SetBuildLog pkgid reportId (Just $ BuildLog buildLog)
        -- go to report page (linking the log)
        seeOther (reportsPageUri reportsResource "" pkgid reportId) $ toResponse ()

    -- result: auth error, not-found error or redirect
    deleteBuildLog :: DynamicPath -> ServerPart Response
    deleteBuildLog dpath =
      runServerPartE $
      withPackageVersionPath dpath $ \pkg ->
      withReportId dpath $ \reportId -> do
        users <- queryGetUserDb
        -- again, restrict this to whom?
        void $ guardAuthenticated hackageRealm users
        let pkgid = pkgInfoId pkg
        void $ updateState reportsState $ SetBuildLog pkgid reportId Nothing
        -- go to report page (which should no longer link the log)
        seeOther (reportsPageUri reportsResource "" pkgid reportId) $ toResponse ()

    ---------------------------------------------------------------------------

    withReportId :: Monad m => DynamicPath -> (BuildReportId -> ServerPartT m a) -> ServerPartT m a
    withReportId dpath func =
      case simpleParse =<< lookup "id" dpath of
        Nothing -> mzero
        Just reportId -> func reportId

    withPackageReport :: DynamicPath -> PackageId -> (BuildReportId -> (BuildReport, Maybe BuildLog) -> ServerPartE a) -> ServerPartE a
    withPackageReport dpath pkgid func =
      withReportId dpath $ \reportId -> do
        mreport <- queryState reportsState $ LookupReport pkgid reportId
        case mreport of
            Nothing -> errNotFound "Report not found" [MText "Build report does not exist"]
            Just report -> func reportId report

