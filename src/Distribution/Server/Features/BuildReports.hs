{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.BuildReports (
    ReportsFeature(..),
    ReportsResource(..),
    initBuildReportsFeature
  ) where

import Distribution.Server.Framework hiding (BuildLog, BuildCovg)

import Distribution.Server.Features.Users
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Core

import Distribution.Server.Features.BuildReports.Backup
import Distribution.Server.Features.BuildReports.State
import qualified Distribution.Server.Features.BuildReports.BuildReport as BuildReport
import Distribution.Server.Features.BuildReports.BuildReport (BuildReport(..))
import Distribution.Server.Features.BuildReports.BuildReports (BuildReports, BuildReportId(..), BuildCovg(..), BuildLog(..))
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource

import Distribution.Server.Packages.Types

import qualified Distribution.Server.Framework.BlobStorage as BlobStorage

import Distribution.Text
import Distribution.Package
import Distribution.Version (nullVersion)

import Control.Arrow (second)
import Data.ByteString.Lazy (toStrict)
import Data.String (fromString)
import Data.Maybe
import Distribution.Compiler ( CompilerId(..) )


-- TODO:
-- 1. Put the HTML view for this module in the HTML feature; get rid of the text view
-- 2. Decide build report upload policy (anonymous and authenticated)
data ReportsFeature = ReportsFeature {
    reportsFeatureInterface :: HackageFeature,

    packageReports :: DynamicPath -> ([(BuildReportId, BuildReport)] -> ServerPartE Response) -> ServerPartE Response,
    packageReport  :: DynamicPath -> ServerPartE (BuildReportId, BuildReport, Maybe BuildLog, Maybe BuildCovg),

    queryPackageReports :: forall m. MonadIO m => PackageId -> m [(BuildReportId, BuildReport)],
    queryBuildLog       :: forall m. MonadIO m => BuildLog  -> m Resource.BuildLog,
    pkgReportDetails    :: forall m. MonadIO m => (PackageIdentifier, Bool) -> m BuildReport.PkgDetails,
    queryLastReportStats:: forall m. MonadIO m => PackageIdentifier -> m (Maybe BuildReport, Maybe BuildCovg),
    reportsResource :: ReportsResource
}

instance IsHackageFeature ReportsFeature where
    getFeatureInterface = reportsFeatureInterface


data ReportsResource = ReportsResource {
    reportsList :: Resource,
    reportsPage :: Resource,
    reportsLog  :: Resource,
    reportsReset:: Resource,
    reportsListUri :: String -> PackageId -> String,
    reportsPageUri :: String -> PackageId -> BuildReportId -> String,
    reportsLogUri  :: PackageId -> BuildReportId -> String
}


initBuildReportsFeature :: String
                        -> ServerEnv
                        -> IO (UserFeature
                            -> UploadFeature
                            -> CoreResource
                            -> IO ReportsFeature)
initBuildReportsFeature name env@ServerEnv{serverStateDir} = do
    reportsState <- reportsStateComponent name serverStateDir

    return $ \user upload core -> do
      let feature = buildReportsFeature name env
                                        user upload core
                                        reportsState
      return feature

reportsStateComponent :: String -> FilePath -> IO (StateComponent AcidState BuildReports)
reportsStateComponent name stateDir = do
  st  <- openLocalStateFrom (stateDir </> "db" </> name) initialBuildReports
  return StateComponent {
      stateDesc    = "Build reports"
    , stateHandle  = st
    , getState     = query st GetBuildReports
    , putState     = update st . ReplaceBuildReports
    , backupState  = \_ -> dumpBackup
    , restoreState = restoreBackup
    , resetState   = reportsStateComponent name
    }

buildReportsFeature :: String
                    -> ServerEnv
                    -> UserFeature
                    -> UploadFeature
                    -> CoreResource
                    -> StateComponent AcidState BuildReports
                    -> ReportsFeature
buildReportsFeature name
                    ServerEnv{serverBlobStore = store}
                    UserFeature{..} UploadFeature{..}
                    CoreResource{ packageInPath
                                , guardValidPackageId
                                , lookupPackageId
                                , corePackagePage
                                }
                    reportsState
  = ReportsFeature{..}
  where
    reportsFeatureInterface = (emptyHackageFeature name) {
        featureDesc = "Build reports and build logs"
      , featureResources =
          map ($ reportsResource) [
              reportsList
            , reportsPage
            , reportsLog
            , reportsReset
            ]
      , featureState = [abstractAcidStateComponent reportsState]
      }

    reportsResource = ReportsResource
          { reportsList = (extendResourcePath "/reports/.:format" corePackagePage) {
                resourceDesc  = [ (GET, "List available build reports")
                                , (POST, "Upload a new build report")
                                , (PUT, "Upload all build files")
                                , (PATCH, "Reset fail count and trigger rebuild")
                                ]
              , resourceGet   = [ ("txt",   textPackageReports) ]
              , resourcePost  = [ ("",      submitBuildReport) ]
              , resourcePut   = [ ("json",    putAllReports) ]
              }

          , reportsReset = (extendResourcePath "/reports/reset/" corePackagePage) {
                resourceDesc  = [ (GET, "Reset fail count and trigger rebuild")
                                 ]
              , resourceGet   = [ ("", resetBuildFails) ]
              }
          , reportsPage = (extendResourcePath "/reports/:id.:format" corePackagePage) {
                resourceDesc   = [ (GET, "Get a specific build report")
                                 , (DELETE, "Delete a specific build report")
                                 ]
              , resourceGet    = [ ("txt", textPackageReport) ]
              , resourceDelete = [ ("",    deleteBuildReport) ]
              }
          , reportsLog  = (extendResourcePath "/reports/:id/log" corePackagePage) {
                resourceDesc   = [ (GET, "Get the build log associated with a build report")
                                 , (DELETE, "Delete a build log")
                                 , (PUT, "Upload a build log for a build report")
                                 ]
              , resourceGet    = [ ("txt", serveBuildLog) ]
              , resourceDelete = [ ("",    deleteBuildLog )]
              , resourcePut    = [ ("",    putBuildLog) ]
              }
          , reportsListUri = \format pkgid -> renderResource (reportsList reportsResource) [display pkgid, format]
          , reportsPageUri = \format pkgid repid -> renderResource (reportsPage reportsResource) [display pkgid, display repid, format]
          , reportsLogUri  = \pkgid repid -> renderResource (reportsLog reportsResource) [display pkgid, display repid]
          }

    ---------------------------------------------------------------------------

    packageReports :: DynamicPath -> ([(BuildReportId, BuildReport)] -> ServerPartE Response) -> ServerPartE Response
    packageReports dpath continue = do
      pkgid <- packageInPath dpath
      if pkgVersion pkgid == nullVersion
        then do
          -- Redirect to the latest version
          pkginfo <- lookupPackageId pkgid
          seeOther (reportsListUri reportsResource "" (pkgInfoId pkginfo)) $
            toResponse ()
        else do
          guardValidPackageId pkgid
          queryPackageReports pkgid >>= continue

    packageReport :: DynamicPath -> ServerPartE (BuildReportId, BuildReport, Maybe BuildLog, Maybe BuildCovg)
    packageReport dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      reportId <- reportIdInPath dpath
      mreport  <- queryState reportsState $ LookupReportCovg pkgid reportId
      case mreport of
        Nothing -> errNotFound "Report not found" [MText "Build report does not exist"]
        Just (report, mlog, covg) -> return (reportId, report, mlog, covg)

    queryPackageReports :: MonadIO m => PackageId -> m [(BuildReportId, BuildReport)]
    queryPackageReports pkgid = do
        reports <- queryState reportsState $ LookupPackageReports pkgid
        return $ map (second fst) reports

    queryBuildLog :: MonadIO m => BuildLog -> m Resource.BuildLog
    queryBuildLog (BuildLog blobId) = do
        file <- liftIO $ BlobStorage.fetch store blobId
        return $ Resource.BuildLog file


    pkgReportDetails :: MonadIO m => (PackageIdentifier, Bool) -> m BuildReport.PkgDetails--(PackageIdentifier, Bool, Maybe (BuildStatus, Maybe UTCTime, Maybe Version))
    pkgReportDetails (pkgid, docs) = do
      failCnt   <- queryState reportsState $ LookupFailCount pkgid
      latestRpt <- queryState reportsState $ LookupLatestReport pkgid
      (time, ghcId) <- case latestRpt of
        Nothing -> return (Nothing,Nothing)
        Just (brp, _, _) -> do
          let (CompilerId _ vrsn) = compiler brp
          return (time brp, Just vrsn)
      return  (BuildReport.PkgDetails pkgid docs failCnt time ghcId)

    queryLastReportStats :: MonadIO m => PackageIdentifier -> m (Maybe BuildReport, Maybe BuildCovg)
    queryLastReportStats pkgid = do
      rpt <- queryState reportsState $ LookupLatestReport pkgid
      case rpt of
        Nothing -> return (Nothing, Nothing)
        Just (a,_,b) -> return (Just a, b)


    ---------------------------------------------------------------------------

    textPackageReports dpath = packageReports dpath $ return . toResponse . show

    textPackageReport dpath = do
      (_, report, _, _) <- packageReport dpath
      return . toResponse $ BuildReport.show report

    -- result: not-found error or text file
    serveBuildLog :: DynamicPath -> ServerPartE Response
    serveBuildLog dpath = do
      (repid, _, mlog, _) <- packageReport dpath
      case mlog of
        Nothing -> errNotFound "Log not found" [MText $ "Build log for report " ++ display repid ++ " not found"]
        Just logId -> do
          cacheControlWithoutETag [Public, maxAgeDays 30]
          toResponse <$> queryBuildLog logId

    -- result: auth error, not-found error, parse error, or redirect
    submitBuildReport :: DynamicPath -> ServerPartE Response
    submitBuildReport dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      guardAuthorised_ [AnyKnownUser] -- allow any logged-in user
      reportbody <- expectTextPlain
      case BuildReport.parse $ toStrict reportbody of
          Left err -> errBadRequest "Error submitting report" [MText err]
          Right report -> do
              when (BuildReport.docBuilder report) $
                  -- Check that the submitter can actually upload docs
                  guardAuthorisedAsMaintainerOrTrustee (packageName pkgid)
              report' <- liftIO $ BuildReport.affixTimestamp report
              reportId <- updateState reportsState $ AddReport pkgid (report', Nothing)
              -- redirect to new reports page
              seeOther (reportsPageUri reportsResource "" pkgid reportId) $ toResponse ()

    {-
      Example using curl:

        curl -u admin:admin \
             -X POST \
             -H "Content-Type: text/plain" \
             --data-binary @reports/nats-0.1 \
             http://localhost:8080/package/nats-0.1/reports/
    -}

    -- result: auth error, not-found error or redirect
    deleteBuildReport :: DynamicPath -> ServerPartE Response
    deleteBuildReport dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      reportId <- reportIdInPath dpath
      guardAuthorised_ [InGroup trusteesGroup]
      success <- updateState reportsState $ DeleteReport pkgid reportId
      if success
          then seeOther (reportsListUri reportsResource "" pkgid) $ toResponse ()
          else errNotFound "Build report not found" [MText $ "Build report #" ++ display reportId ++ " not found"]

    putBuildLog :: DynamicPath -> ServerPartE Response
    putBuildLog dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      reportId <- reportIdInPath dpath
      -- logged in users
      guardAuthorised_ [AnyKnownUser]
      blogbody <- expectTextPlain
      buildLog <- liftIO $ BlobStorage.add store blogbody
      void $ updateState reportsState $ SetBuildLog pkgid reportId (Just $ BuildLog buildLog)
      noContent (toResponse ())

    {-
      Example using curl: (TODO: why is this PUT, while logs are POST?)

        curl -u admin:admin \
             -X PUT \
             -H "Content-Type: text/plain" \
             --data-binary @logs/nats-0.1 \
             http://localhost:8080/package/nats-0.1/reports/1/log
    -}

    deleteBuildLog :: DynamicPath -> ServerPartE Response
    deleteBuildLog dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      reportId <- reportIdInPath dpath
      guardAuthorised_ [InGroup trusteesGroup]
      void $ updateState reportsState $ SetBuildLog pkgid reportId Nothing
      noContent (toResponse ())

    resetBuildFails :: DynamicPath -> ServerPartE Response
    resetBuildFails dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      guardAuthorisedAsMaintainerOrTrustee (packageName pkgid)
      success <- updateState reportsState $ ResetFailCount pkgid
      if success
          then seeOther (reportsListUri reportsResource "" pkgid) $ toResponse ()
          else errNotFound "Report not found" [MText "Build report does not exist"]


    putAllReports :: DynamicPath -> ServerPartE Response
    putAllReports dpath = do
      pkgid <- packageInPath dpath
      guardValidPackageId pkgid
      guardAuthorised_ [AnyKnownUser] -- allow any logged-in user
      buildFiles <- expectAesonContent::ServerPartE BuildReport.BuildFiles
      let reportBody  = BuildReport.reportContent buildFiles
          logBody     = BuildReport.logContent buildFiles
          covgBody    = BuildReport.coverageContent buildFiles
          failStatus  = BuildReport.buildFail buildFiles

      updateState reportsState $ SetFailStatus pkgid failStatus

      -- Upload BuildReport
      case BuildReport.parse $ toStrict $ fromString $ fromMaybe "" reportBody of
          Left err -> errBadRequest "Error submitting report" [MText err]
          Right report -> do
              when (BuildReport.docBuilder report) $
                  -- Check that the submitter can actually upload docs
                  guardAuthorisedAsMaintainerOrTrustee (packageName pkgid)
              report'   <- liftIO $ BuildReport.affixTimestamp report
              logBlob   <- liftIO $ traverse (\x -> BlobStorage.add store $ fromString x) logBody
              reportId  <- updateState reportsState $
                                  AddRptLogCovg pkgid (report', (fmap BuildLog logBlob), (fmap BuildReport.parseCovg covgBody))
              -- redirect to new reports page
              seeOther (reportsPageUri reportsResource "" pkgid reportId) $ toResponse ()

    ---------------------------------------------------------------------------

    reportIdInPath :: MonadPlus m => DynamicPath -> m BuildReportId
    reportIdInPath dpath = maybe mzero return (simpleParse =<< lookup "id" dpath)
