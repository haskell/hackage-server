{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.BuildReports.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Package (PackageId)
import Distribution.Server.Features.BuildReports.BuildReport (BuildReport)
import Distribution.Server.Features.BuildReports.BuildReports
  ( BuildReportId, BuildCovg, BuildLog, BuildStatus, TestLog, TestReportLog )
import Distribution.Server.Framework (AbstractStateComponent)

import Control.Monad.Trans (MonadIO)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    lookupReportCovg :: forall m. MonadIO m => PackageId -> BuildReportId -> m (Maybe (BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg, Maybe TestReportLog))
  , lookupPackageReports :: forall m. MonadIO m => PackageId -> m [(BuildReportId, BuildReport)]
  , lookupFailCount :: forall m. MonadIO m => PackageId -> m (Maybe BuildStatus)
  , lookupLatestReport :: forall m. MonadIO m => PackageId -> m (Maybe (BuildReportId, BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg, Maybe TestReportLog))
  , lookupRunTests :: forall m. MonadIO m => PackageId -> m Bool
  , addReport :: forall m. MonadIO m => PackageId -> (BuildReport, Maybe BuildLog) -> m BuildReportId
  , deleteReport :: forall m. MonadIO m => PackageId -> BuildReportId -> m Bool
  , setBuildLog :: forall m. MonadIO m => PackageId -> BuildReportId -> Maybe BuildLog -> m Bool
  , setTestLog :: forall m. MonadIO m => PackageId -> BuildReportId -> Maybe TestLog -> m Bool
  , resetFailCount :: forall m. MonadIO m => PackageId -> m Bool
  , setRunTests :: forall m. MonadIO m => PackageId -> Bool -> m Bool
  , setFailStatus :: forall m. MonadIO m => PackageId -> Bool -> m ()
  , addRptAllLogsCovg :: forall m. MonadIO m => PackageId -> (BuildReport, Maybe BuildLog, Maybe TestLog, Maybe BuildCovg, Maybe TestReportLog) -> m BuildReportId
  }
