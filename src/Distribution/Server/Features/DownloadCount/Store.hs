{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.DownloadCount.Store
  ( Backend(..)
  , Store(..)
  ) where

import qualified Distribution.Server.Features.DownloadCount.State as State
import Distribution.Server.Framework (AbstractStateComponent)

import Distribution.Package (PackageId)

import Control.Monad.Trans (MonadIO)
import Data.Time.Calendar (Day)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    recordedToday         :: forall m. MonadIO m => m Day
  , getInMemStats         :: forall m. MonadIO m => m State.InMemStats
  , replaceInMemStats     :: forall m. MonadIO m => State.InMemStats -> m ()
  , registerDownload      :: forall m. MonadIO m => PackageId -> m ()
  , checkpointInMemStats  :: forall m. MonadIO m => m ()
  }
