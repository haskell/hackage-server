{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.DownloadCount.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Util.CountingMap (SimpleCountingMap)
import Distribution.Server.Framework (AbstractStateComponent)

import Distribution.Package (PackageId)

import Control.Monad.Trans (MonadIO)
import Data.Time.Calendar (Day)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    -- | Start counting downloads for the given day, unless already counting
    -- for it. If the day changed, returns the previous day and its counts.
    rollOverDay           :: forall m. MonadIO m => Day -> m (Maybe (Day, SimpleCountingMap PackageId))
    -- | Record a batch of downloads: @(package, number of downloads)@ pairs.
  , registerDownloads     :: forall m. MonadIO m => [(PackageId, Int)] -> m ()
  , checkpointInMemStats  :: forall m. MonadIO m => m ()
  }
