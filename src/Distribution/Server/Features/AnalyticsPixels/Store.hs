{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.AnalyticsPixels.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Features.AnalyticsPixels.Types
import Distribution.Server.Framework (AbstractStateComponent)

import Distribution.Package (PackageName)

import Control.Monad.Trans (MonadIO)
import Data.Set (Set)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    getPackageAnalyticsPixels   :: forall m. MonadIO m => PackageName -> m (Set AnalyticsPixel)
  , addPackageAnalyticsPixel    :: forall m. MonadIO m => PackageName -> AnalyticsPixel -> m Bool
  , removePackageAnalyticsPixel :: forall m. MonadIO m => PackageName -> AnalyticsPixel -> m ()
  }
