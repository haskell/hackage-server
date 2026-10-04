{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.HaskellPlatform.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Framework (AbstractStateComponent)

import Distribution.Package (PackageName)
import Distribution.Version (Version)

import Control.Monad.Trans (MonadIO)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    platformVersions      :: forall m. MonadIO m => PackageName -> m [Version]
  , platformPackageLatest :: forall m. MonadIO m => m [(PackageName, Version)]
  , setPlatform           :: forall m. MonadIO m => PackageName -> [Version] -> m ()
  , removePlatform        :: forall m. MonadIO m => PackageName -> m ()
  }
