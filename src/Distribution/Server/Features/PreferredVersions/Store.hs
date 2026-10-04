{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.PreferredVersions.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Package (PackageName)
import Distribution.Server.Features.PreferredVersions.State (PreferredInfo, PreferredVersions)
import Distribution.Server.Framework (AbstractStateComponent)
import Distribution.Version (Version, VersionRange)

import Control.Monad.Trans (MonadIO)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    getPreferredInfo :: forall m. MonadIO m => PackageName -> m PreferredInfo
  , getDeprecatedFor :: forall m. MonadIO m => PackageName -> m (Maybe [PackageName])
  , getPreferredVersions :: forall m. MonadIO m => m PreferredVersions
  , setDeprecatedFor :: forall m. MonadIO m => PackageName -> Maybe [PackageName] -> m ()
  , setPreferredInfo :: forall m. MonadIO m => PackageName -> [VersionRange] -> [Version] -> m PreferredInfo
  , setMigratedEphemeralPrefs :: forall m. MonadIO m => m ()
  }
