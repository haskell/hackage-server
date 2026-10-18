{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.PreferredVersions.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Package (PackageName)
import Distribution.Server.Features.PreferredVersions.State (PreferredInfo)
import Distribution.Server.Framework (AbstractStateComponent)
import Distribution.Version (Version, VersionRange)

import Control.Monad.Trans (MonadIO)
import Data.Map (Map)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    getPreferredInfo :: forall m. MonadIO m => PackageName -> m PreferredInfo
  , getDeprecatedFor :: forall m. MonadIO m => PackageName -> m (Maybe [PackageName])
  , getAllPreferredInfo :: forall m. MonadIO m => m (Map PackageName PreferredInfo)
  , getAllDeprecated :: forall m. MonadIO m => m (Map PackageName [PackageName])
  , isMigratedEphemeralPrefs :: forall m. MonadIO m => m Bool
  , setDeprecatedFor :: forall m. MonadIO m => PackageName -> Maybe [PackageName] -> m ()
  , setPreferredInfo :: forall m. MonadIO m => PackageName -> [VersionRange] -> [Version] -> m PreferredInfo
  , setMigratedEphemeralPrefs :: forall m. MonadIO m => m ()
  }
