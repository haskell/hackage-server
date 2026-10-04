{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.Distro.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Package (PackageName)
import Distribution.Server.Features.Distro.Types (DistroName, DistroPackageInfo)
import Distribution.Server.Framework (AbstractStateComponent)
import Distribution.Server.Users.Group (UserIdSet)
import Distribution.Server.Users.Types (UserId)

import Control.Monad.Trans (MonadIO)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    queryDistroMaintainers :: forall m. MonadIO m => DistroName -> m UserIdSet
  , addDistroMaintainer :: forall m. MonadIO m => DistroName -> UserId -> m ()
  , removeDistroMaintainer :: forall m. MonadIO m => DistroName -> UserId -> m ()
  , enumerateDistros :: forall m. MonadIO m => m [DistroName]
  , queryPackageStatus :: forall m. MonadIO m => PackageName -> m [(DistroName, DistroPackageInfo)]
  , queryDistroStatus :: forall m. MonadIO m => DistroName -> m [(PackageName, DistroPackageInfo)]
  , isDistribution :: forall m. MonadIO m => DistroName -> m Bool
  , queryDistroPackageStatus :: forall m. MonadIO m => DistroName -> PackageName -> m (Maybe DistroPackageInfo)
  , removeDistro :: forall m. MonadIO m => DistroName -> m ()
  , dropDistroPackage :: forall m. MonadIO m => DistroName -> PackageName -> m ()
  , addDistroPackage :: forall m. MonadIO m => DistroName -> PackageName -> DistroPackageInfo -> m ()
  , addDistro :: forall m. MonadIO m => DistroName -> m Bool
  , putDistroPackageList :: forall m. MonadIO m => DistroName -> [(PackageName, DistroPackageInfo)] -> m ()
  }
