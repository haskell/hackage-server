{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.Upload.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Framework (AbstractStateComponent)
import Distribution.Server.Users.Group (UserIdSet)

import Distribution.Package (PackageName)
import Distribution.Server.Users.Types (UserId)

import Control.Monad.Trans (MonadIO)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    getTrustees :: forall m. MonadIO m => m UserIdSet
  , addTrustee :: forall m. MonadIO m => UserId -> m ()
  , removeTrustee :: forall m. MonadIO m => UserId -> m ()
  , getUploaders :: forall m. MonadIO m => m UserIdSet
  , addUploader :: forall m. MonadIO m => UserId -> m ()
  , removeUploader :: forall m. MonadIO m => UserId -> m ()
  , getPackageMaintainers :: forall m. MonadIO m => PackageName -> m UserIdSet
  , addPackageMaintainer :: forall m. MonadIO m => PackageName -> UserId -> m ()
  , removePackageMaintainer :: forall m. MonadIO m => PackageName -> UserId -> m ()
  }
