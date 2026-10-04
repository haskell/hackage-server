{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.Mirror.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Framework (AbstractStateComponent)
import Distribution.Server.Users.UserIdSet (UserIdSet)
import Distribution.Server.Users.Types (UserId)

import Control.Monad.Trans (MonadIO)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    getMirrorClientsList :: forall m. MonadIO m => m UserIdSet
  , addMirrorClient      :: forall m. MonadIO m => UserId -> m ()
  , removeMirrorClient   :: forall m. MonadIO m => UserId -> m ()
  }
