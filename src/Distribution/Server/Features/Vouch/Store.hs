{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.Vouch.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Framework (AbstractStateComponent)
import Distribution.Server.Users.Types (UserId)

import Control.Monad.Trans (MonadIO)
import Data.Time (UTCTime)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    getVouchesFor                      :: forall m. MonadIO m => UserId -> m [(UserId, UTCTime)]
  , addVouch                           :: forall m. MonadIO m => UserId -> (UserId, UTCTime) -> m ()
  , queueVouchCompleteNotification     :: forall m. MonadIO m => UserId -> m ()
  , drainQueuedNotifications           :: forall m. MonadIO m => m [UserId]
  }
