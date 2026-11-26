{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.UserNotify.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Features.UserNotify.Types (NotifyPref)
import Distribution.Server.Framework (AbstractStateComponent)
import Distribution.Server.Users.Types (UserId)

import Control.Monad.Trans (MonadIO)
import Data.Map (Map)
import Data.Time (UTCTime)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    lookupNotifyPref :: forall m. MonadIO m => UserId -> m (Maybe NotifyPref)
  , addNotifyPref :: forall m. MonadIO m => UserId -> NotifyPref -> m ()
  , getNotificationData :: forall m. MonadIO m => m (Map UserId NotifyPref, UTCTime)
  , setNotifyTime :: forall m. MonadIO m => UTCTime -> m ()
  }
