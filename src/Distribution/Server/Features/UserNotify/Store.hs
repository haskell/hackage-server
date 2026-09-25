{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.UserNotify.Store
  ( Backend(..)
  , Store(..)
  ) where

import qualified Distribution.Server.Features.UserNotify.Acid as Acid
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
    lookupNotifyPref :: forall m. MonadIO m => UserId -> m (Maybe Acid.NotifyPref)
  , addNotifyPref :: forall m. MonadIO m => UserId -> Acid.NotifyPref -> m ()
  , getNotificationData :: forall m. MonadIO m => m (Map UserId Acid.NotifyPref, UTCTime)
  , setNotifyTime :: forall m. MonadIO m => UTCTime -> m ()
  }
