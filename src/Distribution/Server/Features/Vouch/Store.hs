{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.Vouch.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Features.Vouch.Types (VouchError, VouchSuccess)
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
    -- | Record a vouch by the voucher (second argument) for the vouchee
    -- (first) at the given time, if 'judgeVouchWith' allows it, given
    -- whether the voucher and the vouchee are uploaders. If this completes
    -- the vouchee's vouches, also queue their notification. Returns the
    -- result and the vouchee's vouches from before this one.
  , addVouchIfAllowed                  :: forall m. MonadIO m => UserId -> UserId -> UTCTime -> Bool -> Bool
                                       -> m (Either VouchError (VouchSuccess, [(UserId, UTCTime)]))
    -- | Return the users whose completion notifications are queued, and
    -- empty the queue.
  , drainQueuedNotifications           :: forall m. MonadIO m => m [UserId]
  }
