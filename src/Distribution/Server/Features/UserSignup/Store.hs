{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.UserSignup.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Features.UserSignup.Types (SignupResetInfo)
import Distribution.Server.Framework (AbstractStateComponent)
import Distribution.Server.Util.Nonce (Nonce)

import Control.Monad.Trans (MonadIO)
import Data.Time (UTCTime)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    getSignupResetInfos       :: forall m. MonadIO m => m [SignupResetInfo]
  , lookupSignupResetInfo    :: forall m. MonadIO m => Nonce -> m (Maybe SignupResetInfo)
  , addSignupResetInfo       :: forall m. MonadIO m => Nonce -> SignupResetInfo -> m Bool
  , deleteSignupResetInfo    :: forall m. MonadIO m => Nonce -> m ()
  , deleteExpiredResetInfos  :: forall m. MonadIO m => UTCTime -> m ()
  }
