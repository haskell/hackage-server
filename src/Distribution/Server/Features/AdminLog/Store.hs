{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.AdminLog.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Features.AdminLog.Types (AdminAction)
import Distribution.Server.Framework (AbstractStateComponent)
import Distribution.Server.Users.Types (UserId)

import Control.Monad.Trans (MonadIO)
import Data.Time (UTCTime)
import qualified Data.ByteString.Lazy.Char8 as BS

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    getAdminLog :: forall m. MonadIO m => m [(UTCTime,UserId,AdminAction,BS.ByteString)]
  , addAdminLog :: forall m. MonadIO m => (UTCTime, UserId, AdminAction, BS.ByteString) -> m ()
  }
