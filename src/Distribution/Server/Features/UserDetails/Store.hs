{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.UserDetails.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Features.UserDetails.Types
import Distribution.Server.Framework (AbstractStateComponent)
import Distribution.Server.Users.Types (UserId)

import Control.Monad.Trans (MonadIO)
import Data.Text (Text)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    lookupUserDetails  :: forall m. MonadIO m => UserId -> m (Maybe AccountDetails)
  , setUserDetails     :: forall m. MonadIO m => UserId -> AccountDetails -> m ()
  , setUserNameContact :: forall m. MonadIO m => UserId -> Text -> Text -> m ()
  , setUserAdminInfo   :: forall m. MonadIO m => UserId -> Maybe AccountKind -> Text -> m ()
  }
