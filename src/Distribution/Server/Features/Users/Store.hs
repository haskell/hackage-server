{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.Users.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Framework (AbstractStateComponent)
import Distribution.Server.Users.Group (UserIdSet)
import Distribution.Server.Users.Types (AuthToken, UserAuth, UserId, UserName)
import Distribution.Server.Users.Users
    ( Users
    , ErrUserNameClash
    , ErrNoSuchUserId
    , ErrDeletedUser
    , ErrTokenNotOwned
    )

import Control.Monad.Trans (MonadIO)
import Data.Text (Text)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    getUsers :: forall m. MonadIO m => m Users
  , addUser :: forall m. MonadIO m => UserName -> UserAuth -> m (Either ErrUserNameClash UserId)
  , addDisabledUser :: forall m. MonadIO m => UserName -> m (Either ErrUserNameClash UserId)
  , setUserEnabledStatus :: forall m. MonadIO m => UserId -> Bool -> m (Maybe (Either ErrNoSuchUserId ErrDeletedUser))
  , deleteUser :: forall m. MonadIO m => UserId -> m (Maybe ErrNoSuchUserId)
  , setUserAuth :: forall m. MonadIO m => UserId -> UserAuth -> m (Maybe (Either ErrNoSuchUserId ErrDeletedUser))
  , addAuthToken :: forall m. MonadIO m => UserId -> AuthToken -> Text -> m (Maybe ErrNoSuchUserId)
  , revokeAuthToken :: forall m. MonadIO m => UserId -> AuthToken -> m (Maybe (Either ErrNoSuchUserId ErrTokenNotOwned))
  , getAdminList :: forall m. MonadIO m => m UserIdSet
  , addAdmin :: forall m. MonadIO m => UserId -> m ()
  , removeAdmin :: forall m. MonadIO m => UserId -> m ()
  }
