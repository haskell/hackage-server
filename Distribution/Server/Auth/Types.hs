{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Distribution.Server.Auth.Types where

import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import Control.Monad.Error.Class (Error, noMsg)

newtype PasswdPlain = PasswdPlain String
  deriving (Eq, Ord, Show, Binary, Typeable)

newtype PasswdHash  = PasswdHash  String
  deriving (Eq, Ord, Show, Binary, Typeable)

data AuthType = BasicAuth | DigestAuth
  deriving (Show, Enum, Eq, Typeable)

data AuthError = NoAuthError | UnrecognizedAuthError | NoSuchUserError
               | PasswordMismatchError | AuthTypeMismatchError
  deriving (Enum, Eq, Show, Typeable)

instance Binary AuthType where
  put t = Binary.put (t == DigestAuth)
  get   = fmap (\b -> if b then DigestAuth else BasicAuth) Binary.get

instance Error AuthError where
    noMsg = NoAuthError

