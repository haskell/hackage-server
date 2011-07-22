{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Distribution.Server.Auth.Types where

import Data.Binary (Binary)
import Control.Monad.Error.Class (Error, noMsg)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)

newtype PasswdPlain = PasswdPlain String
  deriving (Eq, Ord, Show, Binary, Typeable)

newtype PasswdHash  = PasswdHash  String
  deriving (Eq, Ord, Show, Binary, Typeable)

data AuthType = BasicAuth | DigestAuth
  deriving (Show, Enum, Eq, Typeable)

data AuthError = NoAuthError | UnrecognizedAuthError | NoSuchUserError
               | PasswordMismatchError | AuthTypeMismatchError
  deriving (Enum, Eq, Show, Typeable)

$(deriveSafeCopy 0 'base ''AuthType)
$(deriveSafeCopy 0 'base ''PasswdPlain)
$(deriveSafeCopy 0 'base ''PasswdHash)

instance Error AuthError where
    noMsg = NoAuthError
