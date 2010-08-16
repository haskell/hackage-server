{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Distribution.Server.Auth.Types where

import Data.Binary (Binary)
import Control.Monad.Error.Class (Error, noMsg)
import Happstack.Data

newtype PasswdPlain = PasswdPlain String
  deriving (Eq, Ord, Show, Binary, Typeable)

newtype PasswdHash  = PasswdHash  String
  deriving (Eq, Ord, Show, Binary, Typeable)

data AuthType = BasicAuth | DigestAuth
  deriving (Show, Enum, Eq, Typeable)

data AuthError = NoAuthError | UnrecognizedAuthError | NoSuchUserError
               | PasswordMismatchError | AuthTypeMismatchError
  deriving (Enum, Eq, Show, Typeable)

instance Version AuthType
instance Version PasswdPlain
instance Version PasswdHash

$(deriveSerialize ''AuthType)
$(deriveSerialize ''PasswdPlain)
$(deriveSerialize ''PasswdHash)

instance Error AuthError where
    noMsg = NoAuthError
