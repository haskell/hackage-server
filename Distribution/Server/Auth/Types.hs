{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Distribution.Server.Auth.Types where

import Data.Binary (Binary)
import Data.Typeable (Typeable)

newtype PasswdPlain = PasswdPlain String
  deriving (Eq, Ord, Show, Binary, Typeable)

newtype PasswdHash  = PasswdHash  String
  deriving (Eq, Ord, Show, Binary, Typeable)
