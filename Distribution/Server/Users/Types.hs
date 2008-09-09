{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Distribution.Server.Users.Types where

import Distribution.Server.Auth.Types
         ( PasswdHash )

import Data.Typeable (Typeable)
import Data.Binary (Binary)

newtype UserId = UserId Int
  deriving (Eq, Ord, Show, Binary, Typeable)

newtype UserName  = UserName String
  deriving (Eq, Ord, Show, Binary, Typeable)

data UserInfo = UserInfo {
    userName   :: UserName,
    userStatus :: AccountStatus
  }

data AccountStatus = Deleted
                   | Disabled UserAuth
                   | Enabled  UserAuth

type UserAuth = PasswdHash
