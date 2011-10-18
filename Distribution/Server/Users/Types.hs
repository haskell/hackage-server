{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Distribution.Server.Users.Types (
    module Distribution.Server.Users.Types,
    module Distribution.Server.Framework.AuthTypes
  ) where

import Distribution.Server.Framework.AuthTypes

import Distribution.Text
         ( Text(..) )
import qualified Distribution.Server.Util.Parse as Parse
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint          as Disp
import qualified Data.Char as Char

import Data.Serialize (Serialize)
import Control.Applicative ((<$>))

import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)

newtype UserId = UserId Int
  deriving (Eq, Ord, Show, Serialize, Typeable)

newtype UserName  = UserName String
  deriving (Eq, Ord, Show, Serialize, Typeable)

data UserInfo = UserInfo {
    userName   :: UserName,
    userStatus :: UserStatus
  } deriving (Eq, Show, Typeable)

data UserStatus = Deleted
                | Historical
                | Active !AccountEnabled UserAuth
    deriving (Eq, Show, Typeable)
data AccountEnabled = Enabled | Disabled deriving (Show, Enum, Eq, Typeable)

isActive, isHistorical :: UserStatus -> Bool

isActive (Active{}) = True
isActive _          = False

isHistorical Historical = True
isHistorical _          = False

data UserAuth = NewUserAuth PasswdHash
              | OldUserAuth HtPasswdHash
              deriving (Show, Eq, Typeable)

instance Text UserId where
    disp (UserId uid) = Disp.int uid
    parse = UserId <$> Parse.int

instance Text UserName where
    disp (UserName name) = Disp.text name
    parse = UserName <$> Parse.munch1 Char.isAlphaNum

$(deriveSafeCopy 0 'base ''UserId)
$(deriveSafeCopy 0 'base ''UserName)
$(deriveSafeCopy 0 'base ''AccountEnabled)
$(deriveSafeCopy 1 'base ''UserAuth)
$(deriveSafeCopy 0 'base ''UserStatus)
$(deriveSafeCopy 0 'base ''UserInfo)
