{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Distribution.Server.Users.Types (
    module Distribution.Server.Users.Types,
    module Distribution.Server.Framework.AuthTypes
  ) where

import Distribution.Server.Framework.AuthTypes
import Distribution.Server.Framework.MemSize

import Distribution.Text
         ( Text(..) )
import qualified Distribution.Server.Util.Parse as Parse
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint          as Disp
import qualified Data.Char as Char

import Control.Applicative ((<$>))
import Data.Aeson
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)
import GHC.Generics


newtype UserId = UserId Int
  deriving (Eq, Ord, Read, Show, Typeable, MemSize, ToJSON, FromJSON)

newtype UserName  = UserName String
  deriving (Eq, Ord, Read, Show, Typeable, MemSize, ToJSON, FromJSON)

data UserInfo = UserInfo {
                  userName   :: !UserName,
                  userStatus :: !UserStatus
                } deriving (Eq, Show, Typeable, Generic)

instance ToJSON UserInfo where
instance FromJSON UserInfo where

data UserStatus = AccountEnabled  UserAuth
                | AccountDisabled (Maybe UserAuth)
                | AccountDeleted
    deriving (Eq, Show, Typeable, Generic)

-- This ToJSON instance ignores the UserAuth component
-- so that password hashes are harder to leak
instance ToJSON UserStatus where
  toJSON (AccountEnabled _)  = String "AccountEnabled"
  toJSON (AccountDisabled _) = String "AccountDisabled"
  toJSON AccountDeleted      = String "AccountDeleted"

emptyAuth :: UserAuth
emptyAuth = UserAuth (PasswdHash "")

instance FromJSON UserStatus where
  parseJSON (String "AccountEnabled")  =
    return $ AccountEnabled emptyAuth
  parseJSON (String "AccountDisabled") =
    return $ AccountDisabled $ Just emptyAuth
  parseJSON (String "AccountDeleted")  =
    return $ AccountDeleted


newtype UserAuth = UserAuth PasswdHash
    deriving (Show, Eq, Typeable)


isActiveAccount :: UserStatus -> Bool
isActiveAccount (AccountEnabled  _) = True
isActiveAccount (AccountDisabled _) = True
isActiveAccount  AccountDeleted     = False

instance MemSize UserInfo where
    memSize (UserInfo a b) = memSize2 a b

instance MemSize UserStatus where
    memSize (AccountEnabled  a) = memSize1 a
    memSize (AccountDisabled a) = memSize1 a
    memSize (AccountDeleted)    = memSize0

instance MemSize UserAuth where
    memSize (UserAuth a) = memSize1 a


instance Text UserId where
    disp (UserId uid) = Disp.int uid
    parse = UserId <$> Parse.int

instance Text UserName where
    disp (UserName name) = Disp.text name
    parse = UserName <$> Parse.munch1 isValidUserNameChar

isValidUserNameChar :: Char -> Bool
isValidUserNameChar c = (c < '\127' && Char.isAlphaNum c) || (c == '_')

$(deriveSafeCopy 0 'base ''UserId)
$(deriveSafeCopy 0 'base ''UserName)
$(deriveSafeCopy 1 'base ''UserAuth)
$(deriveSafeCopy 0 'base ''UserStatus)
$(deriveSafeCopy 0 'base ''UserInfo)
