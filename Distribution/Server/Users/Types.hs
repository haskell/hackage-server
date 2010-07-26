{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Distribution.Server.Users.Types (
    module Distribution.Server.Users.Types,
    module Distribution.Server.Auth.Types
  ) where

import Distribution.Server.Auth.Types

import Distribution.Text
         ( Text(..) )
import qualified Distribution.Server.Util.Parse as Parse
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint          as Disp
import qualified Data.Char as Char

import Data.Binary (Binary)
import Control.Applicative ((<$>))

import Happstack.Data

newtype UserId = UserId Int
  deriving (Eq, Ord, Show, Binary, Typeable)

newtype UserName  = UserName String
  deriving (Eq, Ord, Show, Binary, Typeable)

data UserInfo = UserInfo {
    userName   :: UserName,
    userStatus :: UserStatus
  } deriving (Show, Typeable)

data UserStatus = Deleted
                | Active !AccountEnabled UserAuth
    deriving (Show, Typeable)
data AccountEnabled = Enabled | Disabled deriving (Show, Enum, Eq, Typeable)

data UserAuth = UserAuth PasswdHash AuthType deriving (Show, Eq, Typeable)

instance Text UserId where
    disp (UserId uid) = Disp.int uid
    parse = UserId <$> Parse.int

instance Text UserName where
    disp (UserName name) = Disp.text name
    parse = UserName <$> Parse.munch1 Char.isAlphaNum

-- the default implementation is version 0
instance Version UserId where
instance Version UserName where
instance Version AccountEnabled where
instance Version UserAuth where
instance Version UserStatus where
instance Version UserInfo where

$(deriveSerialize ''UserId)
$(deriveSerialize ''UserName)
$(deriveSerialize ''AccountEnabled)
$(deriveSerialize ''UserAuth)
$(deriveSerialize ''UserStatus)
$(deriveSerialize ''UserInfo)
