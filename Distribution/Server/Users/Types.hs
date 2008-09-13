{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
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

import Data.Typeable (Typeable)
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import Control.Applicative ((<$>), (<*>), pure)

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

instance Text UserId where
  disp (UserId uid) = Disp.int uid
  parse = UserId <$> Parse.int

instance Text UserName where
  disp (UserName name) = Disp.text name
  parse = UserName <$> Parse.munch1 Char.isAlphaNum

instance Binary AccountStatus where
  put Deleted         = Binary.putWord8 1
  put (Disabled auth) = Binary.putWord8 2 >> Binary.put auth
  put (Enabled  auth) = Binary.putWord8 3 >> Binary.put auth

  get = do
    w <- Binary.getWord8
    case w of
        1 -> pure Deleted
        2 -> Disabled <$> Binary.get
        3 -> Enabled  <$> Binary.get
        _ -> error "decoding AccountStatus"

instance Binary UserInfo where
  put (UserInfo a b) = Binary.put a >> Binary.put b
  get = UserInfo <$> Binary.get <*> Binary.get
