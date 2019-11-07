{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Users.Types (
    module Distribution.Server.Users.Types,
    module Distribution.Server.Users.AuthToken,
    module Distribution.Server.Framework.AuthTypes
  ) where

import Distribution.Server.Framework.AuthTypes
import Distribution.Server.Framework.MemSize
import Distribution.Server.Users.AuthToken

import Distribution.Text
         ( Text(..) )
import qualified Distribution.Server.Util.Parse as Parse
import qualified Text.ParserCombinators.ReadP as Parse
import Distribution.Pretty (Pretty(..))
import Distribution.Parsec (Parsec(..))
import qualified Distribution.Parsec.Class as P
import qualified Distribution.Compat.Parsing as P
import qualified Distribution.Compat.CharParsing as P

import qualified Text.PrettyPrint          as Disp
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L

import Control.Applicative ((<$>))
import Data.Aeson (ToJSON, FromJSON)
import Data.SafeCopy (base, extension, deriveSafeCopy, Migrate(..))
import Data.Typeable (Typeable)
import Data.Hashable


newtype UserId = UserId Int
  deriving (Eq, Ord, Read, Show, Typeable, MemSize, ToJSON, FromJSON, Pretty)

newtype UserName  = UserName String
  deriving (Eq, Ord, Read, Show, Typeable, MemSize, ToJSON, FromJSON, Hashable)

data UserInfo = UserInfo {
                  userName   :: !UserName,
                  userStatus :: !UserStatus,
                  userTokens :: !(M.Map AuthToken T.Text) -- tokens and descriptions
                } deriving (Eq, Show, Typeable)

data UserStatus = AccountEnabled  UserAuth
                | AccountDisabled (Maybe UserAuth)
                | AccountDeleted
    deriving (Eq, Show, Typeable)

newtype UserAuth = UserAuth PasswdHash
    deriving (Show, Eq, Typeable)

isActiveAccount :: UserStatus -> Bool
isActiveAccount (AccountEnabled  _) = True
isActiveAccount (AccountDisabled _) = True
isActiveAccount  AccountDeleted     = False

instance MemSize UserInfo where
    memSize (UserInfo a b c) = memSize3 a b c

instance MemSize UserStatus where
    memSize (AccountEnabled  a) = memSize1 a
    memSize (AccountDisabled a) = memSize1 a
    memSize (AccountDeleted)    = memSize0

instance MemSize UserAuth where
    memSize (UserAuth a) = memSize1 a

-- TODO: remove this instance for Cabal 3.0
instance Text UserId where
    disp (UserId uid) = Disp.int uid
    parse = UserId <$> Parse.int

instance Parsec UserId where
  -- parse a non-negative integer. No redundant leading zeros allowed.
  -- (this is effectively a relabeled versionDigitParser)
  parsec = (P.some d >>= (fmap UserId . toNumber)) P.<?> "UserId (natural number without redunant leading zeroes)"
    where
      toNumber :: P.CabalParsing m => [Int] -> m Int
      toNumber [0]   = return 0
      toNumber (0:_) = P.unexpected "UserId with redundant leading zero"
      -- TODO: Add sanity check this doesn't overflow
      toNumber xs    = return $ L.foldl' (\a b -> a * 10 + b) 0 xs

      d :: P.CharParsing m => m Int
      d = f <$> P.satisfyRange '0' '9'
      f c = Char.ord c - Char.ord '0'

-- TODO: remove this instance for Cabal 3.0
instance Text UserName where
    disp (UserName name) = Disp.text name
    parse = UserName <$> Parse.munch1 isValidUserNameChar

instance Pretty UserName where
  pretty (UserName name) = Disp.text name

instance Parsec UserName where
  parsec = UserName <$> P.munch1 isValidUserNameChar

isValidUserNameChar :: Char -> Bool
isValidUserNameChar c = (c < '\127' && Char.isAlphaNum c) || (c == '_')

data UserInfo_v0 = UserInfo_v0 {
                  userName_v0   :: !UserName,
                  userStatus_v0 :: !UserStatus
                } deriving (Eq, Show, Typeable)

instance Migrate UserInfo where
    type MigrateFrom UserInfo = UserInfo_v0
    migrate v0 =
        UserInfo
        { userName = userName_v0 v0
        , userStatus = userStatus_v0 v0
        , userTokens = M.empty
        }

$(deriveSafeCopy 0 'base ''UserId)
$(deriveSafeCopy 0 'base ''UserName)
$(deriveSafeCopy 1 'base ''UserAuth)
$(deriveSafeCopy 0 'base ''UserStatus)
$(deriveSafeCopy 0 'base ''UserInfo_v0)
$(deriveSafeCopy 1 'extension ''UserInfo)
