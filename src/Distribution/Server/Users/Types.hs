{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Distribution.Server.Users.Types (
    module Distribution.Server.Users.Types,
    module Distribution.Server.Users.AuthToken,
    module Distribution.Server.Framework.AuthTypes
  ) where

import Distribution.Server.Framework.AuthTypes
import Distribution.Server.Framework.MemSize
import Distribution.Server.Users.AuthToken

import Distribution.Pretty (Pretty(..))
import Distribution.Parsec (Parsec(..))
import qualified Distribution.Parsec as P
import qualified Distribution.Compat.Parsing as P
import qualified Distribution.Compat.CharParsing as P

import qualified Text.PrettyPrint          as Disp
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.List as L

import Data.Proxy
import Data.Text (unpack)
import Data.Int
import Data.Aeson (ToJSON, FromJSON)
import Data.SafeCopy (base, extension, deriveSafeCopy, Migrate(..))
import Data.Hashable
import Data.Serialize (Serialize)
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Backend.SQL.SQL92 ()
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Syntax

newtype UserId = UserId Int
  deriving newtype (Eq, Ord, Read, Show, MemSize, ToJSON, FromJSON, Pretty)

-- NOTE: To use UserId directly we need to change it to a non machine-dependent size for Beam
--       We force Beam to treat UserId as a Int32

instance FromBackendRow Sqlite UserId where
  fromBackendRow = UserId . fromIntegral @Int32 <$> fromBackendRow

instance HasSqlValueSyntax SqliteValueSyntax UserId where
  sqlValueSyntax (UserId v) = sqlValueSyntax (fromIntegral v :: Int32)

instance HasSqlEqualityCheck Sqlite UserId where
  sqlEqE _ = sqlEqE (Proxy :: Proxy Int32)
  sqlNeqE _ = sqlNeqE (Proxy :: Proxy Int32)
  sqlEqTriE _ = sqlEqTriE (Proxy :: Proxy Int32)
  sqlNeqTriE _ = sqlNeqTriE (Proxy :: Proxy Int32)

newtype UserName  = UserName String
  deriving newtype (Eq, Ord, Read, Show, MemSize, ToJSON, FromJSON, Hashable, Serialize, HasSqlValueSyntax SqliteValueSyntax)

instance FromBackendRow Sqlite UserName where
  fromBackendRow = UserName . unpack <$> fromBackendRow

data UserInfo = UserInfo {
                  userName   :: !UserName,
                  userStatus :: !UserStatus,
                  userTokens :: !(M.Map AuthToken T.Text) -- tokens and descriptions
                } deriving (Eq, Show)

data UserStatus = AccountEnabled  UserAuth
                | AccountDisabled (Maybe UserAuth)
                | AccountDeleted
    deriving (Eq, Show)

newtype UserAuth = UserAuth PasswdHash
    deriving (Show, Eq)

instance HasSqlValueSyntax SqliteValueSyntax UserAuth where
  sqlValueSyntax (UserAuth (PasswdHash v)) = sqlValueSyntax v

instance FromBackendRow Sqlite UserAuth where
  fromBackendRow = UserAuth . PasswdHash . unpack <$> fromBackendRow

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

instance Pretty UserName where
  pretty (UserName name) = Disp.text name

instance Parsec UserName where
  parsec = UserName <$> P.munch1 isValidUserNameChar

isValidUserNameChar :: Char -> Bool
isValidUserNameChar c = (c < '\127' && Char.isAlphaNum c) || (c == '_')

data UserInfo_v0 = UserInfo_v0 {
                  userName_v0   :: !UserName,
                  userStatus_v0 :: !UserStatus
                } deriving (Eq, Show)

$(deriveSafeCopy 0 'base ''UserId)
$(deriveSafeCopy 0 'base ''UserName)
$(deriveSafeCopy 1 'base ''UserAuth)
$(deriveSafeCopy 0 'base ''UserStatus)
$(deriveSafeCopy 0 'base ''UserInfo_v0)

instance Migrate UserInfo where
    type MigrateFrom UserInfo = UserInfo_v0
    migrate v0 =
        UserInfo
        { userName = userName_v0 v0
        , userStatus = userStatus_v0 v0
        , userTokens = M.empty
        }

$(deriveSafeCopy 1 'extension ''UserInfo)
