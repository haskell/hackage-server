{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Server.Features.UserDetails.State where

import Data.Text (Text, unpack)
import Database.Beam
import Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax (..), autoSqlValueSyntax)
import Database.Beam.Sqlite (Sqlite)
import Database.Beam.Sqlite.Syntax (SqliteValueSyntax (..))
import Distribution.Server.Users.Types

data AccountDetailsT f
  = AccountDetailsRow
  { _adUserId :: Columnar f UserId,
    _adName :: Columnar f Text,
    _adContactEmail :: Columnar f Text,
    _adKind :: Columnar f (Maybe AccountDetailsKind),
    _adAdminNotes :: Columnar f Text
  }
  deriving (Generic, Beamable)

type AccountDetailsRow = AccountDetailsT Identity

deriving instance Show AccountDetailsRow

deriving instance Eq AccountDetailsRow

type AccountDetailsId = PrimaryKey AccountDetailsT Identity

instance Table AccountDetailsT where
  data PrimaryKey AccountDetailsT f = AccountDetailsId (Columnar f UserId) deriving (Generic, Beamable)
  primaryKey = AccountDetailsId . _adUserId

data AccountDetailsKind = RealUser | Special
  deriving (Eq, Show, Read, Enum, Bounded)

instance HasSqlValueSyntax SqliteValueSyntax AccountDetailsKind where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite AccountDetailsKind where
  fromBackendRow = read . unpack <$> fromBackendRow
