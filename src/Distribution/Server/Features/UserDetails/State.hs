{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Server.Features.UserDetails.State where

import Data.Text (Text)
import Database.Beam
import Distribution.Server.Users.Types

data AccountDetailsT f
  = AccountDetailsRow
  { _adUserId :: Columnar f DBUserId,
    _adName :: Columnar f Text,
    _adContactEmail :: Columnar f Text,
    _adKind :: Columnar f (Maybe Text), -- NOTE: valid values are real_user, special.
    _adAdminNotes :: Columnar f Text
  }
  deriving (Generic, Beamable)

type AccountDetailsRow = AccountDetailsT Identity

deriving instance Show AccountDetailsRow

deriving instance Eq AccountDetailsRow

type AccountDetailsId = PrimaryKey AccountDetailsT Identity

instance Table AccountDetailsT where
  data PrimaryKey AccountDetailsT f = AccountDetailsId (Columnar f DBUserId) deriving (Generic, Beamable)
  primaryKey = AccountDetailsId . _adUserId
