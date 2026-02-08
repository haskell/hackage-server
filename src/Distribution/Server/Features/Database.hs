{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Server.Features.Database where

import Data.Int (Int32)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Distribution.Server.Framework
import Distribution.Server.Users.Types (UserId (..))

-- | A feature to store extra information about users like email addresses.
data DatabaseFeature = DatabaseFeature
  { databaseFeatureInterface :: HackageFeature,
    accountDetailsFindByUserId :: forall m. (MonadIO m) => UserId -> m (Maybe AccountDetails)
  }

instance IsHackageFeature DatabaseFeature where
  getFeatureInterface = databaseFeatureInterface

initDatabaseFeature :: ServerEnv -> IO (IO DatabaseFeature)
initDatabaseFeature env = pure $ do
  conn <- open (serverDatabase env)
  pure $ mkDatabaseFeature conn
  where
    mkDatabaseFeature :: Connection -> DatabaseFeature
    mkDatabaseFeature conn = DatabaseFeature {..}
      where
        databaseFeatureInterface =
          (emptyHackageFeature "database")
            { featureDesc = "A feature to store information in a SQL database.",
              featurePostInit = pure (),
              featureState = [] -- CHECK: should probably do a dump of the database here and perform an import somewhere else?
            }

        accountDetailsFindByUserId :: forall m. (MonadIO m) => UserId -> m (Maybe AccountDetails)
        accountDetailsFindByUserId (UserId userId) =
          liftIO $
            runBeamSqlite conn $
              runSelectReturningOne $
                select $
                  filter_ (\ad -> _adUserId ad ==. val_ (fromIntegral userId)) $
                    all_ (_accountDetails hackageDb)

newtype HackageDb f = HackageDb
  {_accountDetails :: f (TableEntity AccountDetailsT)}
  deriving (Generic, Database be)

hackageDb :: DatabaseSettings be HackageDb
hackageDb =
  defaultDbSettings
    `withDbModification` dbModification
      { _accountDetails = setEntityName "account_details"
      }

-- Tables

-- AccountDetails

data AccountDetailsT f
  = AccountDetails
  { _adUserId :: Columnar f Int32, -- CHECK: Can we user Distribution.Server.Users.Types.UserId here instead?
    _adName :: Columnar f Text,
    _adContactEmail :: Columnar f Text,
    _adKind :: Columnar f (Maybe Text), -- NOTE: valid values are real_user, special.
    _adAdminNotes :: Columnar f Text
  }
  deriving (Generic, Beamable)

type AccountDetails = AccountDetailsT Identity

deriving instance Show AccountDetails

deriving instance Eq AccountDetails

type AccountDetailsId = PrimaryKey AccountDetailsT Identity

instance Table AccountDetailsT where
  data PrimaryKey AccountDetailsT f = AccountDetailsId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = AccountDetailsId . _adUserId