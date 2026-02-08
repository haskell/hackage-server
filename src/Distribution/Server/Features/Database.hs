{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Distribution.Server.Features.Database where

import Control.Monad.Reader
import Data.Kind
import Data.Pool
import Database.Beam hiding (runSelectReturningOne)
import qualified Database.Beam
import Database.Beam.Sqlite
import qualified Database.SQLite.Simple
import Distribution.Server.Features.UserDetails.State
import Distribution.Server.Framework

newtype Connection = SqlLiteConnection Database.SQLite.Simple.Connection

runSelectReturningOne :: forall a. (FromBackendRow Sqlite a) => SqlSelect Sqlite a -> Transaction (Maybe a)
runSelectReturningOne q =
  Transaction $ ReaderT $ \(SqlLiteConnection conn) -> runBeamSqlite conn $ Database.Beam.runSelectReturningOne q

runInsert :: forall (table :: (Type -> Type) -> Type). SqlInsert Sqlite table -> Transaction ()
runInsert q =
  Transaction $ ReaderT $ \(SqlLiteConnection conn) -> runBeamSqlite conn $ Database.Beam.runInsert q

newtype Transaction a = Transaction {unTransaction :: ReaderT Connection IO a} -- TODO: don't expose the internals of this
  deriving (Functor, Applicative, Monad)

runTransaction :: Transaction a -> Connection -> IO a
runTransaction (Transaction t) = runReaderT t

-- | A feature to store extra information about users like email addresses.
data DatabaseFeature = DatabaseFeature
  { databaseFeatureInterface :: HackageFeature,
    withTransaction :: forall a m. (MonadIO m) => Transaction a -> m a
  }

instance IsHackageFeature DatabaseFeature where
  getFeatureInterface = databaseFeatureInterface

initDatabaseFeature :: ServerEnv -> IO (IO DatabaseFeature)
initDatabaseFeature env = pure $ do
  dbpool <-
    newPool $
      defaultPoolConfig
        (Database.SQLite.Simple.open (serverDatabase env))
        Database.SQLite.Simple.close
        (5 {- time in seconds before unused connection is closed -})
        (20 {- number of connections -})
  pure $ mkDatabaseFeature dbpool
  where
    mkDatabaseFeature :: Pool Database.SQLite.Simple.Connection -> DatabaseFeature
    mkDatabaseFeature dbpool = DatabaseFeature {..}
      where
        databaseFeatureInterface =
          (emptyHackageFeature "database")
            { featureDesc = "A feature to store information in a SQL database.",
              featurePostInit = pure (),
              featureState = [] -- CHECK: should probably do a dump of the database here and perform an import somewhere else?
            }

        withTransaction :: forall a m. (MonadIO m) => Transaction a -> m a
        withTransaction action =
          liftIO $ withResource dbpool $ \conn ->
            Database.SQLite.Simple.withTransaction conn $
              runTransaction
                action
                (SqlLiteConnection conn)

newtype HackageDb f = HackageDb
  {_accountDetails :: f (TableEntity AccountDetailsT)}
  deriving stock (Generic)

instance Database be HackageDb

hackageDb :: DatabaseSettings be HackageDb
hackageDb =
  defaultDbSettings
    `withDbModification` dbModification
      { _accountDetails = setEntityName "account_details"
      }
