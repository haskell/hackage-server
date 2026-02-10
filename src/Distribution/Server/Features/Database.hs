{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Server.Features.Database
  ( DatabaseFeature (..),
    Transaction,
    initDatabaseFeature,
    runSelectReturningOne,
    runInsert,
    HackageDb (..),
    hackageDb,
    -- for tests
    testDatabaseFeature,
    testDatabaseFeatureIO,
  )
where

import Control.Monad.Reader
import Data.FileEmbed (embedStringFile)
import Data.Kind
import Data.Pool
import Database.Beam hiding (runInsert, runSelectReturningOne)
import qualified Database.Beam
import Database.Beam.Sqlite
import qualified Database.SQLite.Simple
import qualified Database.SQLite3
import Distribution.Server.Features.UserDetails.State
import Distribution.Server.Framework
import Distribution.Server.Users.State

newtype Connection = SqlLiteConnection Database.SQLite.Simple.Connection

runSelectReturningOne :: forall a. (FromBackendRow Sqlite a) => SqlSelect Sqlite a -> Transaction (Maybe a)
runSelectReturningOne q =
  Transaction $ ReaderT $ \(SqlLiteConnection conn) -> runBeamSqlite conn $ Database.Beam.runSelectReturningOne q

runInsert :: forall (table :: (Type -> Type) -> Type). SqlInsert Sqlite table -> Transaction ()
runInsert q =
  Transaction $ ReaderT $ \(SqlLiteConnection conn) -> runBeamSqlite conn $ Database.Beam.runInsert q

newtype Transaction a = Transaction {unTransaction :: ReaderT Connection IO a}
  deriving (Functor, Applicative, Monad)

runTransaction :: Transaction a -> Connection -> IO a
runTransaction t = runReaderT (unTransaction t)

-- | A feature to store extra information about users like email addresses.
data DatabaseFeature = DatabaseFeature
  { databaseFeatureInterface :: HackageFeature,
    withTransaction :: forall a m. (MonadIO m) => Transaction a -> m a,
    -- | whether the database is fresh and feature should migrate acid data
    fresh :: Bool
  }

instance IsHackageFeature DatabaseFeature where
  getFeatureInterface = databaseFeatureInterface

-- | Ensures the database schema is initialized. Returns 'True' if the database was just created, 'False' if it already existed.
initSchema :: Database.SQLite.Simple.Connection -> IO Bool
initSchema conn = do
  [Database.SQLite.Simple.Only tableExists] <-
    Database.SQLite.Simple.query_
      conn
      "SELECT EXISTS(SELECT 1 FROM sqlite_master WHERE type='table' AND name='account_details');"
  Database.SQLite3.exec (Database.SQLite.Simple.connectionHandle conn) $(embedStringFile "init_db.sql")
  pure (not tableExists)

initDatabaseFeature :: ServerEnv -> IO (IO DatabaseFeature)
initDatabaseFeature env = pure $ do
  dbpool <-
    newPool $
      defaultPoolConfig
        (Database.SQLite.Simple.open (serverDatabasePath env))
        Database.SQLite.Simple.close
        (5 {- time in seconds before unused connection is closed -})
        (20 {- number of connections -})

  -- Initialize the database schema.
  -- Script produce no changes if database is already initialized.
  -- TODO: implement migrations
  -- CHECK: Should this be done in featurePostInit instead?
  fresh <- withResource dbpool initSchema

  pure $ mkDatabaseFeature dbpool fresh
  where
    mkDatabaseFeature :: Pool Database.SQLite.Simple.Connection -> Bool -> DatabaseFeature
    mkDatabaseFeature dbpool fresh = DatabaseFeature {..}
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

data HackageDb f = HackageDb
  { _tblAccountDetails :: f (TableEntity AccountDetailsT),
    _tblUsers :: f (TableEntity UsersT)
  }
  deriving stock (Generic)

instance Database be HackageDb

hackageDb :: DatabaseSettings be HackageDb
hackageDb = defaultDbSettings

-- | For testing purposes, in memory single connection database.
testDatabaseFeature ::
  (forall r. IO r -> (r -> IO ()) -> (r -> b) -> b) ->
  (DatabaseFeature -> b) ->
  b
testDatabaseFeature withResourceFn action =
  withResourceFn
    setupTestDatabase
    (\(conn, _) -> Database.SQLite.Simple.close conn)
    (\(_, database) -> action database)

testDatabaseFeatureIO ::
  (forall r. IO r -> (r -> IO ()) -> (IO r -> b) -> b) ->
  (IO DatabaseFeature -> b) ->
  b
testDatabaseFeatureIO withResourceFn action =
  withResourceFn
    setupTestDatabase
    (\(conn, _) -> Database.SQLite.Simple.close conn)
    (\ioResource -> action (snd <$> ioResource))

setupTestDatabase :: IO (Database.SQLite.Simple.Connection, DatabaseFeature)
setupTestDatabase = do
  conn <- Database.SQLite.Simple.open ":memory:"
  fresh <- initSchema conn -- NOTE: Always fresh since it's an in-memory database
  pure
    ( conn,
      DatabaseFeature
        { databaseFeatureInterface = undefined, -- not needed for these tests
          withTransaction = \transaction ->
            liftIO $
              Database.SQLite.Simple.withTransaction conn $
                runTransaction
                  transaction
                  (SqlLiteConnection conn),
          fresh = fresh
        }
    )
