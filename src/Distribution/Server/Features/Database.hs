{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Server.Features.Database where

import Distribution.Server.Framework

-- | A feature to store extra information about users like email addresses.
newtype DatabaseFeature = DatabaseFeature
  { databaseFeatureInterface :: HackageFeature
  }

instance IsHackageFeature DatabaseFeature where
  getFeatureInterface = databaseFeatureInterface

initDatabaseFeature :: ServerEnv -> IO (IO DatabaseFeature)
initDatabaseFeature env =
  pure (pure DatabaseFeature {..})
  where
    databaseFeatureInterface =
      (emptyHackageFeature "database")
        { featureDesc = "A feature to store information in a SQL database.",
          featurePostInit =
            putStrLn ("Database feature initialized using " <> serverDatabase env)
        }