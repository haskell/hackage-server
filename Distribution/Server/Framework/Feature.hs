-- | This module defines a plugin interface for hackage features.
--
module Distribution.Server.Framework.Feature where

import Distribution.Server.Framework.BackupRestore (RestoreBackup, BackupEntry)
import Distribution.Server.Framework.Resource      (Resource)

-- | We compose the overall hackage server featureset from a bunch of these
-- features. The intention is to make the hackage server reasonably modular
-- by allowing distinct features to be designed independently.
--
-- Features can hold their own canonical state and caches, and can provide a
-- set of resources.
--
data HackageFeature = HackageFeature {
    featureName        :: String,
    featureResources   :: [Resource],

    featurePostInit    :: IO (),

    dumpBackup    :: Maybe (IO [BackupEntry]),
    restoreBackup :: Maybe (RestoreBackup)
}

-- | A feature with no state and no resources, just a name.
-- 
-- Define your new feature by extending this one, e.g.
--
-- > myHackageFeature = emptyHackageFeature "wizzo" {
-- >     featureResources = [wizzo]
-- >   }
--
emptyHackageFeature :: String -> HackageFeature
emptyHackageFeature name = HackageFeature {
    featureName        = name,
    featureResources   = [],

    featurePostInit    = return (),

    dumpBackup    = Nothing,
    restoreBackup = Nothing
  }

class IsHackageFeature feature where
  getFeatureInterface :: feature -> HackageFeature
