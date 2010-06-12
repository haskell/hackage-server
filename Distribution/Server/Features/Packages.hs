module Distribution.Server.Features.Packages (
    PackagesFeature(..),
    initPackagesFeature
  ) where

import Distribution.Server.Feature

data PackagesFeature = PackagesFeature

instance HackageFeature PackagesFeature where
    getFeature _ = HackageModule
      { featureName = "core"
      , resources   = []
      , dumpBackup    = return []
      , restoreBackup = Nothing
      }

initPackagesFeature :: CoreFeature -> IO PackagesFeature
initPackagesFeature _ = return PackagesFeature
