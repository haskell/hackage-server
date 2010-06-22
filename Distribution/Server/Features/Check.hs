module Distribution.Server.Features.Check (
    CheckFeature(..),
    initCheckFeature
  ) where

import Distribution.Server.Feature

data CheckFeature = CheckFeature

instance HackageFeature CheckFeature where
    getFeature _ = HackageModule
      { featureName = "check"
      , resources   = []
      , dumpBackup    = Nothing
      , restoreBackup = Nothing
      }

initCheckFeature :: CoreFeature -> IO CheckFeature
initCheckFeature _ = return CheckFeature
