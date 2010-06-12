module Distribution.Server.Features.Json (
    JsonFeature(..),
    initJsonFeature
  ) where

import Distribution.Server.Feature

data JsonFeature = JsonFeature

instance HackageFeature JsonFeature where
    getFeature _ = HackageModule
      { featureName = "core"
      , resources   = []
      , dumpBackup    = return []
      , restoreBackup = Nothing
      }

initJsonFeature :: CoreFeature -> IO JsonFeature
initJsonFeature _ = return JsonFeature
