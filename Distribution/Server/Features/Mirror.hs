module Distribution.Server.Features.Mirror (
    MirrorFeature(..),
    initMirrorFeature
  ) where

import Distribution.Server.Feature

data MirrorFeature = MirrorFeature

instance HackageFeature MirrorFeature where
    getFeature _ = HackageModule
      { featureName = "core"
      , resources   = []
      , dumpBackup    = return []
      , restoreBackup = Nothing
      }

initMirrorFeature :: CoreFeature -> IO MirrorFeature
initMirrorFeature _ = return MirrorFeature
