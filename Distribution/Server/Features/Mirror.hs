module Distribution.Server.Features.Mirror (
    MirrorFeature(..),
    initMirrorFeature
  ) where

import Distribution.Server.Feature
import Distribution.Server.Features.Core
import Distribution.Server.Resource

data MirrorFeature = MirrorFeature

instance HackageFeature MirrorFeature where
    getFeature _ = HackageModule
      { featureName = "mirror"
      , resources   = []
      , dumpBackup    = return []
      , restoreBackup = Nothing
      }

initMirrorFeature :: CoreFeature -> IO MirrorFeature
initMirrorFeature _ = return MirrorFeature
