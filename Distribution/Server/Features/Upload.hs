module Distribution.Server.Features.Upload (
    UploadFeature(..),
    initUploadFeature
  ) where

import Distribution.Server.Feature

data UploadFeature = UploadFeature

instance HackageFeature UploadFeature where
    getFeature _ = HackageModule
      { featureName = "core"
      , resources   = []
      , dumpBackup    = return []
      , restoreBackup = Nothing
      }

initUploadFeature :: CoreFeature -> IO UploadFeature
initUploadFeature _ = return UploadFeature
