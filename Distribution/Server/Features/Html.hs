module Distribution.Server.Features.Html (
    HtmlFeature(..),
    initHtmlFeature
  ) where

import Distribution.Server.Feature

data HtmlFeature = HtmlFeature

instance HackageFeature HtmlFeature where
    getFeature _ = HackageModule
      { featureName = "html"
      , resources   = []
      , dumpBackup    = return []
      , restoreBackup = Nothing
      }

initHtmlFeature :: CoreFeature -> IO HtmlFeature
initHtmlFeature _ = return HtmlFeature
