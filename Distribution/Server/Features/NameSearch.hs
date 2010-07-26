module Distribution.Server.Features.NameSearch (
    NamesFeature(..),
    initNamesFeature
  ) where

import Distribution.Server.Feature

data NamesFeature = NamesFeature {
    NamesResource :: NamesResource
}

data NamesResource = NamesResource {
    openSearchXml :: Resource
    findPackageResource :: Resource
    suggestPackageResource :: Resource
}

instance HackageFeature NamesFeature where
    getFeature names = HackageModule
      { featureName = "names"
      , resources   = map ($namesResource names) []
      , dumpBackup    = Nothing
      , restoreBackup = Nothing
      }

-- Currently only prefix-searching is supported, though with a special data
-- structure 'serv' could turn up 'happstack-server'. The results could also
-- be ordered by download (see DownloadCount.hs sortByDownloads) with the top,
-- say, 10 served
initNamesFeature :: Config -> CoreFeature -> IO NamesFeature
initNamesFeature config core = do
    return NamesFeature
      { NamesResource = fix $ \r -> NamesResource
          { openSearchXml = (resourceAt "/opensearch.xml") { ("xml", \_ -> xmlPage $ serverStaticDir config ) }
            -- /packages/find?name=happstack
            findPackageResource = (resourceAt "/packages/find.:format") { ("txt", \_ -> textPackageFind) }
            suggestPackageResource = (resourceAt "/packages/suggest.:format") { ("json", \_ -> suggestResults) }
          }
      }
  where
    xmlPage staticDir = ... serve a file as "application/opensearchdescription+xml"
    --search pages should have meta ! [name "robots", content="noindex"]
    -- And all pages should have: thelink ! [rel "search", href "/opensearch.xml", title "Hackage",
           thetype "application/opensearchdescription+xml"] << noHtml :
suggestJson :: ServerPart Response
suggestJson = ...

packageNameSearch :: MonadIO m => String -> m [PackageId]
