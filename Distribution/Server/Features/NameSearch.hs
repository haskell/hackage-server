module Distribution.Server.Features.NameSearch (
    NamesFeature(..),
    initNamesFeature
  ) where

import Distribution.Server.Feature
import Distribution.Server.Types
import Distribution.Server.Resource
import Distribution.Server.Features.Core

import Distribution.Server.Util.NameIndex
import qualified Distribution.Server.Cache as Cache
import qualified Distribution.Server.ResourceTypes as Resource
import qualified Distribution.Server.PackageIndex as PackageIndex
import Distribution.Server.Packages.State
import Distribution.Text

import Data.Maybe
import Data.Function (fix)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.URI (URI(..), uriToString)
import Text.JSON
import Happstack.Server
import Happstack.State

data NamesFeature = NamesFeature {
    namesResource :: NamesResource,
    packageNameIndex :: Cache.Cache NameIndex,
    openSearchCache :: Cache.Cache Response
}

data NamesResource = NamesResource {
    openSearchXml :: Resource,
    findPackageResource :: Resource,
    suggestPackageResource :: Resource
}

instance HackageFeature NamesFeature where
    getFeature names = HackageModule
      { featureName = "names"
      , resources   = map ($namesResource names) [openSearchXml, findPackageResource, suggestPackageResource]
      , dumpBackup    = Nothing
      , restoreBackup = Nothing
      }

-- Currently only prefix-searching is supported, though with a special data
-- structure 'serv' could turn up 'happstack-server'. The results could also
-- be ordered by download (see DownloadCount.hs sortByDownloads) with the top,
-- say, 10 served
initNamesFeature :: Config -> CoreFeature -> IO NamesFeature
initNamesFeature config core = do
    let hostStr = uriToString id (URI "http:" (Just $ serverURI config) "" "" "") ""
    names <- fmap (PackageIndex.packageNames . packageList) $ query GetPackagesState
    pkgCache <- Cache.newCacheable (constructIndex (map display names) Nothing)
    osdCache <- Cache.newCacheable (toResponse $ Resource.OpenSearchXml $ BS.pack $ mungeSearchXml hostStr)
    return NamesFeature
      { namesResource = fix $ \_ -> NamesResource
          { openSearchXml = (resourceAt "/opensearch.xml") { resourceGet = [("xml", Cache.respondCache osdCache id)] }
            -- /packages/find?name=happstack
          , findPackageResource = (resourceAt "/packages/find.:format") { resourceGet = [("txt", \_ -> getDataFn (look "name") >>= return . toResponse . show)] }
          , suggestPackageResource = (resourceAt "/packages/suggest.:format") { resourceGet = [("json", \_ -> suggestJson hostStr pkgCache)] }
          }
      }
  where
    --search pages should have meta ! [name "robots", content="noindex"]

suggestJson :: String -> Cache.Cache NameIndex -> ServerPart Response
suggestJson host cache = do
    queryStr <- fmap (fromMaybe "") $ getDataFn (look "name")
    -- TODO: there are a few ways to improve this.
    -- 1. Group similarly prefixed items, so user can type to see others
    -- 2. Sort by revdeps, downloads, etc. (create a general "hotness" index)
    results <- fmap (take 20 . Set.toList . lookupPrefix queryStr) $ Cache.getCache cache
    let packagePrefix = host ++ "/package/"
    return . toResponse $ Resource.SuggestJson $ JSArray
      [ JSString $ toJSString queryStr
      , JSArray $ map (JSString . toJSString) results
      , JSArray []
      , JSArray $ map (JSString . toJSString . (packagePrefix++)) results ]

-- A hacky, fragile, and simple XML generator. On the plus side, it's
-- less complicated than any of the other libraries out there, which don't have
-- clear usage examples, and it's all that's needed for the open search format.
-- It should only be called once per server run.
-- TODO: replace it.
mungeSearchXml :: String -> String
mungeSearchXml host =
    "<?xml version=\"1.0\"?><OpenSearchDescription xmlns=\"http://a9.com/-/spec/opensearch/1.1/\" xmlns:moz=\"http://www.mozilla.org/2006/browser/search/\">" ++
    makeElem "ShortName" [] (Just "Hackage") ++
    makeElem "Description" [] (Just "Hackage package database") ++
    makeElem "Image" [("height", "16"), ("width", "16"), ("type", "image/x-icon")] (Just $ makeUri "/favicon.ico") ++
    makeElem "Url" [("type", "text/html"), ("method", "get"), ("template", makeUri "/packages/find?name={searchTerms}")] Nothing ++
    makeElem "Url" [("type", "application/x-suggestions+json"), ("method", "get"), ("template", makeUri "/packages/suggest.json?name={searchTerms}")] Nothing ++
    makeElem "moz:SearchForm" [] (Just $ makeUri "/packages/") ++
    "</OpenSearchDescription>"
  where makeElem str attrs mint = "<"++str++makeAttrs attrs++maybe " />" (\t->">"++t++"</"++str++">") mint
        makeAttrs attrs = case attrs of [] -> ""; _ -> concatMap (\(k, v) -> " "++k++"=\""++v++"\"") attrs
        makeUri str = host ++ str

