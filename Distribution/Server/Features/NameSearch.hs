{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards, DoRec #-}
module Distribution.Server.Features.NameSearch (
    NamesFeature(..),
    NamesResource(..),
    initNamesFeature,
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.Core

import Distribution.Server.Util.NameIndex
import Distribution.Server.Util.TextSearch
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import Distribution.Server.Packages.Types

import Distribution.Text
import Distribution.Package
import Distribution.PackageDescription

import Control.Applicative ((<|>), optional)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.URI (URI(..))
import Text.JSON


data NamesFeature = NamesFeature {
    namesFeatureInterface :: HackageFeature,
    namesResource :: NamesResource,

    packageFindWith :: forall a. (Maybe (String, Bool) -> ServerPartE a) -> ServerPartE a,
    searchFindPackage :: MonadIO m => String -> Bool -> m ([PackageName], [PackageName])
}

instance IsHackageFeature NamesFeature where
    getFeatureInterface = namesFeatureInterface

data NamesResource = NamesResource {
    openSearchXml :: Resource,
    findPackageResource :: Resource,
    suggestPackageResource :: Resource
}

-- Currently only prefix-searching of package names is supported, as well as a
-- text search of package descriptions using Bayer-Moore. The results could also
-- be ordered by download (see DownloadCount.hs sortByDownloads).
initNamesFeature :: ServerEnv -> CoreFeature -> IO NamesFeature
initNamesFeature env@ServerEnv{serverCacheDelay, serverVerbosity = verbosity}
                 core@CoreFeature{..} = do
    loginfo verbosity "Initialising names feature, start"

    rec let (feature, getNameIndex, getTextIndex, getOpenSearch) =
              namesFeature env core
                           pkgCache textCache osdCache

        pkgCache  <- newAsyncCacheNF getNameIndex
                       defaultAsyncCachePolicy {
                         asyncCacheName = "pkg name index",
                         asyncCacheUpdateDelay  = serverCacheDelay,
                         asyncCacheLogVerbosity = verbosity
                       }

        textCache <- newAsyncCacheNF getTextIndex
                       defaultAsyncCachePolicy {
                         asyncCacheName = "text index",
                         asyncCacheUpdateDelay  = serverCacheDelay,
                         asyncCacheSyncInit     = False,
                         asyncCacheLogVerbosity = verbosity
                       }

        osdCache  <- newAsyncCacheNF getOpenSearch
                       defaultAsyncCachePolicy {
                         asyncCacheName = "osd, TODO not a cache",
                         asyncCacheUpdateDelay  = serverCacheDelay,
                         asyncCacheLogVerbosity = verbosity
                       }

    registerHook packageAddHook    $ \_   -> prodAsyncCache pkgCache
                                          >> prodAsyncCache textCache
    registerHook packageRemoveHook $ \_   -> prodAsyncCache pkgCache
                                          >> prodAsyncCache textCache
    registerHook packageChangeHook $ \_ _ -> prodAsyncCache pkgCache
                                          >> prodAsyncCache textCache

    loginfo verbosity "Initialising names feature, end"
    return feature

namesFeature :: ServerEnv
             -> CoreFeature
             -> AsyncCache NameIndex
             -> AsyncCache TextSearch
             -> AsyncCache Response
             -> (NamesFeature, IO NameIndex, IO TextSearch, IO Response)

namesFeature ServerEnv{serverBaseURI} CoreFeature{..}
             packageNameIndex packageTextIndex
             openSearchCache
  = (NamesFeature{..}, getNameIndex, getTextIndex, getOpenSearch)
  where
    namesFeatureInterface = (emptyHackageFeature "names") {
        featureResources =
          map ($namesResource) [
              openSearchXml
            , findPackageResource
            , suggestPackageResource
            ]
      , featureState  = []
      , featureCaches = [
            CacheComponent {
              cacheDesc       = "package name search index",
              getCacheMemSize = memSize <$> readAsyncCache packageNameIndex
            }
          , CacheComponent {
              cacheDesc       = "package text search index",
              getCacheMemSize = memSize <$> readAsyncCache packageTextIndex
            }
          ]
      , featurePostInit = syncAsyncCache packageTextIndex
      }

    namesResource = NamesResource
      { openSearchXml = (resourceAt "/opensearch.xml") {
          resourceGet = [("xml", \_ -> opensearchGet)]
        }
        -- /packages/find?name=happstack
      , findPackageResource = resourceAt "/packages/find.:format"
      , suggestPackageResource = (resourceAt "/packages/suggest.:format") {
          resourceGet = [("json", \_ -> suggestJson)]
        }
      }

    getNameIndex = do
      index <- queryGetPackageIndex :: IO (PackageIndex PkgInfo)
      let names     = map display $ PackageIndex.packageNames index
          nameindex = constructIndex names Nothing
      return nameindex
    
    getTextIndex = do
      index <- queryGetPackageIndex :: IO (PackageIndex PkgInfo)
      let textindex = constructPackageText index
      return textindex

    constructPackageText :: PackageIndex PkgInfo -> TextSearch
    constructPackageText = constructTextIndex . map makeEntry . PackageIndex.allPackagesByName
      where makeEntry pkgs = let desc = pkgDesc $ last pkgs
                                 pkgStr = display $ packageName desc
                             in (pkgStr, pkgStr ++ " " ++ synopsis (packageDescription desc))


    -- Returns (list of exact matches, list of text matches)
    -- HTML search pages should have meta ! [name "robots", content "noindex"]
    searchFindPackage :: MonadIO m => String -> Bool -> m ([PackageName], [PackageName])
    searchFindPackage str doTextSearch = do
        nmIndex <- readAsyncCache packageNameIndex
        txIndex <- readAsyncCache packageTextIndex
        let nameRes = lookupName str nmIndex
            textRes = if doTextSearch then searchText txIndex str else []
        return $ (map PackageName $ Set.toList nameRes, map (PackageName . fst) textRes)

    packageFindWith :: (Maybe (String, Bool) -> ServerPartE a) -> ServerPartE a
    packageFindWith func = do
        mname <- optional (look "name")
        func $ fmap (\n -> (n, length n >= 3)) mname

    getOpenSearch :: IO Response
    getOpenSearch = return $ toResponse (Resource.OpenSearchXml xmlstr)
      where xmlstr = BS.pack (mungeSearchXml (show serverBaseURI))

    opensearchGet :: ServerPartE Response
    opensearchGet = return . toResponse =<< readAsyncCache openSearchCache

    suggestJson :: ServerPartE Response
    suggestJson = do
        queryStr <- look "name" <|> pure ""
        -- There are a few ways to improve this.
        -- 1. Group similarly prefixed items, so user can see more breadth and less depth
        -- 2. Sort by revdeps, downloads, etc. (create a general "hotness" index)
        results <- fmap (take 20 . Set.toList . lookupPrefix queryStr) $ readAsyncCache packageNameIndex
        let packagePrefix = show (serverBaseURI { uriPath = "/package/" })
        return . toResponse $ Resource.SuggestJson $ JSArray
          [ JSString $ toJSString queryStr
          , JSArray $ map (JSString . toJSString) results
          , JSArray []
          , JSArray $ map (JSString . toJSString . (packagePrefix++)) results ]

-- A hacky but simple OpenSearch XML generator.
-- The only reason it's not hard-coded in the static directory is because it
-- the host name is required to fill it out.
-- It should only be called once per server run.
-- TODO: replace it. maybe.
mungeSearchXml :: String -> String
mungeSearchXml hostStr =
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
        makeUri str = hostStr ++ str

