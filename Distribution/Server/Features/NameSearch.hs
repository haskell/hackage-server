{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.NameSearch (
    NamesFeature(..),
    NamesResource(..),
    initNamesFeature,
  ) where

import Distribution.Server.Acid (query)
import Distribution.Server.Framework
import Distribution.Server.Features.Core

import Distribution.Server.Util.NameIndex
import Distribution.Server.Util.TextSearch
import qualified Distribution.Server.Framework.Cache as Cache
import qualified Distribution.Server.Framework.ResourceTypes as Resource
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import Distribution.Server.Packages.State
import Distribution.Server.Packages.Types
import Distribution.Text
import Distribution.Package
import Distribution.PackageDescription

import Control.Applicative ((<|>), optional, pure)
import Data.Function (fix)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.URI (URI(..), uriToString)
import Control.Monad.Trans (MonadIO)
import Text.JSON

data NamesFeature = NamesFeature {
    namesResource :: NamesResource,
    packageNameIndex :: Cache.Cache NameIndex,
    packageTextIndex :: Cache.Cache TextSearch,
    openSearchCache :: Cache.Cache Response,
    regenerateIndices :: IO (),

    packageFindWith :: forall a. (Maybe (String, Bool) -> ServerPart a) -> ServerPart a,
    searchFindPackage :: MonadIO m => String -> Bool -> m ([PackageName], [PackageName])
}

data NamesResource = NamesResource {
    openSearchXml :: Resource,
    findPackageResource :: Resource,
    suggestPackageResource :: Resource
}

instance IsHackageFeature NamesFeature where
    getFeatureInterface names = (emptyHackageFeature "names") {
        featureResources = map ($namesResource names) [openSearchXml, findPackageResource, suggestPackageResource]
      , featurePostInit = regenerateIndices names
      }

-- Currently only prefix-searching of package names is supported, as well as a
-- text search of package descriptions using Bayer-Moore. The results could also
-- be ordered by download (see DownloadCount.hs sortByDownloads).
initNamesFeature :: ServerEnv -> CoreFeature -> IO NamesFeature
initNamesFeature env core = do
    let hostStr = uriToString id (URI "http:" (Just $ serverHostURI env) "" "" "") ""
    pkgCache <- Cache.newCacheable (emptyNameIndex Nothing)
    textCache <- Cache.newCache (constructTextIndex []) id
    osdCache <- Cache.newCacheable (toResponse $ Resource.OpenSearchXml $ BS.pack $ mungeSearchXml hostStr)
    let regen = do
            index <- fmap packageList $ query GetPackagesState
            -- asynchronously update indices
            Cache.putCache pkgCache (constructIndex (map display $ PackageIndex.packageNames index) Nothing)
            Cache.putCache textCache (constructPackageText index)
    registerHook (packageAddHook core) $ \_ -> regen
    registerHook (packageRemoveHook core) $ \_ -> regen
    registerHook (packageChangeHook core) $ \_ _ -> regen
    return NamesFeature
      { namesResource = fix $ \_ -> NamesResource
          { openSearchXml = (resourceAt "/opensearch.xml") { resourceGet = [("xml", Cache.respondCache osdCache id)] }
            -- /packages/find?name=happstack
          , findPackageResource = resourceAt "/packages/find.:format"
          , suggestPackageResource = (resourceAt "/packages/suggest.:format") { resourceGet = [("json", \_ -> suggestJson hostStr pkgCache)] }
          }
      , packageNameIndex = pkgCache
      , packageTextIndex = textCache
      , openSearchCache = osdCache
      , regenerateIndices = regen
      , packageFindWith
      , searchFindPackage = searchFindPackage pkgCache textCache
      }
  where
    constructPackageText :: PackageIndex PkgInfo -> TextSearch
    constructPackageText = constructTextIndex . map makeEntry . PackageIndex.allPackagesByName
      where makeEntry pkgs = let desc = pkgDesc $ last pkgs
                                 pkgStr = display $ packageName desc
                             in (pkgStr, pkgStr ++ " " ++ synopsis (packageDescription desc))


    -- Returns (list of exact matches, list of text matches)
    -- HTML search pages should have meta ! [name "robots", content "noindex"]
    searchFindPackage :: MonadIO m => Cache.Cache NameIndex -> Cache.Cache TextSearch
                      -> String -> Bool -> m ([PackageName], [PackageName])
    searchFindPackage packageNameIndex packageTextIndex str doTextSearch = do
        nmIndex <- Cache.getCache packageNameIndex
        txIndex <- Cache.getCache packageTextIndex
        let nameRes = lookupName str nmIndex
            textRes = if doTextSearch then searchText txIndex str else []
        return $ (map PackageName $ Set.toList nameRes, map (PackageName . fst) textRes)

    packageFindWith :: (Maybe (String, Bool) -> ServerPart a) -> ServerPart a
    packageFindWith func = do
        mname <- optional (look "name")
        func $ fmap (\n -> (n, length n >= 3)) mname

    suggestJson :: String -> Cache.Cache NameIndex -> ServerPart Response
    suggestJson hostStr cache = do
        queryStr <- look "name" <|> pure ""
        -- There are a few ways to improve this.
        -- 1. Group similarly prefixed items, so user can see more breadth and less depth
        -- 2. Sort by revdeps, downloads, etc. (create a general "hotness" index)
        results <- fmap (take 20 . Set.toList . lookupPrefix queryStr) $ Cache.getCache cache
        let packagePrefix = hostStr ++ "/package/"
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

