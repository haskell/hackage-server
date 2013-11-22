{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards, DoRec #-}
module Distribution.Server.Features.Search (
    SearchFeature(..),
    initSearchFeature,

    -- * Search parameters
    defaultSearchRankParameters,
    SearchEngine.SearchRankParameters(..),
    PkgDocField, PkgDocFeatures,
    BM25F.Explanation(..),
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.Templating

import Distribution.Server.Features.Core

import Distribution.Server.Features.Search.PkgSearch
import Distribution.Server.Features.Search.SearchEngine (SearchRankParameters(..))
import qualified Distribution.Server.Features.Search.SearchEngine as SearchEngine
import qualified Distribution.Server.Features.Search.BM25F as BM25F
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Distribution.Server.Packages.Types

import Distribution.Package
import Distribution.PackageDescription.Configuration (flattenPackageDescription)

import qualified Data.Text as T


data SearchFeature = SearchFeature {
    searchFeatureInterface :: HackageFeature,

    searchPackagesResource :: Resource,

    searchPackages        :: MonadIO m => [String] -> m [PackageName],
    searchPackagesExplain :: MonadIO m
                          => SearchRankParameters PkgDocField PkgDocFeatures
                          -> [String]
                          -> m [(BM25F.Explanation PkgDocField PkgDocFeatures T.Text
                                ,PackageName)]
}

instance IsHackageFeature SearchFeature where
    getFeatureInterface = searchFeatureInterface


initSearchFeature :: ServerEnv -> CoreFeature -> IO SearchFeature
initSearchFeature env@ServerEnv{serverTemplatesDir, serverTemplatesMode, serverVerbosity = verbosity}
                 core@CoreFeature{..} = do
    loginfo verbosity "Initialising search feature, start"

    templates <- loadTemplates serverTemplatesMode
                   [serverTemplatesDir, serverTemplatesDir </> "Search"]
                   [ "opensearch.xml"]

    searchEngineState <- newMemStateWHNF initialPkgSearchEngine

    let feature = searchFeature env core
                                searchEngineState templates

    loginfo verbosity "Initialising search feature, end"
    return feature


searchFeature :: ServerEnv
              -> CoreFeature
              -> MemState PkgSearchEngine
              -> Templates
              -> SearchFeature

searchFeature ServerEnv{serverBaseURI} CoreFeature{..}
              searchEngineState templates
  = SearchFeature{..}
  where
    searchFeatureInterface = (emptyHackageFeature "search") {
        featureResources =
          [ searchOpenSearchResource
          , searchPackagesResource
--          , searchSuggestResource
          ]
      , featureState  = []
      , featureCaches = [
            CacheComponent {
              cacheDesc       = "package search engine",
              getCacheMemSize = memSize <$> readMemState searchEngineState
            }
          ]
      , featurePostInit = postInit
      }

    searchOpenSearchResource = (resourceAt "/packages/opensearch.xml") {
        resourceDesc = [(GET, "An OpenSearch description of the package search")],
        resourceGet  = [("xml", handlerGetOpenSearch)]
      }
    -- /packages/search?terms=happstack
    searchPackagesResource = (resourceAt "/packages/search.:format") {
        resourceDesc = [(GET, "Search for packages matching query terms")]
--        resourceGet  = [("json", handlerGetOpenSearch)]
      }

--  searchSuggestResource = (resourceAt "/packages/suggest.:format") {
--      resourceDesc = [(GET, "An OpenSearch description of the package search")]
--      resourceGet = [("json", \_ -> suggestJson)]
--    }

    getSearchDoc = flattenPackageDescription . pkgDesc

    postInit = do
      pkgindex <- queryGetPackageIndex
      let pkgs = map (getSearchDoc . last)
               . PackageIndex.allPackagesByName $ pkgindex
          se = SearchEngine.insertDocs pkgs initialPkgSearchEngine
      writeMemState searchEngineState se

      registerHookJust packageChangeHook isPackageChangeAny $ \(pkgid, _) ->
        updatePackage (packageName pkgid)

    updatePackage :: PackageName -> IO ()
    updatePackage pkgname = do
      index <- queryGetPackageIndex
      let pkgs = PackageIndex.lookupPackageName index pkgname
      case reverse pkgs of
         []      -> modifyMemState searchEngineState
                      (SearchEngine.deleteDoc pkgname)
         (pkg:_) -> modifyMemState searchEngineState
                      (SearchEngine.insertDoc (getSearchDoc pkg))

    -- Returns list of query results
    searchPackages :: MonadIO m => [String] -> m [PackageName]
    searchPackages terms = do
        se <- readMemState searchEngineState
        let results = SearchEngine.query se (map T.pack terms)
        return results

    searchPackagesExplain :: MonadIO m
                          => SearchRankParameters PkgDocField PkgDocFeatures
                          -> [String]
                          -> m [(BM25F.Explanation PkgDocField PkgDocFeatures T.Text, PackageName)]
    searchPackagesExplain params terms = do
        se <- readMemState searchEngineState
        let results = SearchEngine.queryExplain
                        (SearchEngine.setRankParams params se)
                        (map T.pack terms)
        return results

    handlerGetOpenSearch :: DynamicPath -> ServerPartE Response
    handlerGetOpenSearch _ = do
      template <- getTemplate templates "opensearch.xml"
      let xmlstr = renderTemplate (template ["serverhost" $= show serverBaseURI])
      return $ toResponse (OpenSearchXml xmlstr)

{-
    suggestJson :: ServerPartE Response
    suggestJson =
    --TODO: open search supports a suggest / autocomplete system
-}
