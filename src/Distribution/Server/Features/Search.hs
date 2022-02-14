{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards, RecursiveDo #-}
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
import Distribution.Server.Features.PackageList

import Distribution.Server.Features.Search.PkgSearch
import Distribution.Server.Features.Search.SearchEngine (SearchRankParameters(..))
import qualified Distribution.Server.Features.Search.SearchEngine as SearchEngine
import qualified Distribution.Server.Features.Search.BM25F as BM25F
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Distribution.Server.Packages.Types

import Distribution.Package
import Distribution.PackageDescription.Configuration (flattenPackageDescription)

import qualified Data.Text as T
import qualified Data.Map as Map

import Control.Applicative (optional)
import Data.Aeson

data SearchFeature = SearchFeature {
    searchFeatureInterface :: HackageFeature,

    searchPackagesResource :: Resource,

    searchPackages        :: forall m. MonadIO m => [String] -> m [PackageName],
    searchPackagesExplain :: forall m. MonadIO m
                          => SearchRankParameters PkgDocField PkgDocFeatures
                          -> [String]
                          -> m (Maybe PackageName,
                               [(BM25F.Explanation PkgDocField PkgDocFeatures T.Text
                                ,PackageName)])
}

instance IsHackageFeature SearchFeature where
    getFeatureInterface = searchFeatureInterface


initSearchFeature :: ServerEnv
                  -> IO (CoreFeature
                      -> ListFeature
                      -> IO SearchFeature)
initSearchFeature env@ServerEnv{serverTemplatesDir, serverTemplatesMode} = do
    templates <- loadTemplates serverTemplatesMode
                   [serverTemplatesDir, serverTemplatesDir </> "Search"]
                   [ "opensearch.xml"]

    searchEngineState <- newMemStateWHNF initialPkgSearchEngine

    return $ \core@CoreFeature{..} list -> do
      let feature = searchFeature env core list
                                  searchEngineState templates

      return feature


searchFeature :: ServerEnv
              -> CoreFeature
              -> ListFeature
              -> MemState PkgSearchEngine
              -> Templates
              -> SearchFeature

searchFeature ServerEnv{serverBaseURI} CoreFeature{..} ListFeature{getAllLists}
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
      , featureReloadFiles = reloadTemplates templates
      }

    searchOpenSearchResource = (resourceAt "/packages/opensearch.xml") {
        resourceDesc = [(GET, "An OpenSearch description of the package search")],
        resourceGet  = [("xml", handlerGetOpenSearch)]
      }
    -- /packages/search?terms=happstack
    searchPackagesResource = (resourceAt "/packages/search.:format") {
        resourceDesc = [(GET, "Search for packages matching query terms")],
        resourceGet  = [("json", handlerGetJsonSearch)]
      }

--  searchSuggestResource = (resourceAt "/packages/suggest.:format") {
--      resourceDesc = [(GET, "An OpenSearch description of the package search")]
--      resourceGet = [("json", \_ -> suggestJson)]
--    }

    getSearchDoc = flattenPackageDescription . pkgDesc

    postInit = do
      pkgindex     <- queryGetPackageIndex
      pkgdownloads <- getDownloadCounts
      let pkgs = [ (getSearchDoc pkgLatestVer, pkgdownloads pkgname)
                 | pkgVers <- PackageIndex.allPackagesByName pkgindex
                 , let pkgLatestVer = last pkgVers
                       pkgname      = packageName pkgLatestVer ]
          se = SearchEngine.insertDocs pkgs initialPkgSearchEngine
      writeMemState searchEngineState se

      registerHookJust packageChangeHook isPackageChangeAny $ \(pkgid, _) ->
        updatePackage (packageName pkgid)

    --TODO: update periodically for download count changes
    updatePackage :: PackageName -> IO ()
    updatePackage pkgname = do
      index <- queryGetPackageIndex
      let pkgs = PackageIndex.lookupPackageName index pkgname
      case reverse pkgs of
         []      -> modifyMemState searchEngineState
                      (SearchEngine.deleteDoc pkgname)
         (pkg:_) -> do downloads <- getDownloadCount pkgname
                       modifyMemState searchEngineState
                         (SearchEngine.insertDoc (getSearchDoc pkg, downloads))

    getDownloadCount :: PackageName -> IO Int
    getDownloadCount pkgname = do
      pkginfomap <- getAllLists
      return $ maybe 0 itemDownloads (Map.lookup pkgname pkginfomap)

    getDownloadCounts :: IO (PackageName -> Int)
    getDownloadCounts = do
      pkginfomap <- getAllLists
      return (\pkgname -> maybe 0 itemDownloads (Map.lookup pkgname pkginfomap))

    -- Returns list of query results
    searchPackages :: MonadIO m => [String] -> m [PackageName]
    searchPackages terms = do
        se <- readMemState searchEngineState
        let results = SearchEngine.query se (map T.pack terms)
        return results

    searchPackagesExplain :: MonadIO m
                          => SearchRankParameters PkgDocField PkgDocFeatures
                          -> [String]
                          -> m (Maybe PackageName, [(BM25F.Explanation PkgDocField PkgDocFeatures T.Text, PackageName)])
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

    handlerGetJsonSearch :: DynamicPath -> ServerPartE Response
    handlerGetJsonSearch _ = do
      mtermsStr <-
        queryString $ optional (look "terms")
      case mtermsStr of
        Just termsStr | terms <- words termsStr, not (null terms) -> do
          pkgnames <- searchPackages terms
          ok (toResponse (toJSON (map packageNameJSON pkgnames)))

        _ ->
          errBadRequest "Invalid search request" [MText $ "Empty terms query"]
      where packageNameJSON pkgName =
              object [ T.pack "name" .= unPackageName pkgName ]

{-
    suggestJson :: ServerPartE Response
    suggestJson =
    --TODO: open search supports a suggest / autocomplete system
-}
