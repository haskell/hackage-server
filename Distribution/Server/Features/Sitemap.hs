{-# LANGUAGE RecordWildCards, NamedFieldPuns, RecursiveDo #-}

module Distribution.Server.Features.Sitemap (
  SitemapFeature(..)
, initSitemapFeature
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.Sitemap.Functions

import Distribution.Server.Features.Core
import Distribution.Server.Features.Documentation
import Distribution.Server.Features.Tags

import Distribution.Package
import Distribution.Text (display)
import Distribution.Server.Packages.Types

import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import qualified Data.Vector as Vec
import qualified Data.Text as T

import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Calendar (showGregorian)



data SitemapFeature = SitemapFeature {
  sitemapFeatureInterface :: HackageFeature
}

instance IsHackageFeature SitemapFeature where
  getFeatureInterface = sitemapFeatureInterface

initSitemapFeature :: ServerEnv
                   -> IO ( CoreFeature
                      -> DocumentationFeature
                      -> TagsFeature
                      -> IO SitemapFeature)

initSitemapFeature env@ServerEnv{ serverCacheDelay,
                                  serverVerbosity = verbosity} = do
  initTime <- getCurrentTime

  return $ \coref@CoreFeature{..}
            docsCore@DocumentationFeature{..}
            tagsf@TagsFeature{..} -> do

    rec let (feature, updateSitemapCache) =
              sitemapFeature env coref docsCore tagsf
                             initTime sitemapCache

        sitemapCache <- newAsyncCacheNF updateSitemapCache
                          defaultAsyncCachePolicy {
                            asyncCacheName         = "sitemap.xml",
                            asyncCacheUpdateDelay  = serverCacheDelay,
                            asyncCacheSyncInit     = False,
                            asyncCacheLogVerbosity = verbosity
                          }

    return feature

sitemapFeature  :: ServerEnv
                -> CoreFeature
                -> DocumentationFeature
                -> TagsFeature
                -> UTCTime
                -> AsyncCache XMLResponse
                -> (SitemapFeature, IO XMLResponse)
sitemapFeature  ServerEnv{..}
                CoreFeature{..}
                DocumentationFeature{..}
                TagsFeature{..}
                initTime
                sitemapCache
  = (SitemapFeature{..}, updateSitemapCache)
  where

    sitemapFeatureInterface = (emptyHackageFeature "sitemap") {
      featureResources  = [ xmlSitemapResource ]
      , featureState    = []
      , featureDesc     = "Provides a sitemap.xml for search engines"
      , featureCaches   =
          [ CacheComponent {
              cacheDesc       = "sitemap.xml",
              getCacheMemSize = memSize <$> readAsyncCache sitemapCache
            }
          ]
      , featurePostInit = do
          syncAsyncCache sitemapCache
          addCronJob serverCron CronJob {
            cronJobName      = "regenerate the cached sitemap.xml",
            cronJobFrequency = DailyJobFrequency,
            cronJobOneShot   = False,
            cronJobAction    = prodAsyncCache sitemapCache
          }
    }

    xmlSitemapResource :: Resource
    xmlSitemapResource = (resourceAt "/sitemap.xml") {
      resourceDesc = [(GET, "The dynamically generated sitemap, in XML format")]
    , resourceGet = [("xml", serveSitemap)]
    }

    serveSitemap :: DynamicPath -> ServerPartE Response
    serveSitemap _ = do
      sitemapXML <- liftIO $ readAsyncCache sitemapCache
      cacheControlWithoutETag [Public, maxAgeDays 1]
      return (toResponse sitemapXML)

    pageBuildDate :: T.Text
    pageBuildDate = T.pack (showGregorian (utctDay initTime))

    -- Generates a list of sitemap entries corresponding to hackage pages, then
    -- builds and returns an XML sitemap.
    updateSitemapCache :: IO XMLResponse
    updateSitemapCache = do

      -- Misc. pages
      -- e.g. ["http://myhackage.com/index", ...]
      let miscPages =
            [ "/index"
            , "/accounts"
            , "/packages"
            , "/packages/search"
            , "/packages/recent"
            , "/packages/recent/revisions"
            , "/packages/tags"
            , "/packages/names"
            , "/packages/top"
            , "/packages/preferred"
            , "/packages/deprecated"
            , "/packages/candidates"
            , "/packages/uploaders"
            , "/users"
            , "/users/register-request"
            , "/users/password-reset"
            , "/upload"
            , "/api"
            ]
          miscEntries = urlsToSitemapEntries miscPages
                                             pageBuildDate Weekly 0.75

      -- Pages for each individual tag.
      alltags <- queryGetTagList
      let tagPrefixURI = "/packages/tag/"

      --  tagURLs :: [path :: String]
      --  e.g. ["http://myhackage.com/packages/tag/bsd", ...]
          tagURLs    = map ((tagPrefixURI ++) . display . fst) alltags
          tagEntries = urlsToSitemapEntries tagURLs
                                            pageBuildDate Daily 0.5

      pkgIndex <- queryGetPackageIndex

      let pkgs = PackageIndex.allPackagesByName pkgIndex
          prefixPkgURI = "/package/"

      --  Unversioned package pages - always redirect to latest version.
      --  names :: [(path :: String, lastMod :: UTCTime)]
      --  e.g. [("http://myhackage.com/packages/mypackage", "2012-04-30..."), ...]
          names =
            [ ((prefixPkgURI ++) . display . pkgName . pkgInfoId $ pkg
            , fst . snd . Vec.head . pkgMetadataRevisions $ pkg)
            | pkg <- map head pkgs
            ]
          nameEntries = pathsAndDatesToSitemapEntries names
                                                      Daily 1.0

      --  Versioned package pages
      --  nameVers :: [path :: String]
      --  e.g. ["http://myhackage.com/packages/mypackage-1.0.2", ...]
          nameVers =
            [ ((prefixPkgURI ++) . display . pkgName . pkgInfoId $ pkg)
              ++ "-" ++ (display . pkgVersion . pkgInfoId $ pkg)
            | pkg <- concat pkgs
            ]
          nameVersEntries = urlsToSitemapEntries nameVers
                                                 pageBuildDate Monthly 0.25

      --  Unversioned doc pages - always redirect to latest version.
      --  (for packages with valid documentation)
      basePkgNamesWithDocs <- mapParaM
        (queryHasDocumentation . pkgInfoId) (map head pkgs)

      --  baseDocs :: [path :: String]
      --  e.g. ["http://myhackage.com/packages/mypackage/docs". ...]
      let baseDocs =
            [ ((prefixPkgURI ++) . display . pkgName . pkgInfoId . fst $ pkg)
              ++ "/docs"
            | pkg <- filter ((== True) . snd) basePkgNamesWithDocs
            ]
          baseDocEntries = urlsToSitemapEntries baseDocs
                                                pageBuildDate Daily 1.0

      -- Versioned doc pages
      versionedDocNames <- mapParaM
        (queryHasDocumentation . pkgInfoId) (concat pkgs)

      --  versionedDocURIs :: [path :: String]
      --  e.g. ["http://myhackage.com/packages/mypackage-1.0.2/docs", ...]
      let versionedDocURIs =
            [ ((prefixPkgURI ++) . display . pkgName . pkgInfoId . fst $ pkg)
              ++ "-" ++
              (display . pkgVersion . pkgInfoId . fst $ pkg) ++ "/docs"
            | pkg <- filter ((== True) . snd) versionedDocNames
            ]
          versionedDocEntries = urlsToSitemapEntries versionedDocURIs
                                                     pageBuildDate Monthly 0.25

      -- Combine and build sitemap
          allEntries = miscEntries
            ++ tagEntries
            ++ nameEntries
            ++ nameVersEntries
            ++ baseDocEntries
            ++ versionedDocEntries
          sitemapXML = XMLResponse (renderSitemap serverBaseURI allEntries)

      return sitemapXML

    mapParaM :: Monad m => (a -> m b) -> [a] -> m [(a, b)]
    mapParaM f = mapM (\x -> (,) x `liftM` f x)

