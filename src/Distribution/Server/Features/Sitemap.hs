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
import qualified Data.Map  as Map
import qualified Data.Text as T
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Calendar (showGregorian)
import Network.URI


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
            cronJobAction    = prodAsyncCache sitemapCache "cron"
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

    -- Generates a list of sitemap entries corresponding to hackage pages, then
    -- builds and returns an XML sitemap.
    updateSitemapCache :: IO XMLResponse
    updateSitemapCache = do

      alltags  <- queryGetTagList
      pkgIndex <- queryGetPackageIndex
      docIndex <- queryDocumentationIndex

      let sitemap = generateSitemap serverBaseURI pageBuildDate
                                    (map fst alltags)
                                    pkgIndex docIndex
      return (XMLResponse sitemap)

    pageBuildDate :: T.Text
    pageBuildDate = T.pack (showGregorian (utctDay initTime))

generateSitemap :: URI
                -> T.Text
                -> [Tag]
                -> PackageIndex.PackageIndex PkgInfo
                -> Map.Map PackageId a
                -> ByteString
generateSitemap serverBaseURI pageBuildDate alltags pkgIndex docIndex =
    renderSitemap serverBaseURI allEntries
  where
    -- Combine and build sitemap
    allEntries = miscEntries
              ++ tagEntries
              ++ nameEntries
              ++ nameVersEntries
              ++ baseDocEntries
              ++ versionedDocEntries

    -- Misc. pages
    -- e.g. ["http://myhackage.com/index", ...]
    miscEntries = urlsToSitemapEntries miscPages pageBuildDate Weekly 0.75
    miscPages   =
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

    -- Pages for each individual tag.
    --  tagURLs :: [path :: String]
    --  e.g. ["http://myhackage.com/packages/tag/bsd", ...]
    tagEntries =
      urlsToSitemapEntries
        [ "/packages/tag/" ++  display tag
        | tag <- alltags ]
        pageBuildDate Daily 0.5

    pkgss = PackageIndex.allPackagesByName pkgIndex
    prefixPkgURI = "/package/"

    --  Unversioned package pages - always redirect to latest version.
    --  names :: [(path :: String, lastMod :: UTCTime)]
    --  e.g. [("http://myhackage.com/packages/mypackage", "2012-04-30..."), ...]
    nameEntries =
      pathsAndDatesToSitemapEntries
        [ ( prefixPkgURI ++ display (packageName pkg)
          , uploadtime)
        | pkg <- map head pkgss
        , let (_, (uploadtime, _user)) = Vec.head (pkgMetadataRevisions pkg)
        ]
        Daily 1.0

    --  Versioned package pages
    --  nameVers :: [path :: String]
    --  e.g. ["http://myhackage.com/packages/mypackage-1.0.2", ...]
    nameVersEntries =
      urlsToSitemapEntries
        [ prefixPkgURI ++ display (packageId pkg)
        | pkg <- concat pkgss
        ]
        pageBuildDate Monthly 0.25

    --  Unversioned doc pages - always redirect to latest version.
    --  (for packages with valid documentation)
    --  baseDocs :: [path :: String]
    --  e.g. ["http://myhackage.com/packages/mypackage/docs". ...]
    baseDocEntries =
      urlsToSitemapEntries
        [ prefixPkgURI ++ display (packageName pkg) ++ "/docs"
        | pkg <- map head pkgss
        , Map.member (packageId pkg) docIndex
        ]
        pageBuildDate Daily 1.0

    -- Versioned doc pages
    --  versionedDocURIs :: [path :: String]
    --  e.g. ["http://myhackage.com/packages/mypackage-1.0.2/docs", ...]
    versionedDocEntries =
      urlsToSitemapEntries
        [ prefixPkgURI ++ display (packageId pkg) ++ "/docs"
        |  pkg <- concat pkgss
        , Map.member (packageId pkg) docIndex
        ]
        pageBuildDate Monthly 0.25
