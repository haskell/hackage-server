{-# LANGUAGE RecordWildCards #-}


module Distribution.Server.Features.Sitemap (
  SitemapFeature(..)
, initSitemapFeature
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.Core
import Distribution.Server.Features.Documentation
import Distribution.Server.Features.Tags

import Distribution.Package
import Distribution.Text (display)
import Distribution.Server.Packages.Types

import qualified Distribution.Server.Features.Sitemap.Functions as SM
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import qualified Data.Vector as Vec
import qualified Data.List as L

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

initSitemapFeature env@ServerEnv{..} = do
  initTime <- getCurrentTime

  return $ \coref@CoreFeature{..}
            docsCore@DocumentationFeature{..}
            tagsf@TagsFeature{..} -> do

    let feature = sitemapFeature env
                  coref docsCore tagsf
                  initTime
    return feature

sitemapFeature  :: ServerEnv
                -> CoreFeature
                -> DocumentationFeature
                -> TagsFeature
                -> UTCTime
                -> SitemapFeature
sitemapFeature  ServerEnv{..}
                CoreFeature{..}
                DocumentationFeature{..}
                TagsFeature{..}
                initTime
  = SitemapFeature{..} where

    sitemapFeatureInterface = (emptyHackageFeature "XML sitemap generation") {
      featureResources  = [ xmlSitemapResource ]
      , featureState    = []
      , featureDesc     = "Dynamically generates a sitemap.xml."
      , featureCaches   = []
    }

    xmlSitemapResource :: Resource
    xmlSitemapResource = (resourceAt "/sitemap.xml") {
      resourceDesc = [(GET, "Returns a dynamically generated sitemap in XML form.")]
    , resourceGet = [("xml", generateSitemap)]
    }

    pageBuildDate = showGregorian (utctDay initTime)

    -- Generates a list of URL nodes corresponding to hackage pages, then
    -- builds and returns an XML sitemap as a response.
    generateSitemap :: DynamicPath -> ServerPartE Response
    generateSitemap _ = do

      -- Misc. pages
      -- e.g. ["http://myhackage.com/index", ...]
      let miscPages = map (show serverBaseURI ++)
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
          miscNodes = SM.urlsToNodes miscPages
            pageBuildDate "weekly" "0.75"

      -- Pages for each individual tag.
      alltags <- queryGetTagList
      let tagPrefixURI = show serverBaseURI ++ "/packages/tag/"

      --  tagURLs :: [path :: String]
      --  e.g. ["http://myhackage.com/packages/tag/bsd", ...]
          tagURLs = map ((tagPrefixURI ++) . display . fst) alltags
          tagNodes = SM.urlsToNodes tagURLs
            pageBuildDate "daily" "0.5"

      pkgIndex <- queryGetPackageIndex

      let pkgs = PackageIndex.allPackagesByName pkgIndex
          prefixPkgURI = show serverBaseURI ++ "/package/"

      --  Unversioned package pages - always redirect to latest version.
      --  names :: [(path :: String, lastMod :: UTCTime)]
      --  e.g. [("http://myhackage.com/packages/mypackage", "2012-04-30..."), ...]
          names =
            [ ((prefixPkgURI ++) . display . pkgName . pkgInfoId $ pkg
            , fst . snd . Vec.head . pkgMetadataRevisions $ pkg)
            | pkg <- L.map L.head pkgs
            ]
          nameNodes = SM.pathsAndDatesToNodes names
            "daily" "1.0"

      --  Versioned package pages
      --  nameVers :: [path :: String]
      --  e.g. ["http://myhackage.com/packages/mypackage-1.0.2", ...]
          nameVers =
            [ ((prefixPkgURI ++) . display . pkgName . pkgInfoId $ pkg)
              ++ "-" ++ (display . pkgVersion . pkgInfoId $ pkg)
            | pkg <- concat pkgs
            ]
          nameVersNodes = SM.urlsToNodes nameVers
            pageBuildDate "monthly" "0.25"

      --  Unversioned doc pages - always redirect to latest version.
      --  (for packages with valid documentation)
      basePkgNamesWithDocs <- mapParaM
        (queryHasDocumentation . pkgInfoId) (L.map L.head pkgs)

      --  baseDocs :: [path :: String]
      --  e.g. ["http://myhackage.com/packages/mypackage/docs". ...]
      let baseDocs =
            [ ((prefixPkgURI ++) . display . pkgName . pkgInfoId . fst $ pkg)
              ++ "/docs"
            | pkg <- filter ((== True) . snd) basePkgNamesWithDocs
            ]
          baseDocNodes = SM.urlsToNodes baseDocs
            pageBuildDate "daily" "1.0"

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
          versionedDocNodes = SM.urlsToNodes versionedDocURIs
            pageBuildDate "monthly" "0.25"

      -- Combine and build sitemap
          allNodes = miscNodes
            ++ tagNodes
            ++ nameNodes
            ++ nameVersNodes
            ++ baseDocNodes
            ++ versionedDocNodes
          sitemapXML = XMLResponse $ SM.nodesToSiteMap allNodes

      return $ toResponse sitemapXML

    mapParaM :: Monad m => (a -> m b) -> [a] -> m [(a, b)]
    mapParaM f = mapM (\x -> (,) x `liftM` f x)

