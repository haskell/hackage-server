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
  return $ \coref@CoreFeature{..} docsCore@DocumentationFeature{..} tagsf@TagsFeature{..} -> do
    let feature = sitemapFeature env
                  coref docsCore tagsf
    return feature

sitemapFeature  :: ServerEnv
                -> CoreFeature
                -> DocumentationFeature
                -> TagsFeature
                -> SitemapFeature
sitemapFeature  ServerEnv{..}
                CoreFeature{..}
                DocumentationFeature{..}
                TagsFeature{..}
  = SitemapFeature{..} where

    sitemapFeatureInterface = (emptyHackageFeature "XML sitemap generation") {
      featureResources = [ xmlSitemapResource ]
      , featureState = []
      , featureDesc = "Dynamically generates a sitemap.xml."
    }

    xmlSitemapResource :: Resource
    xmlSitemapResource = (resourceAt "/sitemap.xml") {
      resourceDesc = [(GET, "Returns a dynamically generated sitemap in XML form.")]
    , resourceGet = [("json", generateSitemap)]
    }

    generateSitemap :: DynamicPath -> ServerPartE Response
    generateSitemap _ = do

      -- Misc. pages
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
            , "/packages/uploads"
            , "/users"
            , "/users/register-request"
            , "/users/password-reset"
            , "/upload"
            , "/api"
            , "/new-features"
            ]
          miscNodes = SM.makeURLNodes miscPages "1.0"

      alltags <- queryGetTagList
      let tagPrefixURI = show serverBaseURI ++ "/packages/tag/"
          tagURLs = map ((tagPrefixURI ++) . display . fst) alltags
          tagNodes = SM.makeURLNodes tagURLs "0.5"

      pkgIndex <- queryGetPackageIndex
      let pkgs = PackageIndex.allPackagesByName pkgIndex
          prefixPkgURI = show serverBaseURI ++ "/package/"

      -- Unversioned package pages - always redirect to latest version.
          names = [((prefixPkgURI ++) . display . pkgName . pkgInfoId $ pkg
                  {-, fst . snd . head . pkgMetadataRevisions $ pkg)-}
                  , fst . snd . Vec.head . pkgMetadataRevisions $ pkg)
                  | pkg <- L.map L.head pkgs]
          nameNodes= SM.makeURLNodes' names "1.0"

      -- Versioned package pages
          nameVers = [((prefixPkgURI ++) . display . pkgName . pkgInfoId $ pkg) ++ "-" ++
            (display . pkgVersion . pkgInfoId $ pkg) | pkg <- concat pkgs]
          nameVersNodes = SM.makeURLNodes nameVers "0.25"

      -- Unversioned doc pages (for packages with valid documentation)
      basePkgNamesWithDocs <- mapParaM (queryHasDocumentation . pkgInfoId)
        (L.map L.head pkgs)
      let baseDocs = [((prefixPkgURI ++) . display . pkgName . pkgInfoId . fst $ pkg) ++ "/docs"
            | pkg <- filter ((== True) . snd) basePkgNamesWithDocs]
          baseDocNodes = SM.makeURLNodes baseDocs "1.0"

      -- Versioned doc pages
      versionedDocNames <- mapParaM (queryHasDocumentation . pkgInfoId)
        (concat pkgs)
      let versionedDocURIs = [((prefixPkgURI ++) . display . pkgName . pkgInfoId . fst $ pkg) ++ "-" ++
            (display . pkgVersion . pkgInfoId . fst $ pkg) ++ "/docs"
            | pkg <- filter ((== True) . snd) versionedDocNames]
          versionedDocNodes = SM.makeURLNodes versionedDocURIs "0.25"

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
