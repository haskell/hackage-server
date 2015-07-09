{-# LANGUAGE RecordWildCards #-}


module Distribution.Server.Features.Sitemap (
  SitemapFeature(..)
, initSitemapFeature
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.Core
import Distribution.Server.Features.Documentation

import Distribution.Package
import Distribution.Text (display)
import Distribution.Server.Packages.Types

import qualified Distribution.Server.Features.Sitemap.Functions as SM
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import qualified Data.List as L

data SitemapFeature = SitemapFeature {
  sitemapFeatureInterface :: HackageFeature
}

instance IsHackageFeature SitemapFeature where
  getFeatureInterface = sitemapFeatureInterface

initSitemapFeature :: ServerEnv
                   -> IO ( CoreFeature
                      -> DocumentationFeature
                      -> IO SitemapFeature)
initSitemapFeature env@ServerEnv{..} = do
  return $ \coref@CoreFeature{..} docsCore@DocumentationFeature{..} -> do
    let feature = sitemapFeature env
                  coref docsCore
    return feature

sitemapFeature  :: ServerEnv
                -> CoreFeature
                -> DocumentationFeature
                -> SitemapFeature
sitemapFeature  ServerEnv{..}
                CoreFeature{..}
                DocumentationFeature{..}
  = SitemapFeature{..} where

    sitemapFeatureInterface = (emptyHackageFeature "XML sitemap generation") {
      featureResources = [ xmlSitemapResource ]
    }

    xmlSitemapResource :: Resource
    xmlSitemapResource = (resourceAt "/sitemap.xml") {
      resourceDesc = [(GET, "Returns a dynamically generated sitemap in XML form.")]
    , resourceGet = [("json", generateSitemap)]
    }

    generateSitemap :: DynamicPath -> ServerPartE Response
    generateSitemap _ = do

      pkgIndex <- queryGetPackageIndex
      let pkgs = PackageIndex.allPackagesByName pkgIndex

      -- Unversioned package pages - always redirect to latest version.
          names = [display . pkgName . pkgInfoId $ pkg
            | pkg <- L.map L.head pkgs]
          nameNodes= SM.makeURLNodes names serverBaseURI "1.0"

      -- Versioned package pages
          nameVers = [(display . pkgName . pkgInfoId $ pkg) ++ "-" ++
            (display . pkgVersion . pkgInfoId $ pkg) | pkg <- concat pkgs]
          nameVersNodes = SM.makeURLNodes nameVers serverBaseURI "0.25"

      -- Unversioned doc pages (for packages with valid documentation)
      basePkgNamesWithDocs <- mapParaM (queryHasDocumentation . pkgInfoId)
        (L.map L.head pkgs)
      let baseDocs = [(display . pkgName . pkgInfoId . fst $ pkg) ++ "/docs"
            | pkg <- filter ((== True) . snd) basePkgNamesWithDocs]
          baseDocNodes = SM.makeURLNodes baseDocs serverBaseURI "1.0"

      -- Versioned doc pages
      docsAllVersions <- mapParaM (queryHasDocumentation . pkgInfoId)
        (concat pkgs)
      let versionedDocs = [(display . pkgName . pkgInfoId . fst $ pkg) ++ "-" ++
            (display . pkgVersion . pkgInfoId . fst $ pkg) ++ "/docs"
            | pkg <- filter ((== True) . snd) docsAllVersions]
          versionedDocNodes = SM.makeURLNodes versionedDocs serverBaseURI "0.25"

      -- Combine and build sitemap
          allNodes = nameNodes ++ nameVersNodes ++ baseDocNodes ++ versionedDocNodes
          sitemapXML = XMLResponse $ SM.nodesToSiteMap allNodes

      return $ toResponse sitemapXML

    mapParaM :: Monad m => (a -> m b) -> [a] -> m [(a, b)]
    mapParaM f = mapM (\x -> (,) x `liftM` f x)
