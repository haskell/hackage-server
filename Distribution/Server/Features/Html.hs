module Distribution.Server.Features.Html (
    HtmlFeature(..),
    initHtmlFeature
  ) where


import Distribution.Server.Feature
import Distribution.Server.Features.Core
import Distribution.Server.Features.Packages
import Distribution.Server.Types
import Distribution.Server.Resource
import qualified Distribution.Server.ResourceTypes as Resource
import qualified Distribution.Server.Pages.Package as Pages
import qualified Distribution.Server.Packages.State as State
import qualified Distribution.Server.Distributions.State as State

import Happstack.Server
import Happstack.State (query)
import Distribution.Package
import Distribution.Text (display)
import System.FilePath.Posix ((</>))

data HtmlFeature = HtmlFeature {
    htmlResources :: [Resource]
}

instance HackageFeature HtmlFeature where
    getFeature html = HackageModule
      { featureName = "html"
      , resources   = htmlResources html
      , dumpBackup    = Nothing
      , restoreBackup = Nothing
      }

initHtmlFeature :: CoreFeature -> PackagesFeature -> IO HtmlFeature
initHtmlFeature core pkg = do
    return HtmlFeature
     { htmlResources =
         [ (extendResource . corePackagePage $ coreResource core) { resourceGet = [("html", servePackagePage pkg)] }
         ]
     }

--packagePage :: PackageRender -> [(DistroName, DistroPackageInfo)] -> Maybe URL -> Html
  --  packageRender :: PackageId -> IO (Maybe PackageRender)

servePackagePage :: PackagesFeature -> Config -> DynamicPath -> ServerPart Response
servePackagePage pkg _ dpath = withPackageId dpath $ \pkgid -> require (packageRender pkg pkgid) $ \render -> do
    distributions <- query $ State.PackageStatus (packageName pkgid)
    hasDocs       <- query $ State.HasDocumentation (packageId pkgid)
    let docURL | hasDocs   = Just $ "/package" </> display pkgid </> "documentation"
               | otherwise = Nothing
    ok $ toResponse $ Resource.XHtml $ Pages.packagePage render distributions docURL







