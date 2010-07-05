module Distribution.Server.Features.Html (
    HtmlFeature(..),
    initHtmlFeature
  ) where


import Distribution.Server.Feature
import Distribution.Server.Features.Core
import Distribution.Server.Features.Packages
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Check
import Distribution.Server.Features.Users
import Distribution.Server.Types
import Distribution.Server.Resource

import qualified Distribution.Server.ResourceTypes as Resource
import qualified Distribution.Server.Pages.Package as Pages
import qualified Distribution.Server.Packages.State as State
import qualified Distribution.Server.Distributions.State as State

import Distribution.Server.Pages.Template (hackagePage, hackagePageWith)

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

-- useful functions:
--hackagePage = hackagePageWith []
--hackagePageWith :: [Html] -> String -> [Html] -> Html
--hackagePageWith links heading docs = toHtml [header << docHead, body << docBody]

initHtmlFeature :: CoreFeature -> PackagesFeature -> UploadFeature -> CheckFeature -> UserFeature -> IO HtmlFeature
initHtmlFeature core pkg upload check user = do
    let cores = coreResource core
        users = userResource user
        uploads = uploadResource upload
        checks  = checkResource check
    return HtmlFeature
     { htmlResources =
        -- core
         [ (extendResource $ corePackagePage cores) { resourceGet = [("html", servePackagePage pkg)] }
         --, (extendResource $ coreIndexPage cores) { resourceGet = [("html", serveIndexPage)] }, currently in 'core' feature
         --, (extendResource $ corePackagesPage cores) { resourceGet = [("html", servePackageIndex)] }, currently in 'packages' feature
        -- users
         --, (extendResource $ userList users) { resourceGet = [("html", serveUserList users)] }
            -- list of users with user links; if admin, a link to add user page
         -- , (resourceAt $ "/users/register" { resourceGet = [("html", addUserForm users)] }
            -- form to post to /users/
         --, (extendResource $ userPage users) { resourceGet = [("html", serveUserPage)] }
            -- user page with link to password form and list of groups (how to do this?)
         --, (extendResource $ passwordResource users) { resourceGet = [("html", servePasswordForm)] }
            -- form to PUT password
         --, (extendResource $ enabledResource users) { resourceGet = [("html", serveEnabledForm)] }
            -- form to enable or disable users (admin only)
        -- uploads
         --, (extendResource $ uploadIndexPage uploads) { resourcePost = [("html", serveUploadResult)] }
            -- serve upload result as HTML.. er
         --, (resourceAt "/packages/upload") { resourceGet = [("html", serveUploadForm)] }
            -- form for uploading
        -- checks
         --, (extendResource $ candidatePage checks) { resourceGet = [("html", serveCandidatePage)] }
            -- package page for a candidate
         --, (extendResource $ candidatesPage checks) { resourceGet = [("html", serveCandidatesPage)] }
            -- list of all packages which have candidates
         --, (extendResource $ packageCandidatePage checks) { resourceGet = [("html", servePackageCandidatesPage)] }
            -- list of candidates + forms for maintainers to upload
         --, (extendResource $ publishPage checks) { resourceGet = [("html", servePublishForm)] }
            -- form for publishing package
         --, (resourceAt $ "/packages/candidates/upload///") { resourceGet = [("html", serveCandidateUploadForm)] }
            -- form for uploading general candidate
        -- reports
         --, (extendResource $ reportsList reports) { resourceGet = [("html", serveReportsList)] }
         --, (extendResource $ reportsPage reports) { resourceGet = [("html", serveReportsPage)] }
        -- distros
         --, (extendResource $ distroIndexPage distros) { resourceGet = [("html", serveDistroIndex)] }
         --, (extendResource $ distroAllPage distros) { resourceGet = [("html", serveDistroPackages)] }
         --, (extendResource $ distroPackage distros) { resourceGet = [("html", serveDistroPackage)] }

         ]
     }


servePackagePage :: PackagesFeature -> Config -> DynamicPath -> ServerPart Response
servePackagePage pkg _ dpath = withPackageId dpath $ \pkgid -> require (packageRender pkg pkgid) $ \render -> do
    distributions <- query $ State.PackageStatus (packageName pkgid)
    hasDocs       <- query $ State.HasDocumentation (packageId pkgid)
    let docURL | hasDocs   = Just $ "/package" </> display pkgid </> "documentation"
               | otherwise = Nothing
    ok $ toResponse $ Resource.XHtml $ Pages.packagePage render distributions docURL







