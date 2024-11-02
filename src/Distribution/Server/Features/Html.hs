{-# LANGUAGE RecursiveDo, FlexibleContexts, RankNTypes, NamedFieldPuns, RecordWildCards, LambdaCase #-}
module Distribution.Server.Features.Html (
    HtmlFeature(..),
    initHtmlFeature
  ) where

import Control.Arrow ((&&&))
import Prelude ()
import Distribution.Server.Prelude

import Distribution.Server.Framework
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource
import Distribution.Server.Framework.Templating

import Distribution.Server.Features.Core
import Distribution.Server.Features.Upload
import Distribution.Server.Features.BuildReports
import Distribution.Server.Features.BuildReports.Render
import Distribution.Server.Features.PackageCandidates
import Distribution.Server.Features.Users
import Distribution.Server.Features.DownloadCount
import Distribution.Server.Features.Votes
import Distribution.Server.Features.Search
import Distribution.Server.Features.PreferredVersions
import Distribution.Server.Features.ReverseDependencies
import Distribution.Server.Features.PackageContents (PackageContentsFeature(..))
import Distribution.Server.Features.PackageList
import Distribution.Server.Features.Tags
import Distribution.Server.Features.AnalyticsPixels
import Distribution.Server.Features.Mirror
import Distribution.Server.Features.Distro
import Distribution.Server.Features.Documentation
import Distribution.Server.Features.TarIndexCache
import Distribution.Server.Features.UserDetails
import Distribution.Server.Features.EditCabalFiles
import Distribution.Server.Features.Html.HtmlUtilities
import Distribution.Server.Features.Security.SHA256
import qualified Distribution.Server.Features.BuildReports.BuildReport as BR

import Distribution.Server.Users.Types
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Render
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Users.Group (UserGroup(..))

import qualified Distribution.Server.Pages.Package as Pages
import qualified Distribution.Server.Pages.PackageFromTemplate as PagesNew
import Distribution.Server.Pages.Template
import Distribution.Server.Pages.Util
import qualified Distribution.Server.Pages.Group as Pages
import Distribution.Server.Pages.Reverse (LatestOrOld(..), ReverseHtmlUtil(..), reverseHtmlUtil)
import qualified Distribution.Server.Pages.Index as Pages
import Distribution.Server.Util.CountingMap (cmFind, cmToList)
import Distribution.Server.Util.DocMeta (loadTarDocMeta)
import Distribution.Server.Util.ServeTarball (loadTarEntry)
import Distribution.Simple.Utils ( cabalVersion, toUTF8LBS )

import Distribution.Package
import Distribution.Version
import Distribution.Text (display)

import Data.List (intercalate, intersperse, insert)
import Data.Function (on)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS (ByteString)
import qualified Network.URI as URI

import Text.XHtml.Strict
import qualified Text.XHtml.Strict as XHtml
import Text.XHtml.Table (simpleTable)
import Distribution.PackageDescription (hasLibs)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import qualified Distribution.Server.Pages.Recent as Pages
import qualified Distribution.Server.Util.Paging as Paging
import Distribution.Server.Features.RecentPackages (RecentPackagesFeature (RecentPackagesFeature, getRecentRevisions, getRecentPackages))
import Data.Time (getCurrentTime)
import Text.Read (readMaybe)
import Distribution.Server.Pages.Group (listGroupCompact)


-- TODO: move more of the below to Distribution.Server.Pages.*, it's getting
-- close to 1K lines, way too much... it's okay to keep data-querying in here,
-- but pure HTML generation mostly needlessly clutters up the module.
-- Try to make it so no HTML combinators need to be imported.
--
-- See the TODO file for more ways to improve the HTML.
data HtmlFeature = HtmlFeature {
    htmlFeatureInterface :: HackageFeature
}

instance IsHackageFeature HtmlFeature where
    getFeatureInterface = htmlFeatureInterface

-- This feature provides the HTML view to the models of other features
-- currently it uses the xhtml package to render HTML (Text.XHtml.Strict)
--
-- This means of generating HTML is somewhat temporary, in that a more advanced
-- (and better-looking) HTML ajaxy scheme should come about later on.
initHtmlFeature :: ServerEnv
                -> IO (UserFeature
                    -> CoreFeature
                    -> PackageContentsFeature
                    -> UploadFeature -> PackageCandidatesFeature
                    -> VersionsFeature
                    -> ReverseFeature
                    -> TagsFeature
                    -> AnalyticsPixelsFeature
                    -> DownloadFeature
                    -> VotesFeature
                    -> ListFeature -> SearchFeature
                    -> MirrorFeature -> DistroFeature
                    -> DocumentationFeature
                    -> DocumentationFeature
                    -> TarIndexCacheFeature
                    -> ReportsFeature
                    -> UserDetailsFeature
                    -> RecentPackagesFeature
                    -> IO HtmlFeature)

initHtmlFeature env@ServerEnv{serverTemplatesDir, serverTemplatesMode,
                          serverCacheDelay,
                          serverVerbosity = verbosity} = do
    -- Page templates
    templates <- loadTemplates serverTemplatesMode
                   [serverTemplatesDir, serverTemplatesDir </> "Html"]
                   [ "maintain.html", "maintain-candidate.html"
                   , "reports.html", "report.html", "reports-test.html"
                   , "maintain-docs.html"
                   , "distro-monitor.html"
                   , "revisions.html"
                   , "package-page.html"
                   , "table-interface.html"
                   , "tag-edit.html"
                   , "candidate-page.html"
                   , "candidate-index.html"
                   , "browse.html"
                   , "noscript-search-form.html"
                   , "analytics-pixels-page.html"
                   , "user-analytics-pixels-page.html"
                   , "graph.html"
                   ]


    return $ \user core@CoreFeature{packageChangeHook}
              packages upload
              candidates versions
              reversef
              tags analyticsPixels download
              rank
              list@ListFeature{itemUpdate}
              names mirror
              distros
              docsCore docsCandidates
              tarIndexCache
              reportsCore
              usersdetails
              recentPackagesFeature -> do
      -- do rec, tie the knot
      rec let (feature, packageIndex, packagesPage) =
                htmlFeature env user core
                            packages upload
                            candidates versions
                            reversef
                            tags analyticsPixels download
                            rank
                            list names
                            mirror distros
                            docsCore docsCandidates
                            tarIndexCache
                            reportsCore
                            usersdetails
                            (htmlUtilities core candidates tags user)
                            (reverseHtmlUtil reversef)
                            mainCache namesCache
                            templates
                            recentPackagesFeature

          -- Index page caches
          mainCache  <- newAsyncCacheNF packageIndex
                          defaultAsyncCachePolicy {
                            asyncCacheName = "packages index page (by category)",
                            asyncCacheUpdateDelay  = serverCacheDelay,
                            asyncCacheSyncInit     = False,
                            asyncCacheLogVerbosity = verbosity
                          }

          namesCache <- newAsyncCacheNF packagesPage
                          defaultAsyncCachePolicy {
                            asyncCacheName = "packages index page (by name)",
                            asyncCacheUpdateDelay  = serverCacheDelay,
                            asyncCacheLogVerbosity = verbosity
                          }

      registerHook itemUpdate $ \_ -> do
        prodAsyncCache mainCache  "item update"
        prodAsyncCache namesCache "item update"
      registerHook packageChangeHook $ \_ -> do
        prodAsyncCache mainCache  "package change"
        prodAsyncCache namesCache "package change"

      return feature

htmlFeature :: ServerEnv
            -> UserFeature
            -> CoreFeature
            -> PackageContentsFeature
            -> UploadFeature
            -> PackageCandidatesFeature
            -> VersionsFeature
            -> ReverseFeature
            -> TagsFeature
            -> AnalyticsPixelsFeature
            -> DownloadFeature
            -> VotesFeature
            -> ListFeature
            -> SearchFeature
            -> MirrorFeature
            -> DistroFeature
            -> DocumentationFeature
            -> DocumentationFeature
            -> TarIndexCacheFeature
            -> ReportsFeature
            -> UserDetailsFeature
            -> HtmlUtilities
            -> ReverseHtmlUtil
            -> AsyncCache Response
            -> AsyncCache Response
            -> Templates
            -> RecentPackagesFeature
            -> (HtmlFeature, IO Response, IO Response)

htmlFeature env@ServerEnv{..}
            user
            core@CoreFeature{queryGetPackageIndex}
            packages upload
            candidates versions
            revf@ReverseFeature{..}
            tags analyticsPixels download
            rank
            list@ListFeature{getAllLists}
            names
            mirror distros
            docsCore docsCandidates
            tarIndexCache
            reportsCore
            usersdetails
            utilities@HtmlUtilities{..}
            reverseH@ReverseHtmlUtil{..}
            cachePackagesPage cacheNamesPage
            templates
            recentPackagesFeature
  = (HtmlFeature{..}, packageIndex, packagesPage)
  where
    htmlFeatureInterface = (emptyHackageFeature "html") {
        featureResources = htmlResources
      , featureState     = []
      , featureCaches    = [
           CacheComponent {
             cacheDesc       = "packages page by category",
             getCacheMemSize = memSize <$> readAsyncCache cachePackagesPage
           }
         , CacheComponent {
             cacheDesc       = "packages page by name",
             getCacheMemSize = memSize <$> readAsyncCache cacheNamesPage
           }
         ]
      , featurePostInit = syncAsyncCache cachePackagesPage
      , featureReloadFiles = reloadTemplates templates
      }

    htmlCore       = mkHtmlCore       env
                                      utilities
                                      user
                                      core
                                      versions
                                      upload
                                      tags
                                      analyticsPixels
                                      docsCore
                                      tarIndexCache
                                      reportsCore
                                      download
                                      rank
                                      distros
                                      packages
                                      htmlTags
                                      htmlReverse
                                      revf
                                      htmlPreferred
                                      cachePackagesPage
                                      cacheNamesPage
                                      templates
                                      names
                                      candidates
                                      recentPackagesFeature
    htmlUsers      = mkHtmlUsers      user usersdetails
    htmlUploads    = mkHtmlUploads    utilities upload
    htmlDocUploads = mkHtmlDocUploads utilities core docsCore templates
    htmlDownloads  = mkHtmlDownloads  utilities download
    htmlReports    = mkHtmlReports    utilities core upload user reportsCore templates
    htmlCandidates = mkHtmlCandidates env utilities core versions upload
                                      docsCandidates tarIndexCache
                                      candidates user templates
    htmlPreferred  = mkHtmlPreferred  utilities core versions
    htmlTags       = mkHtmlTags       utilities core upload user list tags templates
    htmlReverse    = mkHtmlReverse    utilities core versions list revf reverseH

    htmlAnalyticsPixels = mkHtmlAnalyticsPixels utilities core user upload analyticsPixels templates

    htmlResources = concat [
        htmlCoreResources       htmlCore
      , htmlUsersResources      htmlUsers
      , htmlUploadsResources    htmlUploads
      , htmlDocUploadsResources htmlDocUploads
      , htmlReportsResources    htmlReports
      , htmlCandidatesResources htmlCandidates
      , htmlPreferredResources  htmlPreferred
      , htmlDownloadsResources  htmlDownloads
      , htmlTagsResources       htmlTags
      , htmlAnalyticsPixelsResources htmlAnalyticsPixels
      , htmlReverseResource     htmlReverse
      -- and user groups. package maintainers, trustees, admins
      , htmlGroupResource user (maintainersGroupResource . uploadResource $ upload)
      , htmlGroupResource user (trusteesGroupResource    . uploadResource $ upload)
      , htmlGroupResource user (uploadersGroupResource   . uploadResource $ upload)
      , htmlGroupResource user (adminResource            . userResource   $ user)
      , htmlGroupResource user (mirrorGroupResource      . mirrorResource $ mirror)
      ]




      -- TODO: write HTML for reports and distros to display the information
      -- effectively reports
      {-
      , (extendResource $ reportsList reports) {
            resourceGet = [("html", serveReportsList)]
          }
      , (extendResource $ reportsPage reports) {
            resourceGet = [("html", serveReportsPage)]
          }
      -}

      -- distros
      {-
      , (extendResource $ distroIndexPage distros) {
            resourceGet = [("html", serveDistroIndex)]
          }
      , (extendResource $ distroAllPage distros) {
            resourceGet = [("html", serveDistroPackages)]
          }
      , (extendResource $ distroPackage distros) {
            resourceGet = [("html", serveDistroPackage)]
          }
      -}

    --------------------------------------------------------------------------------
    -- Additional package indices

    packageIndex :: IO Response
    packageIndex = do
       index <- queryGetPackageIndex
       let htmlIndex = toResponse $ Resource.XHtml $ Pages.packageIndex index
       return htmlIndex

    packagesPage :: IO Response
    packagesPage = do
        items <- liftIO getAllLists
        let htmlpage =
              toResponse $ Resource.XHtml $ hackagePage "All packages by name"
                [ h2 << "All packages by name"
                , ulist ! [theclass "packages"] << map renderItem (Map.elems items)
                ]
        return htmlpage

    {-
    -- Currently unused, mainly because not all web browsers use eager authentication-sending
    -- Setting a cookie might work here, albeit one that's stateless for the server, is not
    -- used for auth and only causes GUI changes, not permission overriding
    loginWidget :: UserResource -> ServerPart Html
    loginWidget user = do
        users <- query State.GetUserDb
        auth  <- Auth.getHackageAuth users
        return . makeLoginWidget user $ case auth of
            Left {} -> Nothing
            Right (_, uinfo) -> Just $ userName uinfo

    makeLoginWidget :: UserResource -> Maybe UserName -> Html
    makeLoginWidget user mname = case mname of
        Nothing -> anchor ! [href $ userLoginUri user Nothing] << "log in"
        Just uname -> anchor ! [href $ userPageUri user "" uname] << display uname
    -}


{-------------------------------------------------------------------------------
  Core
-------------------------------------------------------------------------------}

data HtmlCore = HtmlCore {
    htmlCoreResources :: [Resource]
  }

mkHtmlCore :: ServerEnv
           -> HtmlUtilities
           -> UserFeature
           -> CoreFeature
           -> VersionsFeature
           -> UploadFeature
           -> TagsFeature
           -> AnalyticsPixelsFeature
           -> DocumentationFeature
           -> TarIndexCacheFeature
           -> ReportsFeature
           -> DownloadFeature
           -> VotesFeature
           -> DistroFeature
           -> PackageContentsFeature
           -> HtmlTags
           -> HtmlReverse
           -> ReverseFeature
           -> HtmlPreferred
           -> AsyncCache Response
           -> AsyncCache Response
           -> Templates
           -> SearchFeature
           -> PackageCandidatesFeature
           -> RecentPackagesFeature
           -> HtmlCore
mkHtmlCore ServerEnv{serverBaseURI, serverBlobStore}
           utilities@HtmlUtilities{..}
           UserFeature{queryGetUserDb, checkAuthenticated, guardAuthorised_, adminGroup}
           CoreFeature{coreResource}
           VersionsFeature{ versionsResource
                          , queryGetDeprecatedFor
                          , queryGetPreferredInfo
                          , withPackagePreferred
                          }
           UploadFeature{..}
           TagsFeature{queryTagsForPackage}
           AnalyticsPixelsFeature{getPackageAnalyticsPixels}
           documentationFeature@DocumentationFeature{documentationResource, queryDocumentation}
           TarIndexCacheFeature{cachedTarIndex}
           reportsFeature
           DownloadFeature{recentPackageDownloads,totalPackageDownloads}
           VotesFeature{..}
           DistroFeature{queryPackageStatus}
           PackageContentsFeature{packageRender}
           HtmlTags{..}
           HtmlReverse{..}
           ReverseFeature{queryReverseDeps, revJSON}
           HtmlPreferred{..}
           cachePackagesPage
           cacheNamesPage
           templates
           SearchFeature{..}
           PackageCandidatesFeature{..}
           RecentPackagesFeature{getRecentPackages, getRecentRevisions}
  = HtmlCore{..}
  where
    candidatesCore = candidatesCoreResource
    cores@CoreResource {packageInPath, lookupPackageName, lookupPackageId} = coreResource
    versions = versionsResource
    docs     = documentationResource

    maintainPackage   = (resourceAt "/package/:package/maintain") {
                            resourceGet = [("html", serveMaintainPage)]
                          }

    htmlCoreResources = [
        (extendResource $ corePackagePage cores) {
            resourceDesc = [(GET, "Show detailed package information")]
          , resourceGet  = [("html", servePackagePage)]
          }
      , (resourceAt "/package/:package/dependencies") {
            resourceDesc = [(GET, "Show detailed package dependency information")]
          , resourceGet = [("html", serveDependenciesPage)]
          }
      {-
      , (extendResource $ coreIndexPage cores) {
            resourceGet = [("html", serveIndexPage)]
          }, currently in 'core' feature
      -}
      , (resourceAt "/packages/names" ) {
            resourceGet = [("html", const $ readAsyncCache cacheNamesPage)]
          }
      , (resourceAt "/packages/browse" ) {
            resourceDesc = [(GET, "Show browsable list of all packages")]
          , resourceGet  = [("html", serveBrowsePage)]
          }
      , (extendResource searchPackagesResource) {
                    resourceGet = [("html", serveBrowsePage)]
                  }
      , (resourceAt "/packages/graph.json" ) {
            resourceDesc = [(GET, "Show JSON of package dependency information")]
        , resourceGet = [("json",
                serveGraphJSON)]
          }
      , (resourceAt "/packages/graph" ) {
            resourceDesc = [(GET, "Show graph of package dependency information")]
        , resourceGet = [("html",
                serveGraph)]
          }
      , (extendResource $ corePackagesPage cores) {
            resourceDesc = [(GET, "Show package index")]
          , resourceGet  = [("html", const $ readAsyncCache cachePackagesPage)]
          }
      , maintainPackage
      , (resourceAt "/package/:package/distro-monitor.:format") {
            resourceDesc = [(GET, "A handy page for distro package change monitor tools")]
          , resourceGet  = [("html", serveDistroMonitorPage)]
          }
      , (resourceAt "/package/:package/revisions/.:format") {
            resourceGet  = [("html", serveCabalRevisionsPage)]
          }
      , (resourceAt "/packages/recent.:format") {
            resourceGet = [("html", serveRecentPage),("rss", serveRecentRSS)]
          }
      , (resourceAt "/packages/recent/revisions.:format") {
            resourceGet = [("html", serveRevisionPage), ("rss", serveRevisionRSS)]
          }
      ]

    readParamWithDefaultAndValid :: (Read a, HasRqData m, Monad m, Functor m, Alternative m) =>
      a -> (a -> Bool) -> String -> m a
    readParamWithDefaultAndValid n f queryParam = do
        m <- optional (look queryParam)
        let parsed = m >>= readMaybe >>= (\x -> if f x then Just x else Nothing)

        return $ fromMaybe n parsed

    lookupPageSize :: (HasRqData m, Monad m, Functor m, Alternative m) => Int -> m Int
    lookupPageSize def = readParamWithDefaultAndValid def validPageSize "pageSize"
      where validPageSize x = x > 1 && x <= 200

    lookupPage :: (HasRqData m, Monad m, Functor m, Alternative m) => Int -> m Int
    lookupPage def = readParamWithDefaultAndValid def validPage "page"
      where validPage = (>= 1)

    serveRecentPage :: DynamicPath -> ServerPartE Response
    serveRecentPage _ = do
      recentPackages <- getRecentPackages
      users <- queryGetUserDb
      page <-  lookupPage 1
      pageSize <- lookupPageSize 20

      let conf = Paging.createConf page pageSize recentPackages

      return . toResponse $ Pages.recentPage conf users recentPackages

    serveRecentRSS :: DynamicPath -> ServerPartE Response
    serveRecentRSS _ = do
      recentPackages <- getRecentPackages
      users <- queryGetUserDb
      page <-  lookupPage 1
      pageSize <- lookupPageSize 20
      now   <- liftIO getCurrentTime

      let conf = Paging.createConf page pageSize recentPackages

      return . toResponse $ Pages.recentFeed conf users serverBaseURI now recentPackages

    serveRevisionPage :: DynamicPath -> ServerPartE Response
    serveRevisionPage _ = do
      revisions <- getRecentRevisions
      users <- queryGetUserDb
      page <-  lookupPage 1
      pageSize <- lookupPageSize 40

      let conf = Paging.createConf page pageSize revisions

      return . toResponse $ Pages.revisionsPage conf users revisions

    serveRevisionRSS :: DynamicPath -> ServerPartE Response
    serveRevisionRSS _ = do
      revisions <- getRecentRevisions
      users <- queryGetUserDb
      page <-  lookupPage 1
      pageSize <- lookupPageSize 40
      now   <- liftIO getCurrentTime

      let conf = Paging.createConf page pageSize revisions

      return . toResponse $ Pages.recentRevisionsFeed conf users serverBaseURI now revisions

    serveBrowsePage :: DynamicPath -> ServerPartE Response
    serveBrowsePage _dpath = do
      template <- getTemplate templates "browse.html"
      noscriptForm <- getTemplate templates "noscript-search-form.html"
      terms <- optional (lookText' "terms")
      let
        noscriptFormRendered =
          renderTemplate $ noscriptForm
            [ "ascending" $= True
            , "default" $= True
            , "pageNumber" $= "0"
            , "searchQuery" $= terms
            ]
        pleaseSubmitFragment =
          if terms == mempty
             then ""
             else "<p>To view the search results, please submit this form with your desired sorting preferences.</p>"
      return $ toResponse $ template
          [ "heading" $= "Browse and search packages"
          , templateUnescaped "formFragment" $
              toUTF8LBS pleaseSubmitFragment
              <> noscriptFormRendered
          ]

    serveGraphJSON :: DynamicPath -> ServerPartE Response
    serveGraphJSON _ = do
        graph <- liftIO revJSON
        --TODO: use proper type for graph with ETag
        cacheControl [Public, maxAgeMinutes 30] (etagFromHash graph)
        ok . toResponse $ graph

    serveGraph :: DynamicPath -> ServerPartE Response
    serveGraph _ = do
      cacheControlWithoutETag [Public, maxAgeDays 1] -- essentially static
      template <- getTemplate templates "graph.html"
      return $ toResponse $ template []

    -- Currently the main package page is thrown together by querying a bunch
    -- of features about their attributes for the given package. It'll need
    -- reorganizing to look aesthetic, as opposed to the sleek and simple current
    -- design that takes the 1990s school of web design.
    servePackagePage :: DynamicPath -> ServerPartE Response
    servePackagePage dpath = do
      pkgid <- packageInPath dpath

      withPackagePreferred pkgid $ \pkg pkgs -> do
        render <- liftIO $ packageRender pkg

        let realpkg = rendPkgId render
            pkgname = packageName realpkg
            docURL  = packageDocsContentUri docs realpkg
            execs   = rendExecNames render
            pkgdesc = flattenPackageDescription $ pkgDesc pkg
            maintainers = maintainersGroup pkgname

        prefInfo      <- queryGetPreferredInfo pkgname
        distributions <- queryPackageStatus pkgname
        totalDown     <- cmFind pkgname `liftM` totalPackageDownloads
        recentDown    <- cmFind pkgname `liftM` recentPackageDownloads
        pkgVotes      <- pkgNumVotes pkgname
        pkgScore      <- pkgNumScore pkgname
        auth          <- checkAuthenticated
        userRating    <- case auth of Just (uid,_) -> pkgUserVote pkgname uid; _ -> return Nothing
        mdoctarblob   <- queryDocumentation realpkg
        tags          <- queryTagsForPackage pkgname
        rdeps         <- queryReverseDeps pkgname
        deprs         <- queryGetDeprecatedFor pkgname
        mreadme       <- makeReadme render
        hasDocs       <- queryHasDocumentation documentationFeature realpkg
        mDocPkgId     <- if hasDocs then pure Nothing
                              else latestPackageWithDocumentation documentationFeature prefInfo pkgs
        rptStats      <- queryLastReportStats reportsFeature realpkg
        candidates    <- lookupCandidateName pkgname
        buildStatus   <- renderBuildStatus
          documentationFeature reportsFeature realpkg
        mdocIndex     <- maybe (return Nothing)
          (liftM Just . liftIO . cachedTarIndex) mdoctarblob
        analyticsPixels <- getPackageAnalyticsPixels pkgname
        userDb          <- queryGetUserDb
        maintainerlist  <- liftIO $ queryUserGroup maintainers
        let
          idAndReport = fmap (\(rptId, rpt, _) -> (rptId, rpt)) rptStats
          install = getInstall $ fmap (fst &&& BR.installOutcome . snd) idAndReport
          test    = getTest    $ fmap (        BR.testsOutcome   . snd) idAndReport
          covg = getAvgCovg $ (\(_, _, cvg) -> cvg) =<< rptStats
          loadDocMeta
            | Just doctarblob <- mdoctarblob
            , Just docIndex   <- mdocIndex
            = loadTarDocMeta
                (BlobStorage.filepath serverBlobStore doctarblob)
                docIndex
                realpkg
            | otherwise
            = return Nothing

        mdocMeta <- loadDocMeta

        let infoUrl = fmap (\_ -> preferredPackageUri versions "" pkgname) $
              sumRange prefInfo

        -- Put it all together
        template <- getTemplate templates "package-page.html"
        cacheControlWithoutETag [Public, maxAgeMinutes 5]

        return $ toResponse . template $
          -- IO-related items
          [ "baseurl"           $= show (serverBaseURI { URI.uriScheme = "" })
          , "sbaseurl"          $= show (serverBaseURI { URI.uriScheme = "https:" })
          , "cabalVersion"      $= display cabalVersion
          , "tags"              $= (renderTags tags)
          , "analyticsPixels"   $= map analyticsPixelUrl (Set.toList analyticsPixels)
          , "versions"          $= (PagesNew.renderVersion realpkg
              (classifyVersions prefInfo $ map packageVersion pkgs) infoUrl)
          , "isDeprecatedVersion" $= getVersionStatus prefInfo (packageVersion realpkg) == DeprecatedVersion
          , "totalDownloads"    $= totalDown
          , "hasexecs"          $= not (null execs)
          , "recentDownloads"   $= recentDown
          , "votes"             $= pkgVotes
          , "hasVotes"          $= pkgVotes > 0
          , "hasExecOnly"       $= (not . hasLibs) pkgdesc && (not . null) execs
          , "userRating"        $= userRating
          , "score"             $= pkgScore
          , "hasrdeps"          $= not (rdeps == ([],[]))
          , "rdeps"             $= renderPkgPageDeps rdeps
          , "rdepsummary"       $= renderDeps pkgname rdeps
          , "buildStatus"       $= buildStatus
          , "hasDocs"           $= hasDocs
          , "install"           $= install
          , "test"              $= test
          , "covg"              $= covg
          , "candidates"        $= case candidates of
                                    [] -> [ toHtml "No Candidates"]
                                    _  -> [ PagesNew.commaList $ flip map candidates $ \cand -> anchor ! [href $ corePackageIdUri candidatesCore "" $ packageId cand] << display (packageVersion cand) ]
          , "maintainers"       $= listGroupCompact (map (Users.userIdToName userDb) (Group.toList maintainerlist))
          ] ++
          -- Items not related to IO (mostly pure functions)
          PagesNew.packagePageTemplate render
            mdocIndex mdocMeta mreadme
            docURL mDocPkgId distributions
            deprs
            utilities
            False
      where
        getInstall Nothing = (False, "", "", "")
        getInstall (Just (rptId, buildStatus)) =
          (isBadgeShowing, badgeColor, badgeText, rptUrl)
          where
            BuildReportId rawId = rptId
            rptUrl = "reports/" <> show rawId
            badgeContent BR.InstallOk            = (True, "success", "InstallOk")
            badgeContent (BR.DependencyFailed _) = (True, "critical", "DependencyFailed")
            badgeContent k                       = (True, "critical", show k)
            (isBadgeShowing, badgeColor, badgeText) = badgeContent buildStatus

        getTest (Just BR.Ok)      = (True, "success", "Passed")
        getTest (Just BR.Failed)  = (True, "critical", "Failed")
        getTest _                 = (False, "False", "")

        getAvgCovg :: Maybe BR.BuildCovg -> (Bool, String, Int)
        getAvgCovg Nothing = (False, "", 100)
        getAvgCovg (Just c) = do
              let l = [
                        BR.expressions c
                      , BR.guards (BR.boolean c)
                      , BR.ifConditions (BR.boolean c)
                      , BR.qualifiers (BR.boolean c)
                      , BR.alternatives c
                      , BR.localDeclarations c
                      , BR.topLevel c
                      ]
                  (used,total) = foldl (\(a,b) (x, y) -> (a+x, b+y)) (0,0) l
                  per | total <= 0 = 100
                      | otherwise = (used*100) `div` total
              if per > 66
                then (True, "brightgreen", per)
                else if per > 33
                  then (True, "yellowgreen", per)
                  else (True, "red", per)

    serveDependenciesPage :: DynamicPath -> ServerPartE Response
    serveDependenciesPage dpath = do
      pkgname <- packageInPath dpath
      withPackagePreferred pkgname $ \pkg _ -> do
        cacheControlWithoutETag [Public, maxAgeMinutes 30]
        render <- liftIO $ packageRender pkg
        return $ toResponse $ dependenciesPage False render "docs"

    serveMaintainPage :: DynamicPath -> ServerPartE Response
    serveMaintainPage dpath = do
      pkgname <- packageInPath dpath
      pkgs <- lookupPackageName pkgname
      guardAuthorised_ [InGroup (maintainersGroup pkgname), InGroup trusteesGroup, InGroup adminGroup]
      cacheControl [Public, NoCache] (etagFromHash (length pkgs))
      template <- getTemplate templates "maintain.html"
      return $ toResponse $ template
        [ "pkgname"  $= pkgname
        , "versions" $= map packageId pkgs
        ]

    serveDistroMonitorPage :: DynamicPath -> ServerPartE Response
    serveDistroMonitorPage dpath = do
      pkgname <- packageInPath dpath
      pkgs <- lookupPackageName pkgname
      cacheControl [Public, maxAgeHours 3] (etagFromHash (length pkgs))
      template <- getTemplate templates "distro-monitor.html"
      return $ toResponse $ template
        [ "pkgname"  $= pkgname
        , "versions" $= map packageId pkgs
        ]

    serveCabalRevisionsPage :: DynamicPath -> ServerPartE Response
    serveCabalRevisionsPage dpath = do
      pkginfo  <- packageInPath dpath >>= lookupPackageId
      users    <- queryGetUserDb
      let pkgid        = packageId pkginfo
          pkgname      = packageName pkginfo
          revisions    = reverse $ Vec.toList (pkgMetadataRevisions pkginfo)
          numRevisions = pkgNumRevisions pkginfo

          revchanges   :: [(SHA256Digest, [Change])]
          revchanges   = start revisions where
            start []          = []
            start (curr:rest) = go curr rest

            go curr [] = [(sha256 (cabalFileByteString (fst curr)), [])]
            go curr (prev:rest) =
                ( sha256 (cabalFileByteString (fst curr))
                , changes curr prev )
                : go prev rest

            changes curr prev = either (const []) id $
              diffCabalRevisionsByteString
                (cabalFileByteString (fst prev))
                (cabalFileByteString (fst curr))

      cacheControl [NoCache] (etagFromHash numRevisions)
      template <- getTemplate templates "revisions.html"
      return $ toResponse $ template
        [ "pkgname"   $= pkgname
        , "pkgid"     $= pkgid
        , "revisions" $= zipWith3 (revisionToTemplate users)
                                  (map snd revisions)
                                  [numRevisions-1, numRevisions-2..]
                                  revchanges
        ]
      where
        revisionToTemplate :: Users.Users -> UploadInfo -> Int
                           -> (SHA256Digest, [Change])
                           -> TemplateVal
        revisionToTemplate users (utime, uid) revision (sha256hash, changes) =
          let uname = Users.userIdToName users uid
           in templateDict
                [ templateVal "number" revision
                , templateVal "sha256" (show sha256hash)
                , templateVal "user" (display uname)
                , utcTimeTemplateVal "htmltime" utime
                , templateVal "changes" changes
                ]


-- | Common helper used by 'serveCandidatePage' and 'servePackagePage'
makeReadme :: MonadIO m => PackageRender -> m (Maybe BS.ByteString)
makeReadme render = case rendReadme render of
  Just (tarfile, _, offset, _) ->
        either (\_err -> return Nothing) (return . Just . snd) =<<
          liftIO (loadTarEntry tarfile offset)
  Nothing -> return Nothing

{-------------------------------------------------------------------------------
  Users
-------------------------------------------------------------------------------}

data HtmlUsers = HtmlUsers {
    htmlUsersResources :: [Resource]
  }

mkHtmlUsers :: UserFeature -> UserDetailsFeature -> HtmlUsers
mkHtmlUsers UserFeature{..} UserDetailsFeature{..} = HtmlUsers{..}
  where
    users = userResource


    htmlUsersResources = [
        -- list of users with user links; if admin, a link to add user page
        (extendResource $ userList users) {
            resourceDesc = [ (GET,  "list of users")
                           , (POST, "create a new user")
                           ]
          , resourceGet  = [ ("html", serveUserList) ]
          , resourcePost = [ ("html", \_ -> adminAddUser) ]
          }
        -- form to post to /users/
      , (resourceAt "/users/register") {
            resourceDesc = [ (GET, "show \"add user\" form") ]
          , resourceGet  = [ ("html", addUserForm) ]
          }
        -- user page with link to password form and list of groups (how to do this?)
      , (extendResource $ userPage users) {
            resourceDesc   = [ (GET,    "show user page") ]
          , resourceGet    = [ ("html", serveUserPage) ]
          }
        -- form to PUT password
      , (extendResource $ passwordResource users) {
            resourceDesc = [ (GET, "show password change form")
                           , (PUT, "change password")
                           ]
          , resourceGet  = [ ("html", servePasswordForm) ]
          , resourcePut  = [ ("html", servePutPassword) ]
          }
      ]

    serveUserList :: DynamicPath -> ServerPartE Response
    serveUserList _ = do
        userlist <- Users.enumerateActiveUsers <$> queryGetUserDb
        let hlist = unordList
                      [ anchor ! [href $ userPageUri users "" uname] << display uname
                      | (_, uinfo) <- userlist, let uname = userName uinfo ]
        ok $ toResponse $ Resource.XHtml $ hackagePage "Hackage users" [h2 << "Hackage users", hlist]

    serveUserPage :: DynamicPath -> ServerPartE Response
    serveUserPage dpath = do
      uname    <- userNameInPath dpath
      uid      <- lookupUserName uname
      udetails <- queryUserDetails uid
      let realname = maybe (display uname) (T.unpack . accountName) udetails
      uris     <- getGroupIndex uid
      uriPairs <- forM uris $ \uri -> do
          desc <- getIndexDesc uri
          return $ Pages.renderGroupName desc (Just uri)
      return $ toResponse $ Resource.XHtml $ hackagePage realname
        [ h2 << realname
        , case uriPairs of
              [] -> noHtml
              _  -> toHtml
                [ toHtml $ display uname ++ " is part of the following groups:"
                , unordList uriPairs
                ]
        , hr
        , anchor ! [href $ manageUserUri users "" uname] <<
            "Click here to manage this account"
        ]

    addUserForm :: DynamicPath -> ServerPartE Response
    addUserForm _ =
        return $ toResponse $ Resource.XHtml $ hackagePage "Register account"
          [ paragraph << "Administrators can register new user accounts here."
          , form ! [theclass "box", XHtml.method "post", action $ userListUri users ""] <<
                [ simpleTable [] []
                    [ makeInput [thetype "text"] "username" "User name"
                    , makeInput [thetype "password"] "password" "Password"
                    , makeInput [thetype "password"] "repeat-password" "Confirm password"
                    ]
                , paragraph << input ! [thetype "submit", value "Create user"]
                ]
          ]

    servePasswordForm :: DynamicPath -> ServerPartE Response
    servePasswordForm dpath = do
      uname   <- userNameInPath dpath
      pathUid <- lookupUserName uname
      uid <- guardAuthenticated -- FIXME: why are we duplicating auth decisions in this feature?
      canChange <- canChangePassword uid pathUid
      case canChange of
          False -> errForbidden "Can't change password" [MText "You're neither this user nor an admin."]
          True -> return $ toResponse $ Resource.XHtml $ hackagePage "Change password"
            [ toHtml "Change your password. You'll be prompted for authentication upon submission, if you haven't logged in already."
            , form ! [theclass "box", XHtml.method "post", action $ userPasswordUri userResource "" uname] <<
                  [ simpleTable [] []
                      [ makeInput [thetype "password"] "password" "Password"
                      , makeInput [thetype "password"] "repeat-password" "Confirm password"
                      ]
                  , paragraph << [ hidden "_method" "PUT" --method override
                                 , input ! [thetype "submit", value "Change password"] ]
                  ]
            ]

    servePutPassword :: DynamicPath -> ServerPartE Response
    servePutPassword dpath = do
      uname <- userNameInPath dpath
      changePassword uname
      return $ toResponse $ Resource.XHtml $ hackagePage "Changed password"
          [toHtml "Changed password for ", anchor ! [href $ userPageUri users "" uname] << display uname]

{-------------------------------------------------------------------------------
  Uploads(For new package lifecycle, this might need to be removed)
-------------------------------------------------------------------------------}

data HtmlUploads = HtmlUploads {
    htmlUploadsResources :: [Resource]
  }

mkHtmlUploads :: HtmlUtilities -> UploadFeature -> HtmlUploads
mkHtmlUploads HtmlUtilities{..} UploadFeature{..} = HtmlUploads{..}
  where
    uploads = uploadResource

    htmlUploadsResources = [
      -- uploads
        -- serve upload result as HTML
        (extendResource $ uploadIndexPage uploads) {
            resourceDesc = [(POST, "Upload package")]
          , resourcePost = [("html", serveUploadResult)]
          }
        -- form for uploading
      , (resourceAt "/packages/upload") {
            resourceGet = [("html", serveUploadForm)]
          }
      ]

    serveUploadForm :: DynamicPath -> ServerPartE Response
    serveUploadForm _ = do
        return $ toResponse $ Resource.XHtml $ hackagePage "Upload package"
          [ h2 << "Upload package"
          , paragraph << [toHtml "See also the ", anchor ! [href "/upload"] << "upload help page", toHtml "."]
          , form ! [theclass "box", XHtml.method "post", action "/packages/", enctype "multipart/form-data"] <<
                [ input ! [thetype "file", name "package"]
                , input ! [thetype "submit", value "Upload package"]
                ]
          , paragraph << [toHtml "If you want to deauthenticate first, ", anchor ! [href "/packages/deauth"] << "click here", toHtml "."]
          ]

    serveUploadResult :: DynamicPath -> ServerPartE Response
    serveUploadResult _ = do
        res <- uploadPackage
        let warns = uploadWarnings res
            pkgid = packageId (uploadDesc res)
        return $ toResponse $ Resource.XHtml $ hackagePage "Upload successful" $
          [ paragraph << [toHtml "Successfully uploaded ", packageLink pkgid, toHtml "!"]
          ] ++ case warns of
            [] -> []
            _  -> [paragraph << "There were some warnings:", unordList warns]

{-------------------------------------------------------------------------------
  Documentation uploads
-------------------------------------------------------------------------------}

data HtmlDocUploads = HtmlDocUploads {
    htmlDocUploadsResources :: [Resource]
  }

mkHtmlDocUploads :: HtmlUtilities -> CoreFeature -> DocumentationFeature -> Templates -> HtmlDocUploads
mkHtmlDocUploads HtmlUtilities{..} CoreFeature{coreResource} DocumentationFeature{..} templates = HtmlDocUploads{..}
  where
    CoreResource{packageInPath} = coreResource

    htmlDocUploadsResources = [
        (extendResource $ packageDocsWhole documentationResource) {
            resourcePut    = [ ("html", serveUploadDocumentation) ]
          , resourceDelete = [ ("html", serveDeleteDocumentation) ]
          }
      , (resourceAt "/package/:package/maintain/docs") {
            resourceGet = [("html", serveDocUploadForm)]
          }
      ]

    serveUploadDocumentation :: DynamicPath -> ServerPartE Response
    serveUploadDocumentation dpath = do
        pkgid <- packageInPath dpath
        uploadDocumentation dpath >> ignoreFilters  -- Override 204 No Content
        return $ toResponse $ Resource.XHtml $ hackagePage "Documentation uploaded"
          [ paragraph << [toHtml "Successfully uploaded documentation for ", packageLink pkgid, toHtml "!"]
          ]

    serveDeleteDocumentation :: DynamicPath -> ServerPartE Response
    serveDeleteDocumentation dpath = do
        pkgid <- packageInPath dpath
        deleteDocumentation dpath >> ignoreFilters -- Override 204 No Content
        return $ toResponse $ Resource.XHtml $ hackagePage "Documentation deleted"
          [ paragraph << [toHtml "Successfully deleted documentation for ", packageLink pkgid, toHtml "!"]
          ]

    serveDocUploadForm :: DynamicPath -> ServerPartE Response
    serveDocUploadForm dpath = do
        pkgid <- packageInPath dpath
        template <- getTemplate templates "maintain-docs.html"
        return $ toResponse $ template
          [ "pkgid"     $= (pkgid :: PackageIdentifier)
          , "actionUrl" $= ""
          ]

{-------------------------------------------------------------------------------
  Build reports
-------------------------------------------------------------------------------}

data HtmlReports = HtmlReports {
    htmlReportsResources :: [Resource]
  }

mkHtmlReports :: HtmlUtilities -> CoreFeature -> UploadFeature -> UserFeature -> ReportsFeature -> Templates -> HtmlReports
mkHtmlReports HtmlUtilities{..} CoreFeature{..} UploadFeature{..} UserFeature{..} ReportsFeature{..} templates = HtmlReports{..}
  where
    CoreResource{packageInPath, guardValidPackageId} = coreResource
    ReportsResource{..} = reportsResource

    htmlReportsResources = [
        (extendResource reportsList) {
            resourceGet = [ ("html", servePackageReports) ]
          }
      , (extendResource reportsPage) {
            resourceGet = [ ("html", servePackageReport) ]
          }
      , (extendResource reportsTestsEnabled) {
            resourceGet = [ ("html", servePackageReportTests) ]
          }
      ]

    servePackageReports :: DynamicPath -> ServerPartE Response
    servePackageReports dpath = packageReports dpath $ \reports -> do
        pkgid <- packageInPath dpath
        cacheControl [Public, maxAgeMinutes 30] (etagFromHash (length reports))
        template <- getTemplate templates "reports.html"
        details <- pkgReportDetails (pkgid,True)
        let status = case BR.failCnt details of
              Nothing -> "Not yet tried."
              Just BR.BuildOK -> "Built successfully."
              Just (BR.BuildFailCnt 1) -> "1 consecutive failure."
              Just (BR.BuildFailCnt c) -> show c ++ " consecutive failures."
        return $ toResponse $ template
          [ "pkgid"   $= (pkgid :: PackageIdentifier)
          , "reports" $= reports
          , "status"  $= status
          ]

    servePackageReport :: DynamicPath -> ServerPartE Response
    servePackageReport dpath = do
        (repid, report, mlog, mtest, covg) <- packageReport dpath
        mlog' <- traverse queryBuildLog mlog
        mtest' <- traverse queryTestLog mtest
        let covg' = fmap getCvgDet covg
        pkgid <- packageInPath dpath
        cacheControlWithoutETag [Public, maxAgeDays 30]
        template <- getTemplate templates "report.html"
        return $ toResponse $ template
          [ "pkgid" $= (pkgid :: PackageIdentifier)
          , "report" $= (repid, report)
          , "log" $= toMessage <$> mlog'
          , "test" $= toMessage <$> mtest'
          , "covg" $= covg'
          ]
      where
        getCvgDet c = (
            det $ BR.expressions c,
            det $ BR.guards $ BR.boolean c,
            det $ BR.ifConditions $ BR.boolean c,
            det $ BR.qualifiers $ BR.boolean c,
            det $ BR.alternatives c,
            det $ BR.localDeclarations c,
            det $ BR.topLevel c
          )

        det::(Int,Int)->(Int,Int,Int)
        det (_,0) = (100,0,0)
        det (a,b) = ((a * 100) `div` b ,a,b)

    servePackageReportTests :: DynamicPath -> ServerPartE Response
    servePackageReportTests dpath = do
        pkgid <- packageInPath dpath
        guardValidPackageId pkgid
        guardAuthorised_ [InGroup (maintainersGroup (packageName pkgid)), InGroup trusteesGroup]
        template <- getTemplate templates "reports-test.html"
        runTests <- queryRunTests pkgid
        return $ toResponse $ template
          [ "pkgid"    $= pkgid
          , "runTests" $= runTests
          ]

{-------------------------------------------------------------------------------
  Candidates
-------------------------------------------------------------------------------}

data HtmlCandidates = HtmlCandidates {
    htmlCandidatesResources :: [Resource]
  }

mkHtmlCandidates :: ServerEnv
                 -> HtmlUtilities
                 -> CoreFeature
                 -> VersionsFeature
                 -> UploadFeature
                 -> DocumentationFeature
                 -> TarIndexCacheFeature
                 -> PackageCandidatesFeature
                 -> UserFeature
                 -> Templates
                 -> HtmlCandidates
mkHtmlCandidates ServerEnv{..} utilities@HtmlUtilities{..}
                 CoreFeature{ coreResource = CoreResource{packageInPath}
                            , queryGetPackageIndex
                            }
                 VersionsFeature{ queryGetPreferredInfo }
                 UploadFeature{..}
                 DocumentationFeature{documentationResource, queryDocumentation,..}
                 TarIndexCacheFeature{cachedTarIndex}
                 PackageCandidatesFeature{..}
                 UserFeature{ guardAuthorised, guardAuthorised_, queryGetUserDb }
                 templates = HtmlCandidates{..}
  where
    candidates     = candidatesResource
    candidatesCore = candidatesCoreResource
    docs           = documentationResource

    pkgCandUploadForm = (resourceAt "/package/:package/candidate/upload") {
                            resourceGet = [("html", servePackageCandidateUpload)]
                          }
    candMaintainForm  = (resourceAt "/package/:package/candidate/maintain") {
                            resourceGet = [("html", serveCandidateMaintain)]
                          }
    candDocUploadForm = (resourceAt "/package/:package/candidate/maintain/docs"){
                            resourceGet = [("html", serveCandDocUploadForm)]
                          }

    htmlCandidatesResources = [
      -- candidates
        -- list of all packages which have candidates
        (extendResource $ corePackagesPage candidatesCore) {
            resourceDesc = [ (GET, "Show all package candidates")
                           , (POST, "Upload a new candidate")
                           ]
          , resourceGet  = [ ("html", serveCandidatesPage) ]
          , resourcePost = [ ("html", \_ -> postCandidate) ]
          }
        -- TODO: use custom functions, not htmlResponse
      , (extendResource $ packageCandidatesPage candidates) {
            resourceDesc = [ (GET, "Show candidate upload form")
                           , (POST, "Upload new package candidate")
                           ]
          , resourceGet  = [ ("html", servePackageCandidates pkgCandUploadForm) ]
          , resourcePost = [ ("", postPackageCandidate) ]
          }
        -- package page for a candidate
      , (extendResource $ corePackagePage candidatesCore) {
            resourceDesc   = [ (GET, "Show candidate maintenance form")
                             , (PUT, "Upload new package candidate")
                             , (DELETE, "Delete a package candidate")
                             ]
          , resourceGet    = [("html", serveCandidatePage candMaintainForm)]
          , resourcePut    = [("html", putPackageCandidate)]
          , resourceDelete = [("html", doDeleteCandidate)]
          }
      , (resourceAt "/package/:package/candidate/dependencies") {
            resourceDesc = [(GET, "Show detailed candidate dependency information")]
          , resourceGet = [("html", serveDependenciesPage)]
          }
        -- form for uploading candidate
      , (resourceAt "/packages/candidates/upload") {
            resourceDesc = [ (GET, "Show package candidate upload form") ]
          , resourceGet  = [ ("html", serveCandidateUploadForm) ]
          }
        -- form for uploading candidate for a specific package version
      , pkgCandUploadForm
        -- maintenance for candidate packages
      , candMaintainForm
        -- form for uploading documentation for a candidate
      , candDocUploadForm
        -- form for publishing package
      , (extendResource $ publishPage candidates) {
           resourceDesc = [ (GET, "Show candidate publish form")
                          , (POST, "Publish a package candidate")
                          ]
         , resourceGet  = [ ("html", servePublishForm) ]
         , resourcePost = [ ("html", servePostPublish) ]
         }
      , (extendResource $ deletePage candidates) {
           resourceDesc = [ (GET, "Show candidate deletion form")
                          , (POST, "Delete a package candidate")
                          ]
         , resourceGet  = [ ("html", serveDeleteForm) ]
         , resourcePost = [ ("html", doDeleteCandidate) ]
         }
        -- form for deleting candidates
      , (extendResource $ deleteCandidatesPage candidates) {
            resourceDesc = [ (GET, "Show package candidates delete form")
                           , (POST, "Delete package candidates") ]
          , resourceGet  = [ ("html", serveCandidatesDeleteForm) ]
          , resourcePost = [ ("html", doDeleteCandidates) ]
          }
      , (extendResource $ packageDocsWhole docs) {
            resourcePut    = [ ("html", serveCandUploadDocumentation) ]
          , resourceDelete = [ ("html", serveCandDeleteDocumentation) ]
          }
      ]

    serveCandidateUploadForm :: DynamicPath -> ServerPartE Response
    serveCandidateUploadForm _ = do
        return $ toResponse $ Resource.XHtml $ hackagePage "Checking and uploading candidates"
          [ h2 << "Checking and uploading candidates"
          , paragraph << [toHtml "See also the ", anchor ! [href "/upload"] << "upload help page", toHtml "."]
          , form ! [theclass "box", XHtml.method "post", action "/packages/candidates/", enctype "multipart/form-data"] <<
                [ input ! [thetype "file", name "package"]
                , input ! [thetype "submit", value "Upload candidate"]
                ]
          ]

    servePackageCandidateUpload :: DynamicPath -> ServerPartE Response
    servePackageCandidateUpload _ = do
        return $ toResponse $ Resource.XHtml $ hackagePage "Checking and uploading candidates"
          [ form ! [theclass "box", XHtml.method "post", action "/packages/candidates/", enctype "multipart/form-data"] <<
                [ input ! [thetype "file", name "package"]
                , input ! [thetype "submit", value "Upload candidate"]
                ]
          ]

    guardAuthorisedAsMaintainerOrTrustee pkgname =
      guardAuthorised_ [InGroup (maintainersGroup pkgname), InGroup trusteesGroup]

    serveCandidateMaintain :: DynamicPath -> ServerPartE Response
    serveCandidateMaintain dpath = do
      pkgid <- packageInPath dpath
      candidate <- packageInPath dpath >>= lookupCandidateId
      guardAuthorisedAsMaintainerOrTrustee (packageName candidate)
      template <- getTemplate templates "maintain-candidate.html"
      return $ toResponse $ template
        [ "pkgname"    $= packageName candidate
        , "pkgversion" $= packageVersion candidate
        , "pkgid"   $= (pkgid :: PackageIdentifier)
        ]
    {-some useful URIs here: candidateUri check "" pkgid, packageCandidatesUri check "" pkgid, publishUri check "" pkgid-}

    serveCandDocUploadForm :: DynamicPath -> ServerPartE Response
    serveCandDocUploadForm dpath = do
        pkgid <- packageInPath dpath
        template <- getTemplate templates "maintain-docs.html"
        return $ toResponse $ template
          [ "pkgid" $= (pkgid :: PackageIdentifier)
          , "actionUrl" $= "candidate/"
          ]

    serveCandidatePage :: Resource -> DynamicPath -> ServerPartE Response
    serveCandidatePage maintain dpath = do
      cand <- packageInPath dpath >>= lookupCandidateId
      template <- getTemplate templates "candidate-page.html"
      candRender <- liftIO $ candidateRender cand
      let PackageIdentifier pkgname version = packageId cand
          render = candPackageRender candRender
      otherVersions <- map packageVersion
                     . flip PackageIndex.lookupPackageName pkgname
                   <$> queryGetPackageIndex
      prefInfo <- queryGetPreferredInfo pkgname
      let maintainHtml = anchor ! [href $ renderResource maintain [display $ packageId cand]] << "maintain"
      -- bottom sections, currently documentation and readme
      mdoctarblob <- queryDocumentation (packageId cand)
      mdocIndex   <- maybe (return Nothing)
                           (liftM Just . liftIO . cachedTarIndex)
                           mdoctarblob

      mreadme     <- makeReadme render
      let loadDocMeta
            | Just doctarblob <- mdoctarblob
            , Just docIndex   <- mdocIndex
            = loadTarDocMeta
                (BlobStorage.filepath serverBlobStore doctarblob)
                docIndex
                (packageId cand)
            | otherwise
            = return Nothing
      mdocMeta <- loadDocMeta

      let docURL = packageDocsContentUri docs (packageId cand)

      -- also utilize hasIndexedPackage :: Bool
      let warningBox = case renderWarnings candRender of
              [] -> []
              warn -> [thediv ! [theclass "candidate-warn"] << [paragraph << strong (toHtml "Warnings:"), unordList warn]]

      let maintainers = maintainersGroup pkgname
      userDb          <- queryGetUserDb
      maintainerlist  <- liftIO $ queryUserGroup maintainers

      return $ toResponse . template $
        [ "versions"          $= (PagesNew.renderVersion (packageId cand) (classifyVersions prefInfo $ insert version otherVersions) Nothing)
        , "maintainHtml"      $= [maintainHtml]
        , "warningBox"        $= warningBox
        , "maintainers"       $= listGroupCompact (map (Users.userIdToName userDb) (Group.toList maintainerlist))
        ] ++
        PagesNew.packagePageTemplate render
            mdocIndex mdocMeta mreadme
            docURL Nothing [] Nothing
            utilities
            True

    serveDependenciesPage :: DynamicPath -> ServerPartE Response
    serveDependenciesPage dpath = do
      candId <- packageInPath dpath
      candRender <- liftIO . candidateRender =<< lookupCandidateId candId
      let render = candPackageRender candRender
      return $ toResponse $ dependenciesPage True render "docs"

    guardAuthorisedAsMaintainer pkgName = guardAuthorised [InGroup . maintainersGroup $ pkgName]

    servePublishForm :: DynamicPath -> ServerPartE Response
    servePublishForm dpath = do
      candidate <- packageInPath dpath >>= lookupCandidateId
      uid <- guardAuthorisedAsMaintainer (packageName candidate)

      let pkgid = packageId candidate
      packages <- queryGetPackageIndex
      checkPublish uid packages candidate >>= \case
          Just err -> throwError err
          Nothing  -> do
              return $ toResponse $ Resource.XHtml $ hackagePage "Publishing candidates"
                  [form ! [theclass "box", XHtml.method "post", action $ publishUri candidates "" pkgid]
                      << input ! [thetype "submit", value "Publish package"]]

    serveCandidatesPage :: DynamicPath -> ServerPartE Response
    serveCandidatesPage _ = do
      template <- getTemplate templates "candidate-index.html"
      cands <- queryGetCandidateIndex
      return $ toResponse . template $
        PagesNew.candidatesPageTemplate cands candidates candidatesCore

    servePackageCandidates :: Resource -> DynamicPath -> ServerPartE Response
    servePackageCandidates candPkgUp dpath = do
      pkgname <- packageInPath dpath
      pkgs <- lookupCandidateName pkgname
      let delUri = "/package/"++(display pkgname)++"/candidates/delete"
      return $ toResponse $ Resource.XHtml $ hackagePage "Package candidates" $
        [ h3 << ("Candidates for " ++ display pkgname) ] ++
        case pkgs of
          [] -> [ toHtml "No candidates exist for ", packageNameLink pkgname, toHtml ". Upload one for "
                , anchor ! [href $ renderResource candPkgUp [display pkgname]] << "this"
                , toHtml " or "
                , anchor ! [href "/packages/candidates/upload"] << "another"
                , toHtml " package?"
                ]
          _  -> [ unordList $ flip map pkgs $ \pkg -> anchor ! [href $ corePackageIdUri candidatesCore "" $ packageId pkg] << display (packageVersion pkg)
                , anchor ! [href delUri]<< "Delete All Candidates"]

    servePostPublish :: DynamicPath -> ServerPartE Response
    servePostPublish dpath = do
        uresult <- publishCandidate dpath True
        return $ toResponse $ Resource.XHtml $ hackagePage "Publish successful" $
          [ paragraph << [toHtml "Successfully published ", packageLink (packageId $ uploadDesc uresult), toHtml "!"]
          ] ++ case uploadWarnings uresult of
            [] -> []
            warns -> [paragraph << "There were some warnings:", unordList warns]

    serveDeleteForm :: DynamicPath -> ServerPartE Response
    serveDeleteForm dpath = do
      candidate <- packageInPath dpath >>= lookupCandidateId
      guardAuthorisedAsMaintainerOrTrustee (packageName candidate)
      let pkgid = packageId candidate
      return $ toResponse $ Resource.XHtml $ hackagePage "Deleting candidates"
                  [form ! [theclass "box", XHtml.method "post", action $ deleteUri candidates "" pkgid]
                      << input ! [thetype "submit", value "Delete package candidate"]]

    serveCandidatesDeleteForm :: DynamicPath -> ServerPartE Response
    serveCandidatesDeleteForm dpath = do
      pkgname <- packageInPath dpath
      guardAuthorisedAsMaintainerOrTrustee pkgname
      -- let pkgname = packageName pkgid
      return $ toResponse $ Resource.XHtml $ hackagePage "Deleting package candidates"
                  [form ! [theclass "box", XHtml.method "post", action $ deleteCandidatesUri candidates "" pkgname]
                      << input ! [thetype "submit", value "Delete All Candidates For This Package"]]

    serveCandUploadDocumentation :: DynamicPath -> ServerPartE Response
    serveCandUploadDocumentation dpath = do
        pkgid <- packageInPath dpath
        uploadDocumentation dpath >> ignoreFilters  -- Override 204 No Content
        return $ toResponse $ Resource.XHtml $ hackagePage "Documentation uploaded"
          [ paragraph << [toHtml "Successfully uploaded documentation for ", candidateLink pkgid, toHtml "!"]
          ]

    serveCandDeleteDocumentation :: DynamicPath -> ServerPartE Response
    serveCandDeleteDocumentation dpath = do
        pkgid <- packageInPath dpath
        deleteDocumentation dpath >> ignoreFilters -- Override 204 No Content
        return $ toResponse $ Resource.XHtml $ hackagePage "Documentation deleted"
          [ paragraph << [toHtml "Successfully deleted documentation for ", candidateLink pkgid, toHtml "!"]
          ]

dependenciesPage :: Bool -> PackageRender -> URL -> Resource.XHtml
dependenciesPage isCandidate render docURL =
    Resource.XHtml $ hackagePage (pkg ++ ": dependencies") $
      [h1 << heading, Pages.renderDetailedDependencies render]
       ++ Pages.renderPackageFlags render docURL
  where
    pkg = display $ rendPkgId render
    heading = "Dependencies for " +++ anchor ! [href link] << pkg
    link = "/package/" ++ pkg
            ++ if isCandidate then "/candidate" else ""


{-------------------------------------------------------------------------------
  Preferred versions
-------------------------------------------------------------------------------}

data HtmlPreferred = HtmlPreferred {
    htmlPreferredResources :: [Resource]
  , editPreferred :: Resource
  , editDeprecated :: Resource
  }

mkHtmlPreferred :: HtmlUtilities
                -> CoreFeature
                -> VersionsFeature
                -> HtmlPreferred
mkHtmlPreferred HtmlUtilities{..}
                CoreFeature{ coreResource = CoreResource{
                               packageInPath
                             , lookupPackageName
                             }
                           }
                VersionsFeature{..} = HtmlPreferred{..}
  where
    versions = versionsResource

    editDeprecated    = (resourceAt "/package/:package/deprecated/edit") {
                            resourceGet = [("html", serveDeprecateForm)]
                          }
    editPreferred     = (resourceAt "/package/:package/preferred/edit") {
                            resourceGet = [("html", servePreferForm)]
                          }

    htmlPreferredResources = [
      -- preferred versions
        editDeprecated
      , editPreferred
      , (extendResource $ preferredResource versions) {
            resourceGet = [("html", servePreferredSummary)]
          }
      , (extendResource $ preferredPackageResource versions) {
            resourceGet = [("html", servePackagePreferred editPreferred)]
          , resourcePut = [("html", servePutPreferred)]
          }
      , (extendResource $ deprecatedResource versions) {
            resourceGet = [("html", serveDeprecatedSummary)]
          }
      , (extendResource $ deprecatedPackageResource versions) {
            resourceGet = [("html", servePackageDeprecated editDeprecated)]
          , resourcePut = [("html", servePutDeprecated )]
          }
      ]

    -- This feature is in great need of a Pages module
    serveDeprecatedSummary :: DynamicPath -> ServerPartE Response
    serveDeprecatedSummary _ = doDeprecatedsRender >>= \renders -> do
        return $ toResponse $ Resource.XHtml $ hackagePage "Deprecated packages"
          [ h2 << "Deprecated packages"
          , unordList $ flip map renders $ \(pkg, pkgs) -> [ packageNameLink pkg, toHtml ": ", deprecatedText pkgs ]
          ]

    deprecatedText :: [PackageName] -> Html
    deprecatedText []   = toHtml "deprecated"
    deprecatedText pkgs = toHtml
      [ toHtml "deprecated in favor of "
      , concatHtml $ intersperse (toHtml ", ") (map packageNameLink pkgs)
      ]

    servePackageDeprecated :: Resource -> DynamicPath -> ServerPartE Response
    servePackageDeprecated deprEdit dpath = do
      pkgname <- packageInPath dpath
      mpkg <- doDeprecatedRender pkgname
      return $ toResponse $ Resource.XHtml $ hackagePage "Deprecated status"
        [ h2 << "Deprecated status"
        , paragraph <<
            [ toHtml $ case mpkg of
                  Nothing   -> [packageNameLink pkgname, toHtml " is not deprecated"]
                  Just pkgs -> [packageNameLink pkgname, toHtml " is ", deprecatedText pkgs]
            , thespan ! [thestyle "color: gray"] <<
                [ toHtml " [maintainers: "
                , anchor ! [href $ renderResource deprEdit [display pkgname]] << "edit"
                , toHtml "]" ]
            ]
        ]

    servePreferredSummary :: DynamicPath -> ServerPartE Response
    servePreferredSummary _ = doPreferredsRender >>= \renders -> do
        return $ toResponse $ Resource.XHtml $ hackagePage "Preferred versions"
          [ h2 << "Preferred versions"
          , case renders of
                [] -> paragraph << "There are no global preferred versions."
                _  -> unordList $ flip map renders $ \(pkgname, pref) ->
                    [ packageNameLink pkgname
                    ,  unordList [varList "Preferred ranges" (rendRanges pref),
                                  varList "Deprecated versions" (map display $ rendVersions pref),
                                  toHtml ["Calculated range: ", rendSumRange pref]]
                    ]
          , paragraph <<
              [ anchor ! [href "/packages/preferred-versions"] << "preferred-versions"
              , toHtml " is the text file served with every index tarball that contains this information."
              ]
          ]
      where varList summ [] = toHtml $ summ ++ ": none"
            varList summ xs = toHtml $ summ ++ ": " ++ intercalate ", " xs

    packagePrefAbout :: Maybe Resource -> PackageName -> [Html]
    packagePrefAbout maybeEdit pkgname =
      [ paragraph <<
          [ anchor ! [href $ preferredUri versions ""] << "Preferred and deprecated versions"
          , toHtml " can be used to influence Cabal's decisions about which versions of "
          , packageNameLink pkgname
          , toHtml " to install. If a range of versions is preferred, it means that the installer won't install a non-preferred package version unless it is explicitly specified or if it's the only choice the installer has. Deprecating a version adds a range which excludes just that version. All of this information is collected in the "
          , anchor ! [href "/packages/preferred-versions"] << "preferred-versions"
          , toHtml " file that's included in the index tarball."
          , flip (maybe noHtml) maybeEdit $ \prefEdit -> thespan ! [thestyle "color: gray"] <<
              [ toHtml " [maintainers: "
              , anchor ! [href $ renderResource prefEdit [display pkgname]] << "edit"
              , toHtml "]" ]
          ]
      , paragraph <<
          [ toHtml "If all the available versions of a package are non-preferred or deprecated, cabal-install will treat this the same as if none of them are. This feature doesn't affect whether or not to install a package, only for selecting versions after a given package has decided to be installed. "
          , anchor ! [href $ deprecatedPackageUri versions "" pkgname] << "Entire-package deprecation"
          , toHtml " is also available, but it's separate from preferred versions."
          ]
      ]

    servePackagePreferred :: Resource -> DynamicPath -> ServerPartE Response
    servePackagePreferred prefEdit dpath = do
      pkgname <- packageInPath dpath
      pkgs    <- lookupPackageName pkgname
      pref    <- doPreferredRender pkgname
      let dtitle = display pkgname ++ ": preferred and deprecated versions"
      prefInfo <- queryGetPreferredInfo pkgname
      return $ toResponse $ Resource.XHtml $ hackagePage dtitle --needs core, preferredVersions, pkgname
        [ h2 << dtitle
        , concatHtml $ packagePrefAbout (Just prefEdit) pkgname
        , h4 << "Stored information"
        , case rendRanges pref of
              [] -> paragraph << [display pkgname ++ " has no preferred version ranges."]
              prefs -> paragraph << ["Preferred versions for " ++ display pkgname ++ ":"]
                           +++ unordList prefs
        , case rendVersions pref of
              [] -> paragraph << ["It has no deprecated versions."]
              deprs -> paragraph <<
                  [ "Explicitly deprecated versions for " ++ display pkgname ++ " include: "
                  , intercalate ", " (map display deprs)]
        , toHtml "The version range given to this package, therefore, is " +++ strong (toHtml $ rendSumRange pref)
        , h4 << "Versions affected"
        , paragraph << "Green versions are normal versions. Yellow are those out of any preferred version ranges. Red are deprecated."
        , paragraph ! [theclass "versions"] << snd (Pages.renderVersion
                              (PackageIdentifier pkgname nullVersion)
                              (classifyVersions prefInfo $ map packageVersion pkgs) Nothing)
        ]

    servePutPreferred :: DynamicPath -> ServerPartE Response
    servePutPreferred dpath = do
      pkgname <- packageInPath dpath
      putPreferred pkgname
      return $ toResponse $ Resource.XHtml $ hackagePage "Updated preferred versions"
        [ h2 << "Updated the preferred versions"
        , paragraph <<
            [ toHtml "Updated the "
            , anchor ! [href $ preferredPackageUri versionsResource "" pkgname] << "preferred versions"
            , toHtml " for "
            , packageNameLink pkgname
            , toHtml "."]
        ]

    servePutDeprecated :: DynamicPath -> ServerPartE Response
    servePutDeprecated dpath = do
      pkgname <- packageInPath dpath
      wasDepr <- putDeprecated pkgname
      let dtitle = if wasDepr then "Package deprecated" else "Package undeprecated"
      return $ toResponse $ Resource.XHtml $ hackagePage dtitle
         [ h2 << dtitle
         , paragraph <<
            [ toHtml "Updated the "
            , anchor ! [href $ deprecatedPackageUri versionsResource "" pkgname] << "deprecated status"
            , toHtml " for "
            , packageNameLink pkgname
            , toHtml "."]
         ]

    -- deprecated: checkbox, by: text field, space-separated list of packagenames
    serveDeprecateForm :: DynamicPath -> ServerPartE Response
    serveDeprecateForm dpath = do
      pkgname <- packageInPath dpath
      mpkg <- doDeprecatedRender pkgname
      let (isDepr, mfield) = case mpkg of
              Just pkgs -> (True, unwords $ map display pkgs)
              Nothing -> (False, "")
      return $ toResponse $ Resource.XHtml $ hackagePage "Deprecate package"
          [paragraph << [toHtml "Configure deprecation for ", packageNameLink pkgname],
           form . ulist ! [theclass "box", XHtml.method "post", action $ deprecatedPackageUri versionsResource "" pkgname] <<
            [ hidden "_method" "PUT"
            , li . toHtml $ makeCheckbox isDepr "deprecated" "on" "Deprecate package"
            , li . toHtml $ makeInput [thetype "text", value mfield] "by" "Superseded by: " ++ [br, toHtml "(Optional; space-separated list of package names)"]
            , paragraph << input ! [thetype "submit", value "Set status"]
            ]]

    -- preferred: text box (one version range per line). deprecated: list of text boxes with same name
    servePreferForm :: DynamicPath -> ServerPartE Response
    servePreferForm dpath = do
      pkgname <- packageInPath dpath
      pkgs    <- lookupPackageName pkgname
      pref    <- doPreferredRender pkgname
      let allVersions = map packageVersion pkgs
          rangesList  = rendRanges pref
          deprVersions = rendVersions pref
      return $ toResponse $ Resource.XHtml $ hackagePage "Adjust preferred versions"
          [concatHtml $ packagePrefAbout Nothing pkgname,
           form ! [theclass "box", XHtml.method "post", action $ preferredPackageUri versionsResource "" pkgname] <<
            [ hidden "_method" "PUT"
            , paragraph << "Preferred version ranges."
            , paragraph << textarea ! [name "preferred", rows $ show (4::Int), cols $ show (80::Int)] << unlines rangesList
            , paragraph << "Deprecated versions."
            , toHtml $ intersperse (toHtml " ") $ map (\v -> toHtml $ makeCheckbox (v `elem` deprVersions) "deprecated" (display v) (display v)) allVersions
            , paragraph << input ! [thetype "submit", value "Set status"]
            ]]

{-------------------------------------------------------------------------------
  Downloads
-------------------------------------------------------------------------------}

data HtmlDownloads = HtmlDownloads {
    htmlDownloadsResources :: [Resource]
  }

mkHtmlDownloads :: HtmlUtilities -> DownloadFeature -> HtmlDownloads
mkHtmlDownloads HtmlUtilities{..} DownloadFeature{..} = HtmlDownloads{..}
  where
    downs = downloadResource

    -- downloads
    htmlDownloadsResources = [
        (extendResource $ topDownloads downs) {
            resourceGet = [("html", serveDownloadTop)]
          }
      ]

    serveDownloadTop :: DynamicPath -> ServerPartE Response
    serveDownloadTop _ = do
        pkgList <- sortedPackages `liftM` recentPackageDownloads
        return $ toResponse $ Resource.XHtml $ hackagePage "Total downloads"
          [ h2 << "Downloaded packages"
          , thediv << table << downTableRows pkgList
          ]
      where
        downTableRows pkgList =
            [ tr << [ th << "Package name", th << "Downloads" ] ] ++
            [ tr ! [theclass (if odd n then "odd" else "even")] <<
                [ td << packageNameLink pkgname
                , td << [ toHtml $ show count ] ]
            | ((pkgname, count), n) <- zip pkgList [(1::Int)..] ]

    sortedPackages :: RecentDownloads -> [(PackageName, Int)]
    sortedPackages = sortBy (flip compare `on` snd) . cmToList

{-------------------------------------------------------------------------------
  Tags
-------------------------------------------------------------------------------}

data HtmlTags = HtmlTags {
    htmlTagsResources :: [Resource]
  , tagEdit :: Resource
  }

mkHtmlTags :: HtmlUtilities
           -> CoreFeature
           -> UploadFeature
           -> UserFeature
           -> ListFeature
           -> TagsFeature
           -> Templates
           -> HtmlTags
mkHtmlTags HtmlUtilities{..}
           CoreFeature{ coreResource = CoreResource{
                          packageInPath
                        , guardValidPackageName
                        }
                      }
           UploadFeature{ maintainersGroup, trusteesGroup }
           UserFeature{ guardAuthorised', guardAuthorised_ }
           ListFeature{makeItemList}
           TagsFeature{..}
           templates
           = HtmlTags{..}
  where
    tags = tagsResource

    tagEdit           = (resourceAt "/package/:package/tags/edit") {
                            resourceGet = [("html", serveTagsForm)]
                          }

    htmlTagsResources = [
        (extendResource $ tagsListing tags) {
            resourceGet = [("html", serveTagsListing)]
          }
      , (extendResource $ tagListing tags) {
            resourceGet = [("html", serveTagListing)]
          }
      , (extendResource $ packageTagsListing tags) {
            resourcePut = [("html", putPackageTags)], resourceGet = [("html", showPackageTags)]
          }
      , (extendResource $ tagAliasEdit tags) {
            resourcePut = [("html", putAliasEdit)]
          }
      , (extendResource $ tagAliasEditForm tags) {
            resourceGet = [("html", serveAliasForm)]
          }
      , tagEdit -- (extendResource $ packageTagsEdit tags) { resourceGet = [("html", serveTagsForm)] }
      ]

    serveTagsListing :: DynamicPath -> ServerPartE Response
    serveTagsListing _ = do
        tagList <- queryGetTagList
        let withCounts = filter ((>0) . snd) . map (\(tg, pkgs) -> (tg, Set.size pkgs)) $ tagList
            countSort = sortBy (flip compare `on` snd) withCounts
        return $ toResponse $ Resource.XHtml $ hackagePage "Hackage tags"
          [ h2 << "Hackage tags"
          , h4 << "By name"
          , paragraph ! [theclass "toc"] << (intersperse (toHtml ", ") $ map (tagItem . fst) withCounts)
          , h4 << "By frequency"
          , paragraph ! [theclass "toc"] << (intersperse (toHtml ", ") $ map (toHtml . tagCountItem) countSort)
          ]
      where tagCountItem (tg, count) =
              [ tagItem tg
              , toHtml $ " (" ++ show count ++ ")"
              ]
            tagItem tg = anchor ! [href $ tagUri tags "" tg] << display tg

    putAliasEdit :: DynamicPath -> ServerPartE Response
    putAliasEdit dpath = do
        tagname <- tagInPath dpath
        targetTag <- optional $ look "tags"
        mergeTags targetTag (Tag tagname)
        return $ toResponse $ Resource.XHtml $ hackagePage "Merged Tag"
            [ h2 << "Merged tag"
            , toHtml "Return to "
            , anchor ! [href "/packages/tags"] << "tag listings"
            ]

    serveAliasForm :: DynamicPath -> ServerPartE Response
    serveAliasForm dpath = do
        tagname <- tagInPath dpath
        guardAuthorised_ [InGroup trusteesGroup]

        let aliasForm = [ thediv ! [theclass "box"] <<
                            [h2 << ("Merge Tag " ++ tagname)
                            , form ! [XHtml.method "post", action ("/packages/tag/" ++ tagname ++ "/alias")] <<
                                [ hidden "_method" "PUT"
                                , input ! [value "", name "tags", identifier "tags"]
                                , toHtml " (Tag to merge with) ", br
                                , input ! [thetype "submit", value "Merge"]
                                ]
                            ]
                        ]
        return $ toResponse $ Resource.XHtml $ hackagePage ("Merge Tag " ++ tagname) aliasForm

    serveTagListing :: DynamicPath -> ServerPartE Response
    serveTagListing dpath = do
      tagname <- tagInPath dpath
      withTagPath dpath $ \tg pkgnames -> do
        let tagd = "Packages tagged " ++ display tg
            pkgs = Set.toList pkgnames
        items <- liftIO $ makeItemList pkgs
        let (mtag, histogram) = Map.updateLookupWithKey (\_ _ -> Nothing) tg $ tagHistogram items
            rowList = map makeRow items
            -- make a 'related tags' section, so exclude this tag from the histogram
            count = fromMaybe 0 mtag
        template <- getTemplate templates "table-interface.html"
        return $ toResponse $ template
          [ "heading"   $= tagd
          , "content"   $=  case items of
                [] -> toHtml "No packages have this tag."
                _  -> toHtml
                  [ paragraph << [if count==1 then "1 package has" else show count ++ " packages have", " this tag."]
                  , anchor ! [href $  tagname ++ "/alias/edit"] << "[Merge tag]"
                  , toHtml " (trustees only)"
                  , paragraph ! [theclass "toc"] << [toHtml "Related tags: ", toHtml $ showHistogram histogram]
                  ]
          , "tabledata" $= rowList
          ]

     where
      showHistogram hist = (++takeHtml) . intersperse (toHtml ", ") $
            map histogramEntry $ take takeAmount sortHist
        where hsize = Map.size hist
              takeAmount = max (div (hsize*2) 3) 12
              takeHtml = if takeAmount >= hsize then [] else [toHtml ", ..."]
              sortHist = sortBy (flip compare `on` snd) $ Map.toList hist
      histogramEntry (tg', count) = anchor ! [href $ tagUri tags "" tg'] << display tg' +++ (" (" ++ show count ++ ")")

    putPackageTags :: DynamicPath -> ServerPartE Response
    putPackageTags dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      addns <- optional $ look "addns"
      delns <- optional $ look "delns"
      raddns <- optional $ look "raddns"
      rdelns <- optional $ look "rdelns"

      putTags addns delns raddns rdelns pkgname
      currTags <- queryTagsForPackage pkgname
      revTags <- queryReviewTagsForPackage pkgname
      let disp = renderReviewTags currTags revTags pkgname
      return $ toResponse $ Resource.XHtml $ hackagePage "Package Tags" disp

    showPackageTags :: DynamicPath -> ServerPartE Response
    showPackageTags dpath = do
      pkgname <- packageInPath dpath
      currTags <- queryTagsForPackage pkgname
      revTags <- queryReviewTagsForPackage pkgname
      let disp = renderReviewTags currTags revTags pkgname
      return $ toResponse $ Resource.XHtml $ hackagePage "Package Tags" disp

    -- serve form for editing, to be received by putTags
    serveTagsForm :: DynamicPath -> ServerPartE Response
    serveTagsForm dpath = do
      pkgname <- packageInPath dpath
      currTags <- queryTagsForPackage pkgname
      revTags <- queryReviewTagsForPackage pkgname
      template <- getTemplate templates "tag-edit.html"
      let toStr = intercalate ", " . map display . Set.toList
          tagsStr = toStr currTags
          addns = toStr $ fst  revTags
          delns = toStr $ snd  revTags
      trustainer <- guardAuthorised' [InGroup (maintainersGroup pkgname), InGroup trusteesGroup]
      user <- guardAuthorised' [AnyKnownUser]
      if trustainer || user
        then return $ toResponse . template $
          [ "pkgname"           $= display pkgname
          , "addns"             $= addns
          , "tags"              $= tagsStr
          , "delns"             $= delns
          , "istrustee"         $= trustainer
          , "isuser"            $= not trustainer
          ]
        else return $ toResponse $ Resource.XHtml $ hackagePage "Error" [h2 << "Authorization Error"
                                                                            , paragraph << "You need to be logged in to propose tags"]

-- | Find a TagName inside a path.
tagInPath :: forall m a. (MonadPlus m, FromReqURI a) => DynamicPath -> m a
tagInPath dpath = maybe mzero return (lookup "tag" dpath >>= fromReqURI)

{-------------------------------------------------------------------------------
  Tracking pixels
-------------------------------------------------------------------------------}

newtype HtmlAnalyticsPixels = HtmlAnalyticsPixels {
    htmlAnalyticsPixelsResources :: [Resource]
  }

mkHtmlAnalyticsPixels :: HtmlUtilities -> CoreFeature -> UserFeature -> UploadFeature -> AnalyticsPixelsFeature -> Templates -> HtmlAnalyticsPixels
mkHtmlAnalyticsPixels HtmlUtilities{..} CoreFeature{..} UserFeature{..} UploadFeature{..} AnalyticsPixelsFeature{..} templates = HtmlAnalyticsPixels{..}
  where
    CoreResource{..} = coreResource

    htmlAnalyticsPixelsResources = [
        (extendResource analyticsPixelsResource) {
            resourceGet = [("html", servePackageAnalyticsPixels)]
          , resourcePost = [("html", serveAddPackageAnalyticsPixel)]
          , resourceDelete = [("html", serveRemovePackageAnalyticsPixel)]
        },
        (extendResource userAnalyticsPixelsResource) {
            resourceGet = [("html", serveUserPackageAnalyticsPixels)]
          , resourcePost = [("html", serveAddUserPackageAnalyticsPixel)]
          , resourceDelete = [("html", serveRemoveUserPackageAnalyticsPixel)]
        }
      ]

    serveUserPackageAnalyticsPixels :: DynamicPath -> ServerPartE Response
    serveUserPackageAnalyticsPixels dpath = do
        uname <- userNameInPath dpath
        userPackagesAnalyticsPixelsHtml uname

    serveAddUserPackageAnalyticsPixel :: DynamicPath -> ServerPartE Response
    serveAddUserPackageAnalyticsPixel =
      serveModifyUserPackageAnalyticsPixel $ \pkgname pixel -> do
        _ <- addPackageAnalyticsPixel pkgname pixel
        pure ()

    serveRemoveUserPackageAnalyticsPixel :: DynamicPath -> ServerPartE Response
    serveRemoveUserPackageAnalyticsPixel =
      serveModifyUserPackageAnalyticsPixel removePackageAnalyticsPixel

    serveModifyUserPackageAnalyticsPixel
      :: (PackageName -> AnalyticsPixel -> ServerPartE ())
      -> DynamicPath
      -> ServerPartE Response
    serveModifyUserPackageAnalyticsPixel modifyPixel dpath = do
        uname   <- userNameInPath dpath
        request <-
            getDataFn $ (,)
                <$> look "package"
                <*> look "analytics-pixel"
        case request of
            Left errs ->
                errBadRequest "Error adding new tracking pixel"
                    ((MText "Tracking pixel url missing.") : map MText errs)
            Right (pkgnameStr, analyticsPixel) -> do
                let pkgname = mkPackageName pkgnameStr
                    pixel   = AnalyticsPixel (T.pack analyticsPixel)
                guardAuthorisedAsMaintainerOrTrustee pkgname
                modifyPixel pkgname pixel
                userPackagesAnalyticsPixelsHtml uname

    servePackageAnalyticsPixels :: DynamicPath -> ServerPartE Response
    servePackageAnalyticsPixels dpath = do
        pkgname <- packageInPath dpath
        packageAnalyticsPixelsHtml pkgname

    serveAddPackageAnalyticsPixel :: DynamicPath -> ServerPartE Response
    serveAddPackageAnalyticsPixel = do
        serveModifyPackageAnalyticsPixel $ \pkgname pixel -> do
            _ <- addPackageAnalyticsPixel pkgname pixel
            pure ()

    serveRemovePackageAnalyticsPixel :: DynamicPath -> ServerPartE Response
    serveRemovePackageAnalyticsPixel =
      serveModifyPackageAnalyticsPixel removePackageAnalyticsPixel

    serveModifyPackageAnalyticsPixel
      :: (PackageName -> AnalyticsPixel -> ServerPartE ())
      -> DynamicPath
      -> ServerPartE Response
    serveModifyPackageAnalyticsPixel modifyPixel dpath = do
        pkgname <- packageInPath dpath
        guardValidPackageName pkgname
        guardAuthorisedAsMaintainerOrTrustee pkgname
        request <- getDataFn (look "analytics-pixel")
        case request of
            Left errs ->
                errBadRequest "Error adding new tracking pixel"
                    ((MText "Tracking pixel url missing.") : map MText errs)
            Right analyticsPixel -> do
              let pixel = AnalyticsPixel (T.pack analyticsPixel)
              modifyPixel pkgname pixel
              packageAnalyticsPixelsHtml pkgname

    packageAnalyticsPixelsHtml :: PackageName -> ServerPartE Response
    packageAnalyticsPixelsHtml pkgname = do
        analyticsPixels <- getPackageAnalyticsPixels pkgname
        template <- getTemplate templates "analytics-pixels-page.html"
        return $ toResponse $ template
            [ "pkgname" $= pkgname,
              "AnalyticsPixels" $= map analyticsPixelUrl (Set.toList analyticsPixels)
            ]

    userPackagesAnalyticsPixelsHtml :: UserName -> ServerPartE Response
    userPackagesAnalyticsPixelsHtml uname = do
        uid  <- lookupUserName uname
        -- Get all the packages the user has access to
        uris <- getGroupIndex uid
        pkgs <- foldMap (\uri -> do
            groupDesc <- getIndexDesc uri
            let mpackageName = fmap fst (Group.groupEntity groupDesc)
            pure $ maybeToList (fmap mkPackageName mpackageName)
          )
          uris
        pkgpixels <- forM pkgs $ \pkgname -> do
            pixels <- getPackageAnalyticsPixels pkgname
            pure (pkgname, map analyticsPixelUrl (Set.toList pixels))
        template <- getTemplate templates "user-analytics-pixels-page.html"
        return $ toResponse $ template
            [   "username"  $= uname,
                "pkgs"      $= pkgs,
                "pkgpixels" $= filter (not . null . snd) pkgpixels
            ]

    guardAuthorisedAsMaintainerOrTrustee pkgname =
      guardAuthorised_ [InGroup (maintainersGroup pkgname), InGroup trusteesGroup]

{-------------------------------------------------------------------------------
  Groups
-------------------------------------------------------------------------------}

htmlGroupResource :: UserFeature -> GroupResource -> [Resource]
htmlGroupResource UserFeature{..} r@(GroupResource groupR userR getGroup) =
  [ (extendResource groupR) {
        resourceDesc = [ (GET, "Show list of users")
                       , (POST, "Add a user to the group")
                       ]
      , resourceGet  = [ ("html", getList) ]
      , resourcePost = [ ("html", postUser) ]
      }
  , (extendResource userR) {
        resourceDesc   = [ (DELETE, "Delete a user from the group") ]
      , resourceDelete = [ ("html", deleteFromGroup) ]
      }
  , (extendResourcePath "/edit" groupR) {
        resourceDesc = [ (GET, "Show edit form for the group") ]
      , resourceGet  = [ ("html", getEditList) ]
      }
  ]
  where
    getList dpath = do
        group    <- getGroup dpath
        userDb   <- queryGetUserDb
        usergroup <- liftIO . queryUserGroup $ group
        let unames = [ Users.userIdToName userDb uid
                     | uid   <- Group.toList usergroup ]
        let baseUri = renderResource' groupR dpath
        cacheControl [NoCache] (etagFromHash unames)
        return . toResponse . Resource.XHtml $ Pages.groupPage
            unames baseUri (False, False) (groupDesc group)
    getEditList dpath = do
        group    <- getGroup dpath
        (canAdd, canDelete) <- lookupGroupEditAuth group
        userDb   <- queryGetUserDb
        usergroup <- liftIO . queryUserGroup $ group
        let unames = [ Users.userIdToName userDb uid
                     | uid   <- Group.toList usergroup ]
        let baseUri = renderResource' groupR dpath
        cacheControl [NoCache] (etagFromHash unames)
        return . toResponse . Resource.XHtml $ Pages.groupPage
            unames baseUri (canAdd, canDelete) (groupDesc group)
    postUser dpath = do
        group <- getGroup dpath
        groupAddUser group dpath
        goToList dpath
    deleteFromGroup dpath = do
        group <- getGroup dpath
        groupDeleteUser group dpath
        goToList dpath
    goToList dpath = seeOther (renderResource' (groupResource r) dpath) (toResponse ())

{-------------------------------------------------------------------------------
  Reverse
-------------------------------------------------------------------------------}
data HtmlReverse = HtmlReverse {
    htmlReverseResource :: [Resource]
  }

mkHtmlReverse :: HtmlUtilities
              -> CoreFeature
              -> VersionsFeature
              -> ListFeature
              -> ReverseFeature
              -> ReverseHtmlUtil
              -> HtmlReverse
mkHtmlReverse HtmlUtilities{..}
           CoreFeature{ coreResource = CoreResource{
                          packageInPath
                        , lookupPackageName
                        , corePackageIdUri
                        , corePackageNameUri
                        },
                        queryGetPackageIndex
                      }
           VersionsFeature{withPackageVersion}
           ListFeature{}
           ReverseFeature{..}
           ReverseHtmlUtil{..}
           = HtmlReverse{..}
  where
    htmlReverseResource = [
        (extendResource $ reversePackage reverseResource) {
            resourceGet = [("html", serveReverse OnlyLatest)]
          }
      , (extendResource $ reversePackageOld reverseResource) {
            resourceGet = [("html", serveReverse OnlyOlder)]
          }
      ,(extendResource $ reversePackageFlat reverseResource) {
            resourceGet = [("html", serveReverseFlat)]
          }
      , (extendResource $ reversePackageVerbose reverseResource) {
            resourceGet = [("html", serveReverseVerbose)]
          }
      , (extendResource $ reversePackages reverseResource) {
            resourceGet = [("html", serveReverseList)]
          }
      ]


    serveReverse :: LatestOrOld -> DynamicPath -> ServerPartE Response
    serveReverse isRecent dpath = do
        pkgid <- packageInPath dpath
        let pkgname = pkgName pkgid
        rdisp <- if nullVersion == packageVersion pkgid
                   then lookupPackageName pkgname *> revPackageName pkgname
                   else withPackageVersion pkgid $ \_ -> revPackageId pkgid
        render <- (if isRecent == OnlyLatest then renderReverseRecent else renderReverseOld) pkgname rdisp
        return $ toResponse $ Resource.XHtml $ hackagePage (display pkgname ++ " - Reverse dependencies ") $
            reversePackageRender pkgid (corePackageIdUri "") isRecent render

    redirectIfVersion uriGen pkgid =
      if packageVersion pkgid /= nullVersion
         then do
           let newUri = uriGen reverseResource "" (packageName pkgid)
           seeOther newUri ()
         else pure ()

    serveReverseFlat :: DynamicPath -> ServerPartE Response
    serveReverseFlat dpath = do
        pkg <- packageInPath dpath
        redirectIfVersion reverseFlatUri pkg
        let pkgname = pkgName pkg
        revCount <- revPackageStats pkgname
        pairs <- revPackageFlat pkgname
        return $ toResponse $ Resource.XHtml $ hackagePage (display pkgname ++ " - Flattened reverse dependencies") $
            reverseFlatRender pkgname (corePackageNameUri "") revCount pairs

    serveReverseVerbose :: DynamicPath -> ServerPartE Response
    serveReverseVerbose dpath = do
        pkg <- packageInPath dpath
        redirectIfVersion reverseVerboseUri pkg
        let pkgname = pkgName pkg
        pkgids <- lookupPackageName pkgname
        revCount <- revPackageStats pkgname
        versions <- liftIO $ revForEachVersion pkgname
        return $ toResponse $ Resource.XHtml $ hackagePage (display pkgname ++ " - Reverse dependency statistics") $
            reverseVerboseRender pkgname (map packageVersion pkgids) (corePackageIdUri "") revCount versions

    serveReverseList :: DynamicPath -> ServerPartE Response
    serveReverseList _ = do
        namesWithCounts <- revCountForAllPackages
        hackCount <- PackageIndex.indexSize <$> queryGetPackageIndex
        return $ toResponse $ Resource.XHtml $ hackagePage "Reverse dependencies" $
            reversePackagesRender (corePackageNameUri "") hackCount namesWithCounts
