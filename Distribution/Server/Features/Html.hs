module Distribution.Server.Features.Html (
    HtmlFeature,
    htmlResources,
    initHtmlFeature
  ) where


import Distribution.Server.Acid (query)
import Distribution.Server.Framework
import Distribution.Server.Features.Core
import Distribution.Server.Features.Packages
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Check
import Distribution.Server.Features.Users
import Distribution.Server.Features.DownloadCount
import Distribution.Server.Features.NameSearch
import Distribution.Server.Features.PreferredVersions
-- [reverse index disabled] import Distribution.Server.Features.ReverseDependencies
import Distribution.Server.Features.PackageList
import Distribution.Server.Features.Tags
import Distribution.Server.Features.Mirror

import qualified Distribution.Server.Framework.ResourceTypes as Resource
import qualified Distribution.Server.Pages.Package as Pages
import qualified Distribution.Server.Packages.State as State
import qualified Distribution.Server.Users.State as State
import qualified Distribution.Server.Features.Distro.State as State
import qualified Distribution.Server.Framework.Cache as Cache

import Distribution.Server.Users.Types
import Distribution.Server.Packages.Types
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Users.Group (UserGroup(..))
import Distribution.Server.Features.Distro.Distributions (DistroPackageInfo(..))
import Distribution.Server.Packages.Preferred
-- [reverse index disabled] import Distribution.Server.Packages.Reverse
import Distribution.Server.Packages.Tag

import Distribution.Server.Pages.Template (hackagePage, hackagePageWith, haddockPage)
import Distribution.Server.Pages.Util
import qualified Distribution.Server.Pages.Group as Pages
-- [reverse index disabled] import qualified Distribution.Server.Pages.Reverse as Pages
import qualified Distribution.Server.Pages.Index as Pages
import Text.XHtml.Strict
import qualified Text.XHtml.Strict as XHtml
import Text.XHtml.Table (simpleTable)

import Distribution.Package
import Distribution.Version
import Distribution.Text (display)
import Distribution.PackageDescription
import Data.List (intercalate, intersperse, insert, sortBy)
import Data.Function (on)
import Control.Monad
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath.Posix ((</>))
import Data.Maybe (fromMaybe)

-- TODO: move more of the below to Distribution.Server.Pages.*, it's getting
-- close to 1K lines, way too much... it's okay to keep data-querying in here,
-- but pure HTML generation mostly needlessly clutters up the module.
-- Try to make it so no HTML combinators need to be imported.
--
-- See the TODO file for more ways to improve the HTML.
data HtmlFeature = HtmlFeature {
    htmlResources :: [Resource],
    cachePackagesPage :: Cache.CacheableAction Response,
    cacheNamesPage :: Cache.Cache Response,
    generateCaches :: IO ()
}

instance IsHackageFeature HtmlFeature where
    getFeatureInterface html = (emptyHackageFeature "html") {
        featureResources = htmlResources html
      , featurePostInit  = generateCaches html
      }

-- This feature provides the HTML view to the models of other features
-- currently it uses the xhtml package to render HTML (Text.XHtml.Strict)
--
-- This means of generating HTML is somewhat temporary, in that a more advanced
-- (and better-looking) HTML ajaxy scheme should come about later on.
initHtmlFeature :: Bool
                -> ServerEnv -> CoreFeature -> PackagesFeature -> UploadFeature
                -> CheckFeature -> UserFeature -> VersionsFeature
                -- [reverse index disabled] -> ReverseFeature
                -> TagsFeature -> DownloadFeature
                -> ListFeature -> NamesFeature -> MirrorFeature
                -> IO HtmlFeature
initHtmlFeature enableCaches env
                core pkg upload check user version
                -- [reverse index disabled] reversef
                tagf
                down list namef mirror = do
    -- resources to extend
    let cores = coreResource core
        users = userResource user
        uploads = uploadResource upload
        checks  = checkResource check
        versions = versionsResource version
        -- [reverse index disabled] reverses = reverseResource reversef
        tags = tagsResource tagf
        downs = downloadResource down
        names = namesResource namef
    let store = serverBlobStore env
    -- pages defined for the HTML feature in particular
    let editDeprecated  = (resourceAt "/package/:package/deprecated/edit") { resourceGet = [("html", serveDeprecateForm cores versions)] }
        editPreferred   = (resourceAt "/package/:package/preferred/edit") { resourceGet = [("html", servePreferForm cores versions)] }
        maintainPackage = (resourceAt "/package/:package/maintain") { resourceGet = [("html", serveMaintainLinks editDeprecated editPreferred $
 packageGroupResource uploads)] }
        pkgCandUploadForm = (resourceAt "/package/:package/candidate/upload") { resourceGet = [("html", servePackageCandidateUpload cores checks)] }
        candMaintainForm = (resourceAt "/package/:package/candidate/maintain") { resourceGet = [("html", serveCandidateMaintain cores checks)] }
        tagEdit = (resourceAt "/package/:package/tags/edit") { resourceGet = [("html", serveTagsForm cores tags)] }
    -- Index page caches
    namesCache <- Cache.newCacheable $ toResponse ()
    mainCache <- Cache.newCacheableAction enableCaches $
        do index <- fmap State.packageList $ query State.GetPackagesState
           return (toResponse $ Resource.XHtml $ Pages.packageIndex index)
    let computeNames = Cache.putCache namesCache =<< packagesPage cores list tags
    registerHook (itemUpdate list) $ \_ -> computeNames
    registerHook (packageIndexChange core) $ Cache.refreshCacheableAction mainCache

    return HtmlFeature
      { cachePackagesPage = mainCache
      , cacheNamesPage = namesCache
      , generateCaches = do Cache.refreshCacheableAction mainCache
                            computeNames
      , htmlResources =
        -- core
         [ (extendResource $ corePackagePage cores) { resourceGet = [("html", servePackagePage cores pkg
                                                                                               -- [reverse index disabled] reverses
                                                                                               versions tags maintainPackage tagEdit)] }
         --, (extendResource $ coreIndexPage cores) { resourceGet = [("html", serveIndexPage)] }, currently in 'core' feature
         , (resourceAt "/packages/names" ) { resourceGet = [("html", Cache.respondCache namesCache id)] }
         , (extendResource $ corePackagesPage cores) { resourceGet = [("html", const $ Cache.getCacheableAction mainCache)] }
         , maintainPackage
        -- users
         , (extendResource $ userList users) { resourceGet = [("html", serveUserList users)], resourcePost = [("html", \_ -> htmlResponse $ adminAddUser)] }
            -- list of users with user links; if admin, a link to add user page
         , (resourceAt "/users/register") { resourceGet = [("html", addUserForm users)] }
            -- form to post to /users/
         , (extendResource $ userPage users) { resourceGet = [("html", serveUserPage user)], resourceDelete = [("html", serveDeleteUser)] }
            -- user page with link to password form and list of groups (how to do this?)
         , (extendResource $ passwordResource users) { resourceGet = [("html", servePasswordForm users)], resourcePut = [("html", servePutPassword users)] }
            -- form to PUT password
         , (extendResource $ enabledResource users) { resourceGet = [("html", serveEnabledForm users)], resourcePut = [("html", servePutEnabled users)] }
            -- form to enable or disable users (admin only)

        -- uploads
         , (extendResource $ uploadIndexPage uploads) { resourcePost = [("html", serveUploadResult upload cores)] }
            -- serve upload result as HTML
         , (resourceAt "/packages/upload") { resourceGet = [("html", serveUploadForm)] }
            -- form for uploading

        -- checks
         , (extendResource $ candidatesPage checks) { resourceGet = [("html", serveCandidatesPage cores checks)], resourcePost = [("html", \_ -> htmlResponse $ postCandidate checks user upload store)] }
            -- list of all packages which have candidates
         , (extendResource $ packageCandidatesPage checks) { resourceGet = [("html", servePackageCandidates cores checks pkgCandUploadForm)], resourcePost = [("", htmlResponse . postPackageCandidate checks user upload store)] }
            -- TODO: use custom functions, not htmlResponse
         , (extendResource $ candidatePage checks) { resourceGet = [("html", serveCandidatePage check candMaintainForm)], resourcePut = [("html", htmlResponse . putPackageCandidate checks user upload store)], resourceDelete = [("html", htmlResponse . doDeleteCandidate checks)] }
            -- package page for a candidate
         , (resourceAt "/packages/candidates/upload") { resourceGet = [("html", serveCandidateUploadForm)] }
            -- form for uploading candidate
         , pkgCandUploadForm
            -- form for uploading candidate for a specific package version
         , candMaintainForm
            -- maintenance for candidate packages
         , (extendResource $ publishPage checks) { resourceGet = [("html", servePublishForm checks)], resourcePost = [("html", servePostPublish core user upload)] }
            -- form for publishing package
        -- TODO: write HTML for reports and distros to display the information effectively
        -- reports
         --, (extendResource $ reportsList reports) { resourceGet = [("html", serveReportsList)] }
         --, (extendResource $ reportsPage reports) { resourceGet = [("html", serveReportsPage)] }

        -- distros
         --, (extendResource $ distroIndexPage distros) { resourceGet = [("html", serveDistroIndex)] }
         --, (extendResource $ distroAllPage distros) { resourceGet = [("html", serveDistroPackages)] }
         --, (extendResource $ distroPackage distros) { resourceGet = [("html", serveDistroPackage)] }
        -- preferred versions
          , editDeprecated
          , editPreferred
          , (extendResource $ preferredResource versions) { resourceGet = [("html", servePreferredSummary cores)] }
          , (extendResource $ preferredPackageResource versions) { resourceGet = [("html", servePackagePreferred cores versions editPreferred)], resourcePut = [("html", servePutPreferred cores version)] }
          , (extendResource $ deprecatedResource versions) { resourceGet = [("html", serveDeprecatedSummary cores)]  }
          , (extendResource $ deprecatedPackageResource versions) { resourceGet = [("html", servePackageDeprecated cores editDeprecated)], resourcePut = [("html", servePutDeprecated cores version)] }
        {- [reverse index disabled]
        -- reverse
          , (extendResource $ reversePackage reverses) { resourceGet = [("html", serveReverse cores reverses True)] }
          , (extendResource $ reversePackageOld reverses) { resourceGet = [("html", serveReverse cores reverses False)] }
          , (extendResource $ reversePackageAll reverses) { resourceGet = [("html", serveReverseFlat cores reverses)] }
          , (extendResource $ reversePackageStats reverses) { resourceGet = [("html", serveReverseStats cores reverses)] }
          , (extendResource $ reversePackages reverses) { resourceGet = [("html", serveReverseList cores reversef)] }
          -}
        -- downloads
          , (extendResource $ topDownloads downs) { resourceGet = [("html", serveDownloadTop cores down)] }
        -- tags
          , (extendResource $ tagsListing tags) { resourceGet = [("html", serveTagsListing tags)] }
          , (extendResource $ tagListing tags) { resourceGet = [("html", serveTagListing cores list tags)] }
          , (extendResource $ packageTagsListing tags) { resourcePut = [("html", putPackageTags cores tagf)], resourceGet = [] }
          , tagEdit -- (extendResource $ packageTagsEdit tags) { resourceGet = [("html", serveTagsForm cores tagf)] }
        -- search
          , (extendResource $ findPackageResource names) { resourceGet = [("html", servePackageFind cores list namef tags)] }
         ]
        -- and user groups. package maintainers, trustees, admins
        ++ htmlGroupResource user (packageGroupResource uploads)
        ++ htmlGroupResource user (trusteeResource uploads)
        ++ htmlGroupResource user (uploaderResource uploads)
        ++ htmlGroupResource user (adminResource users)
        ++ htmlGroupResource user (mirrorGroupResource $ mirrorResource mirror)
     }

--------------------------------------------------------------------------------
---- Core
-- Currently the main package page is thrown together by querying a bunch
-- of features about their attributes for the given package. It'll need
-- reorganizing to look aesthetic, as opposed to the sleek and simple current
-- design that takes the 1990s school of web design.
servePackagePage :: CoreResource -> PackagesFeature
                 -- [reverse index disabled] -> ReverseResource
                 -> VersionsResource -> TagsResource -> Resource -> Resource
                 -> DynamicPath -> ServerPart Response
servePackagePage core pkgr
                 -- [reverse index disabled] revr
                 versions tagf maintain tagEdit dpath =
                        htmlResponse $
                        withPackageId dpath $ \pkgid  ->
                        withPackagePreferred pkgid $ \pkg pkgs -> do
    -- get the PackageRender from the PkgInfo
    render <- liftIO $ packageRender pkgr pkg
    let realpkg = rendPkgId render
        pkgname = packageName realpkg
        middleHtml = Pages.renderFields render
    -- get additional information from other features
    prefInfo <- query $ GetPreferredInfo pkgname
    let infoUrl = fmap (\_ -> preferredPackageUri versions "" pkgname) $ sumRange prefInfo
        beforeHtml = [Pages.renderVersion realpkg (classifyVersions prefInfo $ map packageVersion pkgs) infoUrl,
                      Pages.renderDependencies render]
    -- and other package indices
    distributions <- query $ State.PackageStatus pkgname
    -- [reverse index disabled] revCount <- revPackageSummary realpkg
    (totalDown, versionDown) <- perVersionDownloads pkg
    let distHtml = case distributions of
            [] -> []
            _  -> [("Distributions", concatHtml . intersperse (toHtml ", ") $ map showDist distributions)]
        afterHtml  = distHtml ++ [Pages.renderDownloads totalDown versionDown $ packageVersion realpkg
                                 -- [reverse index disabled] ,Pages.reversePackageSummary realpkg revr revCount
                                 ]
    -- bottom sections, currently only documentation
    hasDocs  <- query $ State.HasDocumentation realpkg
    let docURL | hasDocs   = Just $ "/package" </> display realpkg </> "doc"
               | otherwise = Nothing
    -- extra features like tags and downloads
    tags <- query $ TagsForPackage pkgname

    let maintainLink = anchor ! [href $ renderResource maintain [display pkgname]] << toHtml "maintain"
        tagLinks = toHtml [anchor ! [href "/packages/tags"] << "Tags", toHtml ": ",
                           toHtml (renderTags tagf tags), toHtml " | ",
                           anchor ! [href $ renderResource tagEdit [display pkgname]] << "edit"]
        backHackage = anchor ! [href $ "http://hackage.haskell.org/package/" ++ display pkgid] << "on hackage"
    deprs <- query $ GetDeprecatedFor pkgname
    let deprHtml = case deprs of
          Just fors -> paragraph ! [thestyle "color: red"] << [toHtml "Deprecated", case fors of
            [] -> noHtml
            _  -> concatHtml . (toHtml " in favor of ":) . intersperse (toHtml ", ") .
                  map (\for -> anchor ! [href $ corePackageName core "" for] << display for) $ fors]
          Nothing -> noHtml
    -- and put it all together
    return $ toResponse $ Resource.XHtml $ haddockPage (display pkgid) $
        Pages.packagePage render [tagLinks, maintainLink, backHackage] [deprHtml] (beforeHtml ++ middleHtml ++ afterHtml) [] docURL
  where
    showDist (dname, info) = toHtml (display dname ++ ":") +++
        anchor ! [href $ distroUrl info] << toHtml (display $ distroVersion info)

-- TODO: include delete link for admins
serveMaintainLinks :: Resource -> Resource -> GroupResource
                   -> DynamicPath -> ServerPart Response
serveMaintainLinks editDepr editPref mgroup dpath = htmlResponse $
                               withPackageAllPath dpath $ \pkgname _ ->
                               withPackageNameAuth pkgname $ \_ _ -> do
    let dpath' = [("package", display pkgname)]
    return $ toResponse $ Resource.XHtml $ hackagePage "Maintain package" 
      [ unordList $
          [ anchor ! [href $ renderResource' editPref dpath'] << "Edit preferred versions"
          , anchor ! [href $ renderResource' editDepr dpath'] << "Edit deprecation"
          , anchor ! [href $ renderResource' (groupResource mgroup) dpath'] << "Maintainer list"
          ]
      ]
    -- upload documentation

--------------------------------------------------------------------------------
---- Users
serveUserList :: UserResource -> DynamicPath -> ServerPart Response
serveUserList users _ = do
    userlist <- fmap (Map.keys . Users.userNameMap) $ query State.GetUserDb
    let hlist = unordList $ map (\uname -> anchor ! [href $ userPageUri users "" uname] << display uname) userlist
    ok $ toResponse $ Resource.XHtml $ hackagePage "Hackage users" [h2 << "Hackage users", hlist]

serveUserPage :: UserFeature -> DynamicPath -> ServerPart Response
serveUserPage users dpath = htmlResponse $ withUserPath dpath $ \uid info -> do
    let uname = userName info
    uris <- getGroupIndex users uid
    uriPairs <- forM uris $ \uri -> do
        desc <- getIndexDesc users uri
        return $ Pages.renderGroupName desc (Just uri)
    return $ toResponse $ Resource.XHtml $ hackagePage (display uname)
      [ h2 << display uname
    --, paragraph << [toHtml "[", anchor << [href $ userPasswordUri r "" uname] settings, toHtml "]"]
      , case uriPairs of
            [] -> noHtml
            _  -> toHtml
              [ toHtml $ display uname ++ " is part of the following groups:"
              , unordList uriPairs
              ]
      ]

addUserForm :: UserResource -> DynamicPath -> ServerPart Response
addUserForm r _ = htmlResponse $ do
    return $ toResponse $ Resource.XHtml $ hackagePage "Register account"
      [ paragraph << "Register a user account here!"
      , form ! [theclass "box", XHtml.method "POST", action $ userListUri r ""] <<
            [ simpleTable [] []
                [ makeInput [thetype "text"] "username" "User name"
                , makeInput [thetype "password"] "password" "Password"
                , makeInput [thetype "password"] "repeat-password" "Confirm password"
                ]
            , paragraph << input ! [thetype "submit", value "Create user"]
            ]
      ]

servePasswordForm :: UserResource -> DynamicPath -> ServerPart Response
servePasswordForm r dpath = htmlResponse $
                            withUserPath dpath $ \pathUid userInfo -> do
    users <- query State.GetUserDb
    (uid, _) <- guardAuthenticated hackageRealm users
    let uname = userName userInfo
    canChange <- canChangePassword uid pathUid
    case canChange of
        False -> errForbidden "Can't change password" [MText "You're neither this user nor an admin."]
        True -> return $ toResponse $ Resource.XHtml $ hackagePage "Change password"
          [ toHtml "Change your password. You'll be prompted for authentication upon submission, if you haven't logged in already."
          , form ! [theclass "box", XHtml.method "POST", action $ userPasswordUri r "" uname] <<
                [ simpleTable [] []
                    [ makeInput [thetype "password"] "password" "Password"
                    , makeInput [thetype "password"] "repeat-password" "Confirm password"
                    ]
                , hidden "_method" "PUT" --method override
                , paragraph << input ! [thetype "submit", value "Change password"]
                ]
          ]

serveEnabledForm :: UserResource -> DynamicPath -> ServerPart Response
serveEnabledForm r dpath = htmlResponse $
                           withUserPath dpath $ \_ userInfo -> do
    let uname = userName userInfo
    return $ toResponse $ Resource.XHtml $ hackagePage "Change user status"
    -- TODO: expose some of the functionality in changePassword function to determine if permissions are correct
    -- before serving this form (either admin or user)
      [ toHtml "Change the account status here."
      , form ! [theclass "box", XHtml.method "POST", action $ userEnabledUri r "" uname] <<
            [ toHtml $ makeCheckbox (isEnabled userInfo) "enabled" "on" "Enable user account"
            , hidden "_method" "PUT" --method override
            , paragraph << input ! [thetype "submit", value "Change status"]
            ]
      ]
  where isEnabled userInfo = case userStatus userInfo of
            Active Enabled _ -> True
            _ -> False

servePutEnabled :: UserResource -> DynamicPath -> ServerPart Response
servePutEnabled users dpath = htmlResponse $
                              withUserNamePath dpath $ \uname -> do
    enabledAccount uname                          
    return $ toResponse $ Resource.XHtml $ hackagePage "Account status set"
        [toHtml "Account status set for ", anchor ! [href $ userPageUri users "" uname] << display uname]

serveDeleteUser :: DynamicPath -> ServerPart Response
serveDeleteUser dpath =
  htmlResponse $
  withUserNamePath dpath $ \uname -> do
    deleteAccount uname
    let ntitle = "Deleted user"
    return $ toResponse $ Resource.XHtml $ hackagePage ntitle [toHtml ntitle]

servePutPassword :: UserResource -> DynamicPath -> ServerPart Response
servePutPassword users dpath = htmlResponse $
                               withUserNamePath dpath $ \uname -> do
    changePassword uname
    return $ toResponse $ Resource.XHtml $ hackagePage "Changed password"
        [toHtml "Changed password for ", anchor ! [href $ userPageUri users "" uname] << display uname]

htmlGroupResource :: UserFeature -> GroupResource -> [Resource]
htmlGroupResource users r@(GroupResource groupR userR groupGen) =
  [ (extendResource groupR) { resourceGet = [("html", htmlResponse . getList)], resourcePost = [("html", htmlResponse . postUser)] }
  , (extendResource userR) { resourceDelete = [("html", htmlResponse . deleteFromGroup)] }
  , (extendResourcePath "/edit" groupR) { resourceGet = [("html", htmlResponse . getEditList)] }
  ]
  where
    getList dpath = withGroup (groupGen dpath) $ \group -> do
        uidlist <- liftIO . queryUserList $ group
        unames <- query $ State.ListGroupMembers uidlist
        let baseUri = renderResource' groupR dpath
        return . toResponse . Resource.XHtml $ Pages.groupPage
            unames baseUri (False, False) (groupDesc group)
    getEditList dpath = withGroup (groupGen dpath) $ \group ->
                        withGroupEditAuth group $ \canAdd canDelete -> do
        userlist <- liftIO . queryUserList $ group
        unames <- query $ State.ListGroupMembers userlist
        let baseUri = renderResource' groupR dpath
        return . toResponse . Resource.XHtml $ Pages.groupPage
            unames baseUri (canAdd, canDelete) (groupDesc group)
    postUser dpath = withGroup (groupGen dpath) $ \group -> do
        groupAddUser users group dpath
        goToList dpath
    deleteFromGroup dpath = withGroup (groupGen dpath) $ \group -> do
        groupDeleteUser users group dpath
        goToList dpath
    withGroup group func = liftIO (groupExists group) >>= \exists -> case exists of
        False -> errNotFound "User group doesn't exist" [MText "User group doesn't exist"]
        True  -> func group
    goToList dpath = seeOther (renderResource' (groupResource r) dpath) (toResponse ())

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

--------------------------------------------------------------------------------
---- Upload
serveUploadForm :: DynamicPath -> ServerPart Response
serveUploadForm _ = htmlResponse $ do
    return $ toResponse $ Resource.XHtml $ hackagePage "Upload package"
      [ h2 << "Upload package"
      , paragraph << [toHtml "See also the ", anchor ! [href "/upload.html"] << "upload help page", toHtml "."]
      , form ! [theclass "box", XHtml.method "POST", action "/packages/", enctype "multipart/form-data"] <<
            [ input ! [thetype "file", name "package"]
            , input ! [thetype "submit", value "Upload package"]
            ]
      ]

serveUploadResult :: UploadFeature -> CoreResource -> DynamicPath -> ServerPart Response
serveUploadResult pkgf core _ = htmlResponse $ do
    res <- uploadPackage pkgf
    let warns = uploadWarnings res
        pkgid = packageId (uploadDesc res)
    return $ toResponse $ Resource.XHtml $ hackagePage "Upload successful" $
      [ paragraph << [toHtml "Successfully uploaded ", packageLink core pkgid, toHtml "!"]
      ] ++ case warns of
        [] -> []
        _  -> [paragraph << "There were some warnings:", unordList warns]

--------------------------------------------------------------------------------
---- Candidates
serveCandidateUploadForm :: DynamicPath -> ServerPart Response
serveCandidateUploadForm _ = htmlResponse $ do
    return $ toResponse $ Resource.XHtml $ hackagePage "Checking and uploading candidates"
      [ h2 << "Checking and uploading candidates"
      , paragraph << [toHtml "See also the ", anchor ! [href "/upload.html"] << "upload help page", toHtml "."]
      , form ! [theclass "box", XHtml.method "POST", action "/packages/candidates/", enctype "multipart/form-data"] <<
            [ input ! [thetype "file", name "package"]
            , input ! [thetype "submit", value "Upload candidate"]
            ]
      ]

servePackageCandidateUpload :: CoreResource -> CheckResource -> DynamicPath -> ServerPart Response
servePackageCandidateUpload _ _ _ = htmlResponse $ do
    return $ toResponse $ Resource.XHtml $ hackagePage "Checking and uploading candidates"
      [ form ! [theclass "box", XHtml.method "POST", action "/packages/candidates/", enctype "multipart/form-data"] <<
            [ input ! [thetype "file", name "package"]
            , input ! [thetype "submit", value "Upload candidate"]
            ]
      ]

serveCandidateMaintain :: CoreResource -> CheckResource -> DynamicPath -> ServerPart Response
serveCandidateMaintain _ _ dpath =
        htmlResponse $
        withCandidatePath dpath $ \_ candidate ->
        withPackageAuth candidate $ \_ _ -> do
    return $ toResponse $ Resource.XHtml $ hackagePage "Maintain candidate"
        [toHtml "Here, you can delete a candidate, publish it, upload a new one, and edit the maintainer group."]
{-some useful URIs here: candidateUri check "" pkgid, packageCandidatesUri check "" pkgid, publishUri check "" pkgid-}


serveCandidatePage :: CheckFeature -> Resource -> DynamicPath -> ServerPart Response
serveCandidatePage pkg maintain dpath = htmlResponse $
                               withCandidatePath dpath $ \_ cand -> do
    candRender <- liftIO $ candidateRender pkg cand
    let PackageIdentifier pkgname version = packageId cand
        render = candPackageRender candRender
    otherVersions <- fmap (map packageVersion .
                           flip PackageIndex.lookupPackageName pkgname .
                           State.packageList) $ query State.GetPackagesState
    prefInfo <- query $ GetPreferredInfo pkgname
    let sectionHtml = [Pages.renderVersion (packageId cand) (classifyVersions prefInfo $ insert version otherVersions) Nothing,
                       Pages.renderDependencies render] ++ Pages.renderFields render
        maintainHtml = anchor ! [href $ renderResource maintain [display $ packageId cand]] << "maintain"
    -- also utilize hasIndexedPackage :: Bool
    let warningBox = case renderWarnings candRender of
            [] -> []
            warn -> [thediv ! [theclass "notification"] << [toHtml "Warnings:", unordList warn]]
    return $ toResponse $ Resource.XHtml $ haddockPage (display $ packageId cand) $
        Pages.packagePage render [maintainHtml] warningBox sectionHtml [] Nothing

servePublishForm :: CheckResource -> DynamicPath -> ServerPart Response
servePublishForm r dpath = htmlResponse $
                           withCandidatePath dpath $ \_ candidate ->
                           withPackageAuth candidate $ \_ _ -> do
    let pkgid = packageId candidate
    packages <- fmap State.packageList $ query State.GetPackagesState
    case checkPublish packages candidate of
        Just err -> throwError err
        Nothing  -> do
            return $ toResponse $ Resource.XHtml $ hackagePage "Publishing candidates"
                [form ! [theclass "box", XHtml.method "POST", action $ publishUri r "" pkgid]
                    << input ! [thetype "submit", value "Publish package"]]

serveCandidatesPage :: CoreResource -> CheckResource -> DynamicPath -> ServerPart Response
serveCandidatesPage _ check _ = do
    cands <- fmap State.candidateList $ query State.GetCandidatePackages
    return $ toResponse $ Resource.XHtml $ hackagePage "Package candidates"
      [ h2 << "Package candidates"
      , paragraph <<
          [ toHtml "Here follow all the candidate package versions on Hackage. "
          , thespan ! [thestyle "color: gray"] <<
              [ toHtml "["
              , anchor ! [href "/packages/candidates/upload"] << "upload"
              , toHtml "]" ]
          ]
      , unordList $ map showCands $ PackageIndex.allPackagesByName cands
      ]
    -- note: each of the lists here should be non-empty, according to PackageIndex
  where showCands pkgs =
            let desc = packageDescription . pkgDesc . candPkgInfo $ last pkgs
                pkgname = packageName desc
            in  [ anchor ! [href $ packageCandidatesUri check "" pkgname ] << display pkgname
                , toHtml ": "
                , toHtml $ intersperse (toHtml ", ") $ flip map pkgs $ \pkg ->
                     anchor ! [href $ candidateUri check "" (packageId pkg)] << display (packageVersion pkg)
                , toHtml $ ". " ++ description desc
                ]

servePackageCandidates :: CoreResource -> CheckResource -> Resource -> DynamicPath -> ServerPart Response
servePackageCandidates core check candPkgUp dpath =
  htmlResponse $
  withPackageName dpath $ \pkgname ->
  withCandidates pkgname $ \_ pkgs ->
    return $ toResponse $ Resource.XHtml $ hackagePage "Package candidates" $
      [ h3 << ("Candidates for " ++ display pkgname) ] ++
      case pkgs of
        [] -> [ toHtml "No candidates exist for ", packageNameLink core pkgname, toHtml ". Upload one for "
              , anchor ! [href $ renderResource candPkgUp [display pkgname]] << "this"
              , toHtml " or "
              , anchor ! [href $ "/packages/candidates/upload"] << "another"
              , toHtml " package?"
              ]
        _  -> [ unordList $ flip map pkgs $ \pkg -> anchor ! [href $ candidateUri check "" $ packageId pkg] << display (packageVersion pkg) ]

-- TODO: make publishCandidate a member of the Check feature, just like 
-- putDeprecated and putPreferred are for the Versions feature.
servePostPublish :: CoreFeature -> UserFeature -> UploadFeature
                 -> DynamicPath -> ServerPart Response
servePostPublish core users upload dpath = htmlResponse $ do
    uresult <- publishCandidate core users upload dpath False
    return $ toResponse $ Resource.XHtml $ hackagePage "Publish successful" $
      [ paragraph << [toHtml "Successfully published ", packageLink (coreResource core) (packageId $ uploadDesc uresult), toHtml "!"]
      ] ++ case uploadWarnings uresult of
        [] -> []
        warns -> [paragraph << "There were some warnings:", unordList warns]

--------------------------------------------------------------------------------
-- Preferred
-- This feature is in great need of a Pages module
serveDeprecatedSummary :: CoreResource -> DynamicPath -> ServerPart Response
serveDeprecatedSummary core _ = doDeprecatedsRender >>= \renders -> do
    return $ toResponse $ Resource.XHtml $ hackagePage "Deprecated packages"
      [ h2 << "Deprecated packages"
      , unordList $ flip map renders $ \(pkg, pkgs) -> [ packageNameLink core pkg, toHtml ": ", deprecatedText core pkgs ]
      ]

deprecatedText :: CoreResource -> [PackageName] -> Html
deprecatedText _ [] = toHtml "deprecated"
deprecatedText core pkgs = toHtml
  [ toHtml "deprecated in favor of "
  , concatHtml $ intersperse (toHtml ", ") (map (packageNameLink core) pkgs)
  ]

servePackageDeprecated :: CoreResource -> Resource -> DynamicPath -> ServerPart Response
servePackageDeprecated core deprEdit dpath =
  htmlResponse $
  withPackageName dpath $ \pkgname -> do
    mpkg <- doDeprecatedRender pkgname
    return $ toResponse $ Resource.XHtml $ hackagePage "Deprecated status"
      [ h2 << "Deprecated status"
      , paragraph <<
          [ toHtml $ case mpkg of
                Nothing   -> [packageNameLink core pkgname, toHtml " is not deprecated"]
                Just pkgs -> [packageNameLink core pkgname, toHtml " is ", deprecatedText core pkgs]
          , thespan ! [thestyle "color: gray"] <<
              [ toHtml " [maintainers: "
              , anchor ! [href $ renderResource deprEdit [display pkgname]] << "edit"
              , toHtml "]" ]
          ]
      ]

servePreferredSummary :: CoreResource -> DynamicPath -> ServerPart Response
servePreferredSummary core _ = doPreferredsRender >>= \renders -> do
    return $ toResponse $ Resource.XHtml $ hackagePage "Preferred versions"
      [ h2 << "Preferred versions"
      , case renders of
            [] -> paragraph << "There are no global preferred versions."
            _  -> unordList $ flip map renders $ \(pkgname, pref) ->
                [ packageNameLink core pkgname
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

packagePrefAbout :: CoreResource -> VersionsResource -> Maybe Resource -> PackageName -> [Html]
packagePrefAbout core versions maybeEdit pkgname =
  [ paragraph <<
      [ anchor ! [href $ preferredUri versions ""] << "Preferred and deprecated versions"
      , toHtml $ " can be used to influence Cabal's decisions about which versions of "
      , packageNameLink core pkgname
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

servePackagePreferred :: CoreResource -> VersionsResource -> Resource -> DynamicPath -> ServerPart Response
servePackagePreferred core versions prefEdit dpath =
        htmlResponse $
        withPackageAllPath dpath $ \pkgname pkgs -> do
    pref <- doPreferredRender pkgname
    let dtitle = display pkgname ++ ": preferred and deprecated versions"
    prefInfo <- query $ GetPreferredInfo pkgname
    return $ toResponse $ Resource.XHtml $ hackagePage dtitle --needs core, preferredVersions, pkgname
      [ h2 << dtitle
      , concatHtml $ packagePrefAbout core versions (Just prefEdit) pkgname
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
      , paragraph << "Blue versions are normal versions. Green are those out of any preferred version ranges. Gray are deprecated."
      , paragraph << (snd $ Pages.renderVersion
                                (PackageIdentifier pkgname $ Version [] [])
                                (classifyVersions prefInfo $ map packageVersion pkgs) Nothing)
      ]

servePutPreferred :: CoreResource -> VersionsFeature -> DynamicPath -> ServerPart Response
servePutPreferred core versions dpath =
  htmlResponse $
  withPackageName dpath $ \pkgname -> do
    putPreferred versions pkgname
    return $ toResponse $ Resource.XHtml $ hackagePage "Set preferred versions"
      [ h2 << "Set preferred versions"
      , paragraph <<
          [ toHtml "Set the "
          , anchor ! [href $ preferredPackageUri (versionsResource versions) "" pkgname] << "preferred versions"
          , toHtml " for "
          , packageNameLink core pkgname
          , toHtml "."]
      ]

servePutDeprecated :: CoreResource -> VersionsFeature -> DynamicPath -> ServerPart Response
servePutDeprecated core versions dpath = 
        htmlResponse $
        withPackageName dpath $ \pkgname -> do
    wasDepr <- putDeprecated versions pkgname
    let dtitle = if wasDepr then "Package deprecated" else "Package undeprecated"
    return $ toResponse $ Resource.XHtml $ hackagePage dtitle
       [ h2 << dtitle
       , paragraph <<
          [ toHtml "Set the "
          , anchor ! [href $ deprecatedPackageUri (versionsResource versions) "" pkgname] << "deprecated status"
          , toHtml " for "
          , packageNameLink core pkgname
          , toHtml "."]
       ]

-- deprecated: checkbox, by: text field, space-separated list of packagenames
serveDeprecateForm :: CoreResource -> VersionsResource -> DynamicPath -> ServerPart Response
serveDeprecateForm core r dpath =
  htmlResponse $
  withPackageName dpath $ \pkgname -> do
    mpkg <- doDeprecatedRender pkgname
    let (isDepr, mfield) = case mpkg of
            Just pkgs -> (True, unwords $ map display pkgs)
            Nothing -> (False, "")
    return $ toResponse $ Resource.XHtml $ hackagePage "Deprecate package"
        [paragraph << [toHtml "Configure deprecation for ", packageNameLink core pkgname],
         form . ulist ! [theclass "box", XHtml.method "POST", action $ deprecatedPackageUri r "" pkgname] <<
          [ hidden "_method" "PUT"
          , li . toHtml $ makeCheckbox isDepr "deprecated" "on" "Deprecate package"
          , li . toHtml $ makeInput [thetype "text", value mfield] "by" "Superseded by: " ++ [br, toHtml "(Optional; space-separated list of package names)"]
          , paragraph << input ! [thetype "submit", value "Set status"]
          ]]

-- preferred: text box (one version range per line). deprecated: list of text boxes with same name
servePreferForm :: CoreResource -> VersionsResource -> DynamicPath -> ServerPart Response
servePreferForm core r dpath =
  htmlResponse $
  withPackageName dpath $ \pkgname ->
  withPackageAll pkgname $ \pkgs -> do
    pref <- doPreferredRender pkgname
    let allVersions = map packageVersion pkgs
        rangesList  = rendRanges pref
        deprVersions = rendVersions pref
    return $ toResponse $ Resource.XHtml $ hackagePage "Adjust preferred versions"
        [concatHtml $ packagePrefAbout core r Nothing pkgname,
         form ! [theclass "box", XHtml.method "POST", action $ preferredPackageUri r "" pkgname] <<
          [ hidden "_method" "PUT"
          , paragraph << "Preferred version ranges."
          , paragraph << textarea ! [name "preferred", rows $ show (4::Int), cols $ show (80::Int)] << unlines rangesList
          , paragraph << "Deprecated versions."
          , toHtml $ intersperse (toHtml " ") $ map (\v -> toHtml $ makeCheckbox (v `elem` deprVersions) "deprecated" (display v) (display v)) allVersions
          , paragraph << input ! [thetype "submit", value "Set status"]
          ]]

{- [reverse index disabled]
--------------------------------------------------------------------------------
-- Reverse
serveReverse :: CoreResource -> ReverseResource -> Bool -> DynamicPath -> ServerPart Response
serveReverse core revr isRecent dpath =
  htmlResponse $
  withPackageId dpath $ \pkgid -> do
    let pkgname = packageName pkgid
    rdisp <- case packageVersion pkgid of
              Version [] [] -> withPackageAll pkgname   $ \_ -> revPackageName pkgname
              _             -> withPackageVersion pkgid $ \_ -> revPackageId pkgid
    render <- (if isRecent then renderReverseRecent else renderReverseOld) pkgname rdisp
    return $ toResponse $ Resource.XHtml $ hackagePage (display pkgname ++ " - Reverse dependencies ") $
        Pages.reversePackageRender pkgid (corePackageUri core "") revr isRecent render

serveReverseFlat :: CoreResource -> ReverseResource -> DynamicPath -> ServerPart Response
serveReverseFlat core revr dpath = htmlResponse $
                                  withPackageAllPath dpath $ \pkgname _ -> do
    revCount <- query $ GetReverseCount pkgname
    pairs <- revPackageFlat pkgname
    return $ toResponse $ Resource.XHtml $ hackagePage (display pkgname ++ "Flattened reverse dependencies") $
        Pages.reverseFlatRender pkgname (corePackageName core "") revr revCount pairs

serveReverseStats :: CoreResource -> ReverseResource -> DynamicPath -> ServerPart Response
serveReverseStats core revr dpath = htmlResponse $
                                   withPackageAllPath dpath $ \pkgname pkgs -> do
    revCount <- query $ GetReverseCount pkgname
    return $ toResponse $ Resource.XHtml $ hackagePage (display pkgname ++ "Reverse dependency statistics") $
        Pages.reverseStatsRender pkgname (map packageVersion pkgs) (corePackageUri core "") revr revCount

serveReverseList :: CoreResource -> ReverseFeature -> DynamicPath -> ServerPart Response
serveReverseList core revs _ = do
    let revr = reverseResource revs
    triple <- sortedRevSummary revs
    hackCount <- fmap (PackageIndex.indexSize . State.packageList) $ query State.GetPackagesState
    return $ toResponse $ Resource.XHtml $ hackagePage "Reverse dependencies" $
        Pages.reversePackagesRender (corePackageName core "") revr hackCount triple
-}

--------------------------------------------------------------------------------
-- Downloads
-- presently just a dumb listing
serveDownloadTop :: CoreResource -> DownloadFeature -> DynamicPath -> ServerPart Response
serveDownloadTop core downs _ = do
    pkgList <- liftIO $ sortedPackages downs
    return $ toResponse $ Resource.XHtml $ hackagePage "Total downloads" $
      [ h2 << "Downloaded packages"
      , thediv << table << downTableRows pkgList
      ]
  where
    downTableRows pkgList = 
        [ tr << [ th << "Package name", th << "Downloads" ] ] ++
        [ tr ! [theclass (if odd n then "odd" else "even")] <<
            [ td << packageNameLink core pkgname
            , td << [ toHtml $ (show count) ] ]
        | ((pkgname, count), n) <- zip pkgList [(1::Int)..] ]

--------------------------------------------------------------------------------
-- Additional package indices
packagesPage :: MonadIO m => CoreResource -> ListFeature
                          -> TagsResource -> m Response
packagesPage core listf tagf = do
    let itemFunc = renderItem core (Just tagf)
    items <- liftIO $ getAllLists listf
    return $ toResponse $ Resource.XHtml $ hackagePage "All packages by name" $
      [ h2 << "All packages by name"
      , ulist ! [theclass "packages"] << map itemFunc (Map.elems items)
      ]

--------------------------------------------------------------------------------
-- Tags
serveTagsListing :: TagsResource -> DynamicPath -> ServerPart Response
serveTagsListing tags _ = do
    tagList <- query GetTagList
    let withCounts = filter ((>0) . snd) . map (\(tg, pkgs) -> (tg, Set.size pkgs)) $ tagList
        countSort = sortBy (flip compare `on` snd) withCounts
    return $ toResponse $ Resource.XHtml $ hackagePage "Hackage tags" $
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

serveTagListing :: CoreResource -> ListFeature -> TagsResource
                -> DynamicPath -> ServerPart Response
serveTagListing core listf tagf dpath = withTagPath dpath $ \tg pkgnames -> do
    let tagd = "Packages tagged " ++ display tg
        itemFunc = renderItem core (Just tagf)
        pkgs = Set.toList pkgnames
    items <- liftIO $ makeItemList listf pkgs
    let (mtag, histogram) = Map.updateLookupWithKey (\_ _ -> Nothing) tg $ tagHistogram items
        -- make a 'related tags' section, so exclude this tag from the histogram
        count = fromMaybe 0 mtag
    return $ toResponse $ Resource.XHtml $ hackagePage tagd $
      [ h2 << tagd
      , case items of
            [] -> toHtml "No packages have this tag."
            _  -> toHtml
              [ paragraph << [if count==1 then "1 package has" else show count ++ " packages have", " this tag."]
              , paragraph ! [theclass "toc"] << [toHtml "Related tags: ", toHtml $ showHistogram histogram]
              , ulist ! [theclass "packages"] << map itemFunc items ]
      ]
 where
  showHistogram hist = (++takeHtml) . intersperse (toHtml ", ") $
        map histogramEntry $ take takeAmount sortHist
    where hsize = Map.size hist
          takeAmount = max (div (hsize*2) 3) 12
          takeHtml = if takeAmount >= hsize then [] else [toHtml ", ..."]
          sortHist = sortBy (flip compare `on` snd) $ Map.toList hist
  histogramEntry (tg', count) = anchor ! [href $ tagUri tagf "" tg'] << display tg' +++ (" (" ++ show count ++ ")")

putPackageTags :: CoreResource -> TagsFeature -> DynamicPath -> ServerPart Response
putPackageTags core tags dpath =
  htmlResponse $
  withPackageAllPath dpath $ \pkgname _ -> do
    putTags tags pkgname
    return $ toResponse $ Resource.XHtml $ hackagePage "Set tags"
        [toHtml "Put tags for ", packageNameLink core pkgname]

-- serve form for editing, to be received by putTags
serveTagsForm :: CoreResource -> TagsResource -> DynamicPath -> ServerPart Response
serveTagsForm core tags dpath =
  htmlResponse $
  withPackageName dpath $ \pkgname -> do
    currTags <- query (TagsForPackage pkgname)
    let tagsStr = concat . intersperse ", " . map display . Set.toList $ currTags
    return $ toResponse $ Resource.XHtml $ hackagePage "Edit package tags"
      [paragraph << [toHtml "Set tags for ", packageNameLink core pkgname],
       form ! [theclass "box", XHtml.method "POST", action $ packageTagsUri tags "" pkgname] <<
        [ hidden "_method" "PUT"
        , dlist . ddef . toHtml $ makeInput [thetype "text", value tagsStr] "tags" "Set tags to "
        , paragraph << input ! [thetype "submit", value "Set tags"]
        ]]

--------------------------------------------------------------------------------
-- Searching
servePackageFind :: CoreResource -> ListFeature -> NamesFeature
                 -> TagsResource -> DynamicPath -> ServerPart Response
servePackageFind core listf names tagf _ = packageFindWith $ \mstr -> case mstr of
    Nothing -> return $ toResponse $ Resource.XHtml $
                        hackagePage "Text search" $ searchForm ""
    Just (str, texts) -> do
        let itemFunc = renderItem core (Just tagf)
        (exact, text) <- searchFindPackage names str texts
        exactItems <- liftIO $ makeItemList listf exact
        textItems <- liftIO $ makeItemList listf text
        return $ toResponse $ Resource.XHtml $ hackagePageWith [noIndex] "Text search" $
          [ toHtml $ searchForm str
          , h2 << "Exact matches"
          , case exact of [] -> toHtml "None";
                          _ -> ulist ! [theclass "packages"] << map itemFunc exactItems
          , h2 << "Text matches"
          , case texts of
                False -> toHtml "Try a longer word."
                True  -> ulist ! [theclass "packages"] << map itemFunc textItems
          ]
  where searchForm str =             
          [ h2 << "Text search"
          , paragraph << "Search for all package descriptions containing a given string. This looks for the search text anywhere it can find it, ignoring punctuation and letter case. It is mainly a replacement for Ctrl+F on the main packages page presently."
          , form ! [theclass "box", XHtml.method "GET", action "/packages/find"] <<
                [ toHtml $ makeInput [value str] "name" "Look for "
                , input ! [thetype "submit", value "Go!"]
                ]
          , paragraph <<
              [ toHtml "Use ", anchor ! [href "http://holumbus.fh-wedel.de/hayoo/hayoo.html"] << "Hayoo"
              , toHtml " to search module and function names and "
              , anchor ! [href "http://www.haskell.org/hoogle/"] << "Hoogle"
              , toHtml " to fuzzily search type signatures and function names."]
          ]

----------------------------------------------------
-- HTML utilities

htmlResponse :: ServerPartE a -> ServerPart a
htmlResponse part = runServerPartE (handleErrorResponse htmlError part)
  where
    htmlError :: ErrorResponse -> ServerPartE Response
    htmlError (ErrorResponse errCode errTitle message) =
        resp errCode $ toResponse
            $ Resource.XHtml $ hackagePage errorStr [h2 << errorStr, paragraph << errorToHtml message]
      where
        errorStr = "Error: " ++ errTitle

    errorToHtml :: [MessageSpan] -> [Html]
    errorToHtml []               = []
    errorToHtml (MText x    :xs) = toHtml x: errorToHtml xs
    errorToHtml (MLink x url:xs) = (anchor ! [href url] << x): errorToHtml xs

packageLink :: CoreResource -> PackageId -> Html
packageLink core pkgid = anchor ! [href $ corePackageUri core "" pkgid] << display pkgid

packageNameLink :: CoreResource -> PackageName -> Html
packageNameLink core pkgname = anchor ! [href $ corePackageName core "" pkgname] << display pkgname

-- Prevents page indexing (e.g. for search pages).
noIndex :: Html
noIndex = meta ! [name "robots", content "noindex"]

renderItem :: CoreResource -> Maybe TagsResource -> PackageItem -> Html
renderItem core mtagf item = li ! classes <<
      [ packageNameLink core pkgname
      , toHtml $ " " ++ ptype (itemHasLibrary item) (itemNumExecutables item)
                     ++ ": " ++ itemDesc item
      , case mtagf of
            Nothing -> noHtml
            Just tagf -> " (" +++ renderTags tagf (itemTags item) +++ ")"
      ]
  where pkgname = itemName item
        ptype _ 0 = "library"
        ptype lib num = (if lib then "library and " else "")
                     ++ (case num of 1 -> "program"; _ -> "programs")
        classes = case classList of [] -> []; _ -> [theclass $ unwords classList]
        classList = (case itemDeprecated item of Nothing -> []; _ -> ["deprecated"])

renderTags :: TagsResource -> Set Tag -> [Html]
renderTags tagf tags = intersperse (toHtml ", ")
    (map (\tg -> anchor ! [href $ tagUri tagf "" tg] << display tg)
      $ Set.toList tags)

