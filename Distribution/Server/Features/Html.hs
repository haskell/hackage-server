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
import Distribution.Server.Features.DownloadCount
import Distribution.Server.Features.PreferredVersions
import Distribution.Server.Features.ReverseDependencies
import Distribution.Server.Types
import Distribution.Server.Error
import Distribution.Server.Resource

import qualified Distribution.Server.ResourceTypes as Resource
import qualified Distribution.Server.Pages.Package as Pages
import qualified Distribution.Server.Packages.State as State
import qualified Distribution.Server.Users.State as State
import qualified Distribution.Server.Distributions.State as State
--import qualified Distribution.Server.Auth.Basic as Auth

import Distribution.Server.Users.Types
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.PackageIndex as PackageIndex
import Distribution.Server.Users.Group (UserGroup(..))
import Distribution.Server.Distributions.Distributions (DistroPackageInfo(..))
import Distribution.Server.Packages.Preferred
import Distribution.Server.Packages.Reverse

import Distribution.Server.Pages.Template (hackagePage)
import qualified Distribution.Server.Pages.Group as Pages
import qualified Distribution.Server.Pages.Reverse as Pages
import Text.XHtml.Strict
import qualified Text.XHtml.Strict as XHtml
import Text.XHtml.Table (simpleTable)

import Happstack.Server
import Happstack.State (query)
import Distribution.Package
import Distribution.Version
import Distribution.Text (display)
import Data.List (intersperse, insert)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
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

-- this feature provides the HTML view to the models of other features
-- currently it uses the xhtml package to render HTML (Text.XHtml.Strict)
--
-- This module is somewhat temporary, in that a more advanced (and better-looking)
-- HTML scheme should come about later on.
initHtmlFeature :: Config -> CoreFeature -> PackagesFeature -> UploadFeature -> CheckFeature -> UserFeature -> VersionsFeature -> ReverseFeature -> IO HtmlFeature
initHtmlFeature _ core pkg upload check user version reversef = do
    -- resources to extend
    let cores = coreResource core
        users = userResource user
        uploads = uploadResource upload
        checks  = checkResource check
        versions = versionsResource version
        reverses = reverseResource reversef
    -- pages defined for the HTML feature in particular
    let editDeprecated  = (resourceAt "/package/:package/deprecated/edit") { resourceGet = [("html", serveDeprecateForm versions)] }
        editPreferred   = (resourceAt "/package/:package/preferred/edit") { resourceGet = [("html", servePreferForm versions)] }
        maintainPackage = (resourceAt "/package/:package/maintain") { resourceGet = [("html", serveMaintainLinks editDeprecated editPreferred)] }
    return HtmlFeature
     { htmlResources =
        -- core
         [ (extendResource $ corePackagePage cores) { resourceGet = [("html", servePackagePage cores pkg reverses maintainPackage)] }
         --, (extendResource $ coreIndexPage cores) { resourceGet = [("html", serveIndexPage)] }, currently in 'core' feature
         --, (extendResource $ corePackagesPage cores) { resourceGet = [("html", servePackageIndex)] }, currently in 'packages' feature
         , maintainPackage
        -- users
         , (extendResource $ userList users) { resourceGet = [("html", serveUserList users)] }
            -- list of users with user links; if admin, a link to add user page
         , (resourceAt "/users/register") { resourceGet = [("html", addUserForm users)] }
            -- form to post to /users/
         , (extendResource $ userPage users) { resourceGet = [("html", serveUserPage users)] }
            -- user page with link to password form and list of groups (how to do this?)
         , (extendResource $ passwordResource users) { resourceGet = [("html", servePasswordForm users)] }
            -- form to PUT password
         --, (extendResource $ enabledResource users) { resourceGet = [("html", serveEnabledForm)] }
            -- form to enable or disable users (admin only)

        -- uploads
         --, (extendResource $ uploadIndexPage uploads) { resourcePost = [("html", serveUploadResult)] }
            -- serve upload result as HTML.. er
         , (resourceAt "/packages/upload") { resourceGet = [("html", serveUploadForm)] }
            -- form for uploading

        -- checks
         , (extendResource $ candidatePage checks) { resourceGet = [("html", serveCandidatePage check)] }
            -- package page for a candidate
         --, (extendResource $ candidatesPage checks) { resourceGet = [("html", serveCandidatesPage)] }
            -- list of all packages which have candidates
         , (resourceAt "/packages/candidates/upload") { resourceGet = [("html", serveCandidateUploadForm)] }
            -- form for uploading candidate
         --, (extendResource $ packageCandidatePage checks) { resourceGet = [("html", servePackageCandidatesPage)] }
            -- list of candidates + forms for maintainers to upload
         , (extendResource $ publishPage checks) { resourceGet = [("html", servePublishForm checks)] }
            -- form for publishing package

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
          , (extendResource $ preferredResource versions) { resourceGet = [("html", servePreferredSummary versions)] }
          , (extendResource $ preferredPackageResource versions) { resourceGet = [("html", servePackagePreferred versions)] }
          , (extendResource $ deprecatedResource versions) { resourceGet = [("html", serveDeprecatedSummary versions)] }
          , (extendResource $ deprecatedPackageResource versions) { resourceGet = [("html", servePackageDeprecated versions)] }
        -- reverse
          , (extendResource $ reversePackage reverses) { resourceGet = [("html", serveReverse cores reverses True)] }
          , (extendResource $ reversePackageOld reverses) { resourceGet = [("html", serveReverse cores reverses False)] }
          , (extendResource $ reversePackageAll reverses) { resourceGet = [("html", serveReverseFlat cores reverses)] }
          , (extendResource $ reversePackageStats reverses) { resourceGet = [("html", serveReverseStats cores reverses)] }
          , (extendResource $ reversePackages reverses) { resourceGet = [("html", serveReverseList cores reverses)] }
         ]
        -- and user groups. package maintainers, trustees, admins
        ++ htmlGroupResource user (packageGroupResource uploads)
        ++ htmlGroupResource user (trusteeResource uploads)
        ++ htmlGroupResource user (adminResource users)
     }

---- Core
-- Currently the main package page is thrown together by querying a bunch
-- of features about their attributes for the given package. It'll need
-- reorganizing to look aesthetic, as opposed to the sleek and simple current
-- design that takes the 1990s school of web design.
servePackagePage :: CoreResource -> PackagesFeature -> ReverseResource
                 -> Resource -> DynamicPath -> ServerPart Response
servePackagePage core pkgr revr maintain dpath =
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
    let beforeHtml = [Pages.renderVersion realpkg (classifyVersions prefInfo $ map packageVersion pkgs),
                      Pages.renderDependencies render]
    -- and other package indices
    distributions <- query $ State.PackageStatus pkgname
    revCount <- revPackageSummary realpkg
    let distHtml = case distributions of
            [] -> []
            _  -> [("Distributions", concatHtml . intersperse (toHtml ", ") $ map showDist distributions)]
        afterHtml  = distHtml ++ [Pages.reversePackageSummary realpkg revr revCount]
    -- bottom sections, currently only documentation
    hasDocs  <- query $ State.HasDocumentation realpkg
    let docURL | hasDocs   = Just $ "/package" </> display pkgid </> "documentation"
               | otherwise = Nothing
    -- extra features like tags and downloads
    let maintainLink = paragraph ! [thestyle "font-size: small"] <<
            [ toHtml "["
            , anchor ! [href $ renderResource' maintain dpath] << toHtml "maintain"
            , toHtml "]"]
    (totalDown, versionDown) <- perVersionDownloads pkg
    deprs <- query $ GetDeprecatedFor pkgname
    let downBox = thediv ! [theclass "floatright"] <<
           [paragraph ! [thestyle "color: gray"] << "Downloads",
            paragraph << [show totalDown, " total"],
            paragraph << [show versionDown, " for ", display (packageVersion pkg)]]
        deprHtml = case deprs of
          Just fors -> paragraph ! [thestyle "color: red"] << [toHtml "Deprecated", case fors of
            [] -> noHtml
            _  -> concatHtml . (toHtml "in favor of ":) . intersperse (toHtml ", ") .
                  map (\for -> anchor ! [href $ corePackageName core "" for] << display for) $ fors]
          Nothing -> noHtml
    -- and put it all together
    returnOk $ toResponse $ Resource.XHtml $
        Pages.packagePage render [downBox, deprHtml, maintainLink] (beforeHtml ++ middleHtml ++ afterHtml) [] docURL
  where
    showDist (dname, info) = toHtml (display dname ++ ":") +++
        anchor ! [href $ distroUrl info] << toHtml (display $ distroVersion info)

-- TODO
serveMaintainLinks :: Resource -> Resource -> DynamicPath -> ServerPart Response
serveMaintainLinks _ _ dpath = htmlResponse $
                               withPackageAllPath dpath $ \pkgname _ ->
                               withPackageNameAuth pkgname $ \_ _ -> do
    returnOk $ toResponse $ Resource.XHtml $ hackagePage "Maintain package" [h3 << "Various links to form pages here"]

---- Users
serveUserList :: UserResource -> DynamicPath -> ServerPart Response
serveUserList users _ = do
    userlist <- fmap (Map.keys . Users.userNameMap) $ query State.GetUserDb
    let hlist = unordList $ map (\uname -> anchor ! [href $ userPageUri users "" uname] << display uname) userlist
    ok $ toResponse $ Resource.XHtml $ hackagePage "Hackage users" [h3 << "Hackage users", hlist]

serveUserPage :: UserResource -> DynamicPath -> ServerPart Response
serveUserPage _ dpath = htmlResponse $ withUserPath dpath $ \_ info -> do
    let uname = userName info
    returnOk $ toResponse $ Resource.XHtml $ hackagePage (display uname) [ h3 << display uname, toHtml "[User groups, links to pages for configuring user options]" ]

--username, password, repeat-password
addUserForm :: UserResource -> DynamicPath -> ServerPart Response
addUserForm r _ = htmlResponse $ do
    returnOk $ toResponse $ Resource.XHtml $ hackagePage "Register account"
      [ paragraph << "Register a user account here."
      , form ! [theclass "box", XHtml.method "POST", action $ userListUri r ""] <<
            [ simpleTable [] []
                [ makeInput [thetype "text"] "username" "User name"
                , makeInput [thetype "password"] "password" "Password"
                , makeInput [thetype "password"] "repeat-password" "Confirm password"
                ]
            , paragraph << input ! [thetype "submit", value "Create user"]
            ]
      ]

--field password, repeat-password, checkbox auth
servePasswordForm :: UserResource -> DynamicPath -> ServerPart Response
servePasswordForm r dpath = htmlResponse $
                            withUserPath dpath $ \_ userinfo -> do
    let uname = userName userinfo
    returnOk $ toResponse $ Resource.XHtml $ hackagePage "Change password"
    -- TODO: expose some of the functionality in changePassword function to determine if permissions are correct
    -- before serving this form (either admin or user)
      [ toHtml "Change your password. You'll be prompted for authentication upon submission, if you haven't logged in already."
      , form ! [theclass "box", XHtml.method "POST", action $ userPasswordUri r uname] <<
            [ simpleTable [] []
                [ makeInput [thetype "password"] "password" "Password"
                , makeInput [thetype "password"] "repeat-password" "Confirm password"
                ]
            , toHtml $ makeCheckbox True "auth" "on" "Use digest auth"
            , hidden "_method" "PUT" --method override
            , paragraph << input ! [thetype "submit", value "Change password"]
            ]
      ]

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
        returnOk . toResponse . Resource.XHtml $ Pages.groupPage
            unames Nothing Nothing (groupDesc group)
    getEditList dpath = withGroup (groupGen dpath) $ \group ->
                        withGroupEditAuth group $ \canAdd canDelete -> do
        userlist <- liftIO . queryUserList $ group
        unames <- query $ State.ListGroupMembers userlist
        let baseUri = renderResource' groupR dpath
            maybeUri b = if b then Just baseUri else Nothing
        returnOk . toResponse . Resource.XHtml $ Pages.groupPage
            unames (maybeUri canAdd) (maybeUri canDelete) (groupDesc group)
    postUser dpath = withGroup (groupGen dpath) $ \group -> do
        res <- groupAddUser users group dpath
        case res of
            Left err -> returnError' err
            Right {} -> goToList dpath
    deleteFromGroup dpath = withGroup (groupGen dpath) $ \group -> do
        res <- groupDeleteUser users group dpath
        case res of
            Left err -> returnError' err
            Right {} -> goToList dpath
    goToList dpath = fmap Right $ seeOther (renderResource' (groupResource r) dpath) (toResponse ())

{-
-- Currently unused, mainly because not all web browsers use eager authentication-sending
-- Setting a cookie might work here, albeit one that's stateless for the server, is not
-- used securely and only causes GUI changes, not permission overriding
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

---- Upload
serveUploadForm :: DynamicPath -> ServerPart Response
serveUploadForm _ = htmlResponse $ do
    returnOk $ toResponse $ Resource.XHtml $ hackagePage "Upload packages"
      [ paragraph << "Insert paragraphs of instructions here"
      , form ! [theclass "box", XHtml.method "POST", action "/packages/", enctype "multipart/form-data"] <<
            [ input ! [thetype "file", name "package"]
            , input ! [thetype "submit", value "Upload package"]
            ]
      ]

---- Candidates
serveCandidateUploadForm :: DynamicPath -> ServerPart Response
serveCandidateUploadForm _ = htmlResponse $ do
    returnOk $ toResponse $ Resource.XHtml $ hackagePage "Checking and uploading candidates"
      [ form ! [theclass "box", XHtml.method "POST", action "/packages/candidates/", enctype "multipart/form-data"] <<
            [ input ! [thetype "file", name "package"]
            , input ! [thetype "submit", value "Upload candidate"]
            ]
      ]

serveCandidatePage :: CheckFeature -> DynamicPath -> ServerPart Response
serveCandidatePage pkg dpath = htmlResponse $
                               withCandidatePath dpath $ \_ cand -> do
    candRender <- liftIO $ candidateRender pkg cand
    let PackageIdentifier pkgname version = packageId cand
        render = candPackageRender candRender
    otherVersions <- fmap (map packageVersion .
                           flip PackageIndex.lookupPackageName pkgname .
                           State.packageList) $ query State.GetPackagesState
    prefInfo <- query $ GetPreferredInfo pkgname
    let sectionHtml = [Pages.renderVersion (packageId cand) (classifyVersions prefInfo $ insert version otherVersions),
                       Pages.renderDependencies render] ++ Pages.renderFields render
    -- also utilize renderWarnings :: [String] and hasIndexedPackage :: Bool
    let warningBox = case renderWarnings candRender of
            [] -> []
            warn -> [thediv ! [theclass "box"] << [toHtml "Warnings:", unordList warn]]
    returnOk $ toResponse $ Resource.XHtml $ Pages.packagePage render warningBox sectionHtml [] Nothing

servePublishForm :: CheckResource -> DynamicPath -> ServerPart Response
servePublishForm r dpath = htmlResponse $
                           withCandidatePath dpath $ \_ candidate ->
                           withPackageAuth candidate $ \_ _ -> do
    let pkgid = packageId candidate
    packages <- fmap State.packageList $ query State.GetPackagesState
    case checkPublish packages candidate of
        Just err -> returnError' err
        Nothing  -> do
            returnOk $ toResponse $ Resource.XHtml $ hackagePage "Publishing candidates"
                [form ! [theclass "box", XHtml.method "POST", action $ publishUri r "" pkgid]
                    << input ! [thetype "submit", value "Publish package"]]

-- Preferred
-- TODO: use HTML, not text
serveDeprecatedSummary :: VersionsResource -> DynamicPath -> ServerPart Response
serveDeprecatedSummary r _ = doDeprecatedsRender >>=
    return . toResponse . unlines . map (\(pkg, pkgs) -> display pkg ++ ": " ++ case pkgs of
        [] -> "deprecated"
        _  -> show pkgs)

servePackageDeprecated :: VersionsResource -> DynamicPath -> ServerPart Response
servePackageDeprecated r dpath = htmlResponse $
                                 withPackageName dpath $ \pkgname ->
                                 responseWith (doDeprecatedRender pkgname) $ \mpkg ->
    returnOk . toResponse $ case mpkg of
        Nothing   -> display pkgname ++ " is not deprecated"
        Just pkgs -> display pkgname ++ " is " ++ show pkgs

servePreferredSummary :: VersionsResource -> DynamicPath -> ServerPart Response
servePreferredSummary r _ = doPreferredsRender >>= return . toResponse . show

servePackagePreferred :: VersionsResource -> DynamicPath -> ServerPart Response
servePackagePreferred r dpath = htmlResponse $
                                withPackageName dpath $ \pkgname ->
                                responseWith (doPreferredRender pkgname) $ \pref ->
    returnOk . toResponse . unlines $
        (case rendRanges pref of
            [] -> ["No preferred versions"]
            prefs -> "Preferred versions:":map (" * "++) prefs)
          ++
        (case rendVersions pref of
            []   -> ["No deprecated versions"]
            deprs -> ["Deprecated versions: " ++ show (map display deprs)])

-- deprecated: checkbox, by: text field, space-separated list of packagenames
serveDeprecateForm :: VersionsResource -> DynamicPath -> ServerPart Response
serveDeprecateForm r dpath = htmlResponse $
                             withPackageName dpath $ \pkgname ->
                             responseWith (doDeprecatedRender pkgname) $ \mpkg -> do
    let (isDepr, mfield) = case mpkg of
            Just pkgs -> (True, unwords $ map display pkgs)
            Nothing -> (False, "")
    returnOk $ toResponse $ Resource.XHtml $ hackagePage "Deprecate package"
        [form ! [theclass "box", XHtml.method "POST", action $ deprecatedPackageUri r "" pkgname] <<
          [ hidden "_method" "PUT"
          , toHtml $ makeCheckbox isDepr "deprecated" "on" "Deprecate package"
          , dlist . ddef . toHtml $ makeInput [thetype "text", value mfield] "by" "in favor of "
          , paragraph << input ! [thetype "submit", value "Set status"]
          ]]

-- preferred: text box (one version range per line). deprecated: list of text boxes with same name
servePreferForm :: VersionsResource -> DynamicPath -> ServerPart Response
servePreferForm r dpath = htmlResponse $
                          withPackageName dpath $ \pkgname ->
                          withPackageAll pkgname $ \pkgs ->
                          responseWith (doPreferredRender pkgname) $ \pref -> do
    let allVersions = map packageVersion pkgs
        rangesList  = rendRanges pref
        deprVersions = rendVersions pref
    returnOk $ toResponse $ Resource.XHtml $ hackagePage "Adjust preferred versions"
        [form ! [theclass "box", XHtml.method "POST", action $ preferredPackageUri r "" pkgname] <<
          [ hidden "_method" "PUT"
          , paragraph << "Preferred version ranges."
          , paragraph << textarea ! [name "preferred", rows $ show (4::Int), cols $ show (80::Int)] << unlines rangesList
          , paragraph << "Deprecated versions."
          , toHtml $ intersperse (toHtml " ") $ map (\v -> toHtml $ makeCheckbox (v `elem` deprVersions) "deprecated" (display v) (display v)) allVersions
          , paragraph << input ! [thetype "submit", value "Set status"]
          ]]


-- Reverse

serveReverse :: CoreResource -> ReverseResource -> Bool -> DynamicPath -> ServerPart Response
serveReverse core revr isRecent dpath = htmlResponse $
                                       withPackageId dpath $ \pkgid -> do
    let pkgname = packageName pkgid
    let mdisp = case packageVersion pkgid of
            Version [] [] -> withPackageAll pkgname $ \_ -> fmap Right $ revPackageName pkgname
            _ -> withPackageVersion pkgid $ \_ -> fmap Right $ revPackageId pkgid
    responseWith mdisp $ \rdisp -> do
        render <- (if isRecent then renderReverseRecent else renderReverseOld) pkgname rdisp
        returnOk $ toResponse $ Resource.XHtml $ hackagePage (display pkgname ++ " - Reverse dependencies ") $
            Pages.reversePackageRender pkgid (corePackageUri core "") revr isRecent render

serveReverseFlat :: CoreResource -> ReverseResource -> DynamicPath -> ServerPart Response
serveReverseFlat core revr dpath = htmlResponse $
                                  withPackageAllPath dpath $ \pkgname _ -> do
    revCount <- query $ GetReverseCount pkgname
    pairs <- revPackageFlat pkgname
    returnOk $ toResponse $ Resource.XHtml $ hackagePage (display pkgname ++ "Flattened reverse dependencies") $
        Pages.reverseFlatRender pkgname (corePackageName core "") revr revCount pairs

serveReverseStats :: CoreResource -> ReverseResource -> DynamicPath -> ServerPart Response
serveReverseStats core revr dpath = htmlResponse $
                                   withPackageAllPath dpath $ \pkgname pkgs -> do
    revCount <- query $ GetReverseCount pkgname
    returnOk $ toResponse $ Resource.XHtml $ hackagePage (display pkgname ++ "Reverse dependency statistics") $
        Pages.reverseStatsRender pkgname (map packageVersion pkgs) (corePackageUri core "") revr revCount

serveReverseList :: CoreResource -> ReverseResource -> DynamicPath -> ServerPart Response
serveReverseList core revr _ = do
    triple <- revSummary
    hackCount <- fmap (PackageIndex.indexSize . State.packageList) $ query State.GetPackagesState
    return $ toResponse $ Resource.XHtml $ hackagePage "Reverse dependencies" $
        Pages.reversePackagesRender (corePackageName core "") revr hackCount triple

----------------------------------------------------
-- HTML utilities
htmlResponse :: MServerPart Response -> ServerPart Response
htmlResponse mpart = htmlResponseWith mpart return

htmlResponseWith :: MServerPart a -> (a -> ServerPart Response) -> ServerPart Response
htmlResponseWith mpart func = mpart >>= \mres -> case mres of
    Left  err -> htmlError err
    Right res -> func res

htmlError :: ErrorResponse -> ServerPart Response
htmlError (ErrorResponse errCode errTitle message) = resp errCode $ toResponse
        $ Resource.XHtml $ hackagePage errorStr [h3 << errorStr, paragraph << errorToHtml message]
  where errorStr = "Error: " ++ errTitle

errorToHtml :: [Message] -> [Html]
errorToHtml [] = []
errorToHtml (MLink x url:xs) = (anchor ! [href url] << x): errorToHtml xs
errorToHtml (MText x    :xs) = toHtml x: errorToHtml xs

makeInput :: [HtmlAttr] -> String -> String -> [Html]
makeInput attrs fname labelName = [label ! [thefor fname] << labelName,
                                   input ! (attrs ++ [name fname, identifier fname])]

makeCheckbox :: Bool -> String -> String -> String -> [Html]
makeCheckbox isChecked fname fvalue labelName = [input ! ([thetype "checkbox", name fname, identifier fname, value fvalue]
                                                 ++ if isChecked then [checked] else []),
                                        toHtml " ",
                                        label ! [thefor fname] << labelName]

