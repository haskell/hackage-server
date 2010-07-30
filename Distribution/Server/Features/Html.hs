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
import Data.List (intercalate, intersperse, insert)
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
    let editDeprecated  = (resourceAt "/package/:package/deprecated/edit") { resourceGet = [("html", serveDeprecateForm cores versions)] }
        editPreferred   = (resourceAt "/package/:package/preferred/edit") { resourceGet = [("html", servePreferForm cores versions)] }
        maintainPackage = (resourceAt "/package/:package/maintain") { resourceGet = [("html", serveMaintainLinks editDeprecated editPreferred)] }
    return HtmlFeature
     { htmlResources =
        -- core
         [ (extendResource $ corePackagePage cores) { resourceGet = [("html", servePackagePage cores pkg reverses versions maintainPackage)] }
         --, (extendResource $ coreIndexPage cores) { resourceGet = [("html", serveIndexPage)] }, currently in 'core' feature
         --, (extendResource $ corePackagesPage cores) { resourceGet = [("html", servePackageIndex)] }, currently in 'packages' feature
         , maintainPackage
        -- users
        -- TODO: registration and whatnot
         , (extendResource $ userList users) { resourceGet = [("html", serveUserList users)] } --resourcePost: admin add user
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
          , (extendResource $ preferredResource versions) { resourceGet = [("html", servePreferredSummary cores)] }
          , (extendResource $ preferredPackageResource versions) { resourceGet = [("html", servePackagePreferred cores versions editPreferred)], resourcePut = [("html", servePutPreferred cores version)] }
          , (extendResource $ deprecatedResource versions) { resourceGet = [("html", serveDeprecatedSummary cores)]  }
          , (extendResource $ deprecatedPackageResource versions) { resourceGet = [("html", servePackageDeprecated cores editDeprecated)], resourcePut = [("html", servePutDeprecated cores version)] }
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
                 -> VersionsResource -> Resource -> DynamicPath
                 -> ServerPart Response
servePackagePage core pkgr revr versions maintain dpath =
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
            , anchor ! [href $ renderResource maintain [display pkgname]] << toHtml "maintain"
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

serveUserPage :: UserFeature -> DynamicPath -> ServerPart Response
serveUserPage users dpath = htmlResponse $ withUserPath dpath $ \uid info -> do
    let uname = userName info
    strs <- getGroupIndex (groupIndex users) uid
    returnOk $ toResponse $ Resource.XHtml $ hackagePage (display uname)
      [ h3 << display uname
      , case strs of
            [] -> noHtml
            _  -> toHtml
              [ toHtml $ display uname ++ " is part of the following groups:"
              , unordList $ map (\str -> anchor ! [href str] << str) strs
              ]
      ]

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
                            withUserPath dpath $ \_ userInfo -> do
    let uname = userName userInfo
    returnOk $ toResponse $ Resource.XHtml $ hackagePage "Change password"
    -- TODO: expose some of the functionality in changePassword function to determine if permissions are correct
    -- before serving this form (either admin or user)
      [ toHtml "Change your password. You'll be prompted for authentication upon submission, if you haven't logged in already."
      , form ! [theclass "box", XHtml.method "POST", action $ userPasswordUri r "" uname] <<
            [ simpleTable [] []
                [ makeInput [thetype "password"] "password" "Password"
                , makeInput [thetype "password"] "repeat-password" "Confirm password"
                ]
            , toHtml $ makeCheckbox True "auth" "on" "Use digest auth"
            , hidden "_method" "PUT" --method override
            , paragraph << input ! [thetype "submit", value "Change password"]
            ]
      ]

serveEnabledForm :: UserResource -> DynamicPath -> ServerPart Response
serveEnabledForm r dpath = htmlResponse $
                           withUserPath dpath $ \_ userInfo -> do
    let uname = userName userInfo
    returnOk $ toResponse $ Resource.XHtml $ hackagePage "Change user status"
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
                              withUserNamePath dpath $ \uname ->
                              responseWith (enabledAccount uname) $ \_ -> do
    returnOk $ toResponse $ Resource.XHtml $ hackagePage "Account status set"
        [toHtml "Account status set for ", anchor ! [href $ userPageUri users "" uname] << display uname]

serveDeleteUser :: DynamicPath -> ServerPart Response
serveDeleteUser dpath = htmlResponse $
                        withUserNamePath dpath $ \uname ->
                        responseWith (deleteAccount uname) $ \_ -> do
    let ntitle = "Deleted user"
    returnOk $ toResponse $ Resource.XHtml $ hackagePage ntitle [toHtml ntitle]

servePutPassword :: UserResource -> DynamicPath -> ServerPart Response
servePutPassword users dpath = htmlResponse $
                               withUserNamePath dpath $ \uname ->
                               responseWith (changePassword uname) $ \_ -> do
    returnOk $ toResponse $ Resource.XHtml $ hackagePage "Changed password"
        [toHtml "Changed password for ", anchor ! [href $ userPageUri users "" uname] << display uname]

htmlGroupResource :: UserFeature -> GroupResource -> [Resource]
htmlGroupResource users r@(GroupResource groupR userR groupGen) =
  [ (extendResource groupR) { resourceGet = [("html", htmlResponse . getList)], resourcePost = [("html", htmlResponse . postUser)] }
  , (extendResource userR) { resourceDelete = [("html", htmlResponse . deleteFromGroup)] }
  , (extendResourcePath "/edit" groupR) { resourceGet = [("html", htmlResponse . getEditList)] }
  ]
  where
    getList dpath = withGroup (liftIO $ groupGen dpath) $ \group -> do
        uidlist <- liftIO . queryUserList $ group
        unames <- query $ State.ListGroupMembers uidlist
        returnOk . toResponse . Resource.XHtml $ Pages.groupPage
            unames Nothing Nothing (groupDesc group)
    getEditList dpath = withGroup (liftIO $ groupGen dpath) $ \group ->
                        withGroupEditAuth group $ \canAdd canDelete -> do
        userlist <- liftIO . queryUserList $ group
        unames <- query $ State.ListGroupMembers userlist
        let baseUri = renderResource' groupR dpath
            maybeUri b = if b then Just baseUri else Nothing
        returnOk . toResponse . Resource.XHtml $ Pages.groupPage
            unames (maybeUri canAdd) (maybeUri canDelete) (groupDesc group)
    postUser dpath = withGroup (liftIO $ groupGen dpath) $ \group -> do
        res <- groupAddUser users group dpath
        case res of
            Left err -> returnError' err
            Right {} -> goToList dpath
    deleteFromGroup dpath = withGroup (liftIO $ groupGen dpath) $ \group -> do
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

serveUploadResult :: UploadFeature -> CoreResource -> DynamicPath -> ServerPart Response
serveUploadResult pkgf core _ = htmlResponse $
                                responseWith (uploadPackage pkgf) $ \res -> do
    let warns = uploadWarnings res
        pkgid = packageId (uploadDesc res)
    returnOk $ toResponse $ Resource.XHtml $ hackagePage "Upload successful" $
      [ paragraph << [toHtml "Successfully uploaded ", anchor ! [href $ corePackageUri core "" pkgid] << display pkgid, toHtml "!"]
      ] ++ case warns of
        [] -> []
        _  -> [paragraph << "There were some warnings:", unordList warns]

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
    let sectionHtml = [Pages.renderVersion (packageId cand) (classifyVersions prefInfo $ insert version otherVersions) Nothing,
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
serveDeprecatedSummary :: CoreResource -> DynamicPath -> ServerPart Response
serveDeprecatedSummary core _ = doDeprecatedsRender >>= \renders -> do
    return $ toResponse $ Resource.XHtml $ hackagePage "Deprecated packages"
      [ h3 << "Deprecated packages"
      , toHtml $ flip map renders $ \(pkg, pkgs) -> [ packageNameLink core pkg, toHtml ": ", deprecatedText core pkgs ]
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
        withPackageName dpath $ \pkgname ->
        responseWith (doDeprecatedRender pkgname) $ \mpkg ->
    returnOk $ toResponse $ Resource.XHtml $ hackagePage "Deprecated status"
      [ h3 << "Deprecated status"
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
      [ h3 << "Preferred versions"
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
  where varList _    [] = toHtml "none"
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
        withPackageAllPath dpath $ \pkgname pkgs ->
        responseWith (doPreferredRender pkgname) $ \pref -> do
    let dtitle = display pkgname ++ ": preferred and deprecated versions"
    prefInfo <- query $ GetPreferredInfo pkgname
    returnOk $ toResponse $ Resource.XHtml $ hackagePage dtitle --needs core, preferredVersions, pkgname
      [ h3 << dtitle
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
        withPackageName dpath $ \pkgname ->
        responseWith (putPreferred versions pkgname) $ \_ ->
    returnOk $ toResponse $ Resource.XHtml $ hackagePage "Set preferred versions"
       [ h3 << "Set preferred versions"
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
        withPackageName dpath $ \pkgname ->
        responseWith (putDeprecated versions pkgname) $ \wasDepr -> do
    let dtitle = if wasDepr then "Package deprecated" else "Package undeprecated"
    returnOk $ toResponse $ Resource.XHtml $ hackagePage dtitle
       [ h3 << dtitle
       , paragraph <<
          [ toHtml "Set the "
          , anchor ! [href $ deprecatedPackageUri (versionsResource versions) "" pkgname] << "deprecated status"
          , toHtml " for "
          , packageNameLink core pkgname
          , toHtml "."]
       ]

-- deprecated: checkbox, by: text field, space-separated list of packagenames
serveDeprecateForm :: CoreResource -> VersionsResource -> DynamicPath -> ServerPart Response
serveDeprecateForm core r dpath = htmlResponse $
                             withPackageName dpath $ \pkgname ->
                             responseWith (doDeprecatedRender pkgname) $ \mpkg -> do
    let (isDepr, mfield) = case mpkg of
            Just pkgs -> (True, unwords $ map display pkgs)
            Nothing -> (False, "")
    returnOk $ toResponse $ Resource.XHtml $ hackagePage "Deprecate package"
        [paragraph << [toHtml "Configure deprecation for ", packageNameLink core pkgname],
         form ! [theclass "box", XHtml.method "POST", action $ deprecatedPackageUri r "" pkgname] <<
          [ hidden "_method" "PUT"
          , toHtml $ makeCheckbox isDepr "deprecated" "on" "Deprecate package"
          , dlist . ddef . toHtml $ makeInput [thetype "text", value mfield] "by" "in favor of "
          , paragraph << input ! [thetype "submit", value "Set status"]
          ]]

-- preferred: text box (one version range per line). deprecated: list of text boxes with same name
servePreferForm :: CoreResource -> VersionsResource -> DynamicPath -> ServerPart Response
servePreferForm core r dpath =
        htmlResponse $
        withPackageName dpath $ \pkgname ->
        withPackageAll pkgname $ \pkgs ->
        responseWith (doPreferredRender pkgname) $ \pref -> do
    let allVersions = map packageVersion pkgs
        rangesList  = rendRanges pref
        deprVersions = rendVersions pref
    returnOk $ toResponse $ Resource.XHtml $ hackagePage "Adjust preferred versions"
        [concatHtml $ packagePrefAbout core r Nothing pkgname,
         form ! [theclass "box", XHtml.method "POST", action $ preferredPackageUri r "" pkgname] <<
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

packageLink :: CoreResource -> PackageId -> Html
packageLink core pkgid = anchor ! [href $ corePackageUri core "" pkgid] << display pkgid

packageNameLink :: CoreResource -> PackageName -> Html
packageNameLink core pkgname = anchor ! [href $ corePackageName core "" pkgname] << display pkgname

makeInput :: [HtmlAttr] -> String -> String -> [Html]
makeInput attrs fname labelName = [label ! [thefor fname] << labelName,
                                   input ! (attrs ++ [name fname, identifier fname])]

makeCheckbox :: Bool -> String -> String -> String -> [Html]
makeCheckbox isChecked fname fvalue labelName = [input ! ([thetype "checkbox", name fname, identifier fname, value fvalue]
                                                 ++ if isChecked then [checked] else []),
                                        toHtml " ",
                                        label ! [thefor fname] << labelName]

