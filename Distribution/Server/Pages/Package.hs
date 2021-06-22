-- Body of the HTML page for a package
{-# LANGUAGE PatternGuards, RecordWildCards, ViewPatterns #-}
module Distribution.Server.Pages.Package
  ( packagePage
  , renderPackageFlags
  , renderDependencies
  , renderDetailedDependencies
  , renderVersion
  , renderFields
  , renderDownloads
  , renderChangelog
  , moduleSection
  , readmeSection
  , rendLicense
  , maintainField
  , categoryField
  , linkField
  , descriptionSection
  , renderHaddock
  , maintainerSection
  , downloadSection
  , moduleToDocUrl
  ) where

import Distribution.Server.Features.PreferredVersions

import Distribution.Server.Pages.Template (hackagePageWithHead)
import qualified Distribution.Server.Pages.Package.HaddockParse as Haddock
import Distribution.Server.Pages.Package.HaddockHtml
import Distribution.Server.Packages.ModuleForest
import Distribution.Server.Packages.Render
import Distribution.Server.Users.Types (userStatus, userName, isActiveAccount)
import Distribution.Server.Util.Markdown (renderMarkdownRel, supposedToBeMarkdown)
import Data.TarIndex (TarIndex)
import qualified Data.TarIndex as Tar

import qualified Distribution.ModuleName as Module
import Distribution.ModuleName (ModuleName)
import Distribution.Package
import Distribution.PackageDescription as P
import Distribution.Version
import Distribution.Types.CondTree
import Distribution.Text        (display)
import Distribution.Utils.ShortText (fromShortText, ShortText)
import Text.XHtml.Strict hiding (p, name, title, content)
import qualified Text.XHtml.Strict

import Data.Monoid              (Monoid(..), (<>))
import Data.Maybe               (fromMaybe, maybeToList, isJust, mapMaybe)
import Data.List                (intersperse, intercalate)
import Control.Arrow            (second)
import System.FilePath.Posix    ((</>), (<.>), takeFileName)
import Data.Time.Locale.Compat  (defaultTimeLocale)
import Data.Time.Format         (formatTime)

import qualified Documentation.Haddock.Markup as Haddock

import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T (lenientDecode)
import qualified Data.ByteString.Lazy as BS (ByteString, toStrict)

instance HTML ShortText where
   toHtml = toHtml . fromShortText

packagePage :: PackageRender -> [Html] -> [Html] -> [(String, Html)]
            -> [(String, Html)] -> Maybe TarIndex -> Maybe BS.ByteString
            -> URL -> Bool
            -> Html
packagePage render headLinks top sections
            bottom mdocIndex mreadMe
            docURL isCandidate =
    hackagePageWithHead [canonical] docTitle docBody
  where
    pkgid   = rendPkgId render
    pkgName = display $ packageName pkgid

    pkgUrl = "/package/" ++ pkgName

    canonical = thelink ! [ rel "canonical"
                          , href pkgUrl ] << noHtml
    docTitle = pkgName
            ++ case fromShortText $ synopsis (rendOther render) of
                 ""    -> ""
                 short -> ": " ++ short

    docBody = bodyTitle
          : concat [
             candidateBanner,
             renderHeads,
             top,
             pkgBody render sections docURL,
             moduleSection render mdocIndex docURL False,
             renderPackageFlags render docURL,
             downloadSection render,
             maintainerSection pkgid isCandidate,
             readmeSection render mreadMe,
             map pair bottom
           ]

    bodyTitle = case fromShortText $ synopsis (rendOther render) of
      ""    -> h1 << pkgName
      short -> h1 << [ toHtml (pkgName ++ ": ")
                     , small (toHtml short)
                     ]

    candidateBanner
      | isCandidate = [ thediv ! [theclass "candidate-info"]
                        << [ paragraph << [ strong (toHtml "This is a package candidate release!")
                                          , toHtml " Here you can preview how this package release will appear once published to the main package index (which can be accomplished via the 'maintain' link below)."
                                          , toHtml " Please note that once a package has been published to the main package index it cannot be undone!"
                                          , toHtml " Please consult the "
                                          , anchor ! [href "/upload"] << "package uploading documentation"
                                          , toHtml " for more information."
                                          ] ] ]
      | otherwise = []

    renderHeads = case headLinks of
        [] -> []
        items -> [thediv ! [thestyle "font-size: small"] <<
            (map (\item -> "[" +++ item +++ "] ") items)]

    pair (title, content) =
        toHtml [ h2 << title, content ]

-- | Body of the package page
pkgBody :: PackageRender -> [(String, Html)] -> URL -> [Html]
pkgBody render sections docURL =
    descriptionSection render docURL
 ++ propertySection sections

descriptionSection :: PackageRender -> URL -> [Html]
descriptionSection PackageRender{..} docURL =
        [thediv ! [identifier "description"] <<
           renderHaddock (moduleToDocUrl PackageRender{..} docURL)
                         (fromShortText $ description rendOther)]
     ++ readmeLink
  where
    readmeLink = case rendReadme of
      Just _ -> [ hr
                , toHtml "["
                , anchor ! [href "#readme"] << "Skip to ReadMe"
                , toHtml "]"
                ]
      _      -> []

-- | Resolve 'ModuleName' to 'URL' if module is exposed by package
moduleToDocUrl :: PackageRender -> URL -> ModuleName -> Maybe URL
moduleToDocUrl _ docURL modName = Just url
  where -- TODO: only return 'Just' links when .html target exists
    url = docURL </> (intercalate "-" (Module.components modName) ++ ".html")

renderHaddock :: (ModuleName -> Maybe URL) -> String -> [Html]
renderHaddock _ "" = []
renderHaddock modResolv desc =
  case Haddock.parse desc of
      Nothing  -> [paragraph << p | p <- paragraphs desc]
      Just doc -> [Haddock.markup (htmlMarkup modResolv) doc]

readmeSection :: PackageRender -> Maybe BS.ByteString -> [Html]
readmeSection PackageRender { rendReadme = Just (_, _etag, _, filename)
                            , rendPkgId  = pkgid }
              (Just content) =
    [ hr
    , h2 ! [identifier "readme"] << ("Readme for " ++ name)
    , toHtml "["
    , anchor ! [href "#description"] << "back to package description"
    , toHtml "]"
    , thediv ! [theclass "embedded-author-content"]
            << if supposedToBeMarkdown filename
                 then renderMarkdownRel name content
                 else pre << unpackUtf8 content
    ] where
      name = display pkgid
readmeSection _ _ = []


unpackUtf8 :: BS.ByteString -> String
unpackUtf8 = T.unpack
           . T.decodeUtf8With T.lenientDecode
           . BS.toStrict

-- Break text into paragraphs (separated by blank lines)
paragraphs :: String -> [String]
paragraphs = map unlines . paras . lines
  where paras xs = case dropWhile null xs of
                [] -> []
                xs' -> case break null xs' of
                        (para, xs'') -> para : paras xs''

downloadSection :: PackageRender -> [Html]
downloadSection PackageRender{..} =
    [ h2 << "Downloads"
    , ulist << map (li <<) downloadItems
    , metadataNote
    ]
  where
    metadataNote = if isJust rendUpdateInfo
           then Text.XHtml.Strict.p <<
                [ toHtml "Note: This package has "
                , anchor ! [href revsURL] << "metadata revisions"
                , toHtml " in the cabal description newer than included in the tarball. To unpack the package including the revisions, use 'cabal get'."
                ]
           else noHtml

    downloadItems =
      [ if rendHasTarball
          then [ anchor ! [href downloadURL] << tarGzFileName
               , toHtml << " ["
               , anchor ! [href srcURL] << "browse"
               , toHtml << "]"
               , toHtml << " (Cabal source package)"
               ]
          else [ toHtml << "Package tarball not uploaded" ]
      , case (rendHasTarball,rendUpdateInfo) of
          (False,_) ->
            [ anchor ! [href cabalURL] << "Package description" ]
          (True,Nothing) ->
            [ anchor ! [href cabalURL] << "Package description"
            , toHtml " (as included in the package)"
            ]
          (True,Just _) ->
            [ anchor ! [href cabalURL] << "Package description"
            , toHtml " ("
            , anchor ! [href revsURL] << "revised"
            , toHtml " from the package)"
            ]
      ]

    revsURL       = rendPkgUri </> "revisions/"
    downloadURL   = rendPkgUri </> display rendPkgId <.> "tar.gz"
    cabalURL      = rendPkgUri </> display (packageName rendPkgId) <.> "cabal"
    srcURL        = rendPkgUri </> "src/"
    tarGzFileName = display rendPkgId ++ ".tar.gz"

maintainerSection :: PackageId -> Bool -> [Html]
maintainerSection pkgid isCandidate =
    [ h4 << "Maintainers' corner"
    , paragraph << "For package maintainers and hackage trustees"
    , ulist << li << anchor ! [href maintainURL]
                  << "edit package information"
    ]
  where
    maintainURL | isCandidate = "candidate/maintain"
                | otherwise   = display (packageName pkgid) </> "maintain"

-- | Render a table of the package's flags and along side it a tip
-- indicating how to enable/disable flags with Cabal.
renderPackageFlags :: PackageRender -> URL -> [Html]
renderPackageFlags render docURL =
  case rendFlags render of
    [] -> mempty
    flags ->
      [h2 << "Flags"
      ,flagsTable flags
      ,tip]
  where tip =
          paragraph ! [theclass "tip"] <<
          [thespan << "Use "
          ,code "-f <flag>"
          ,thespan << " to enable a flag, or "
          ,code "-f -<flag>"
          ,thespan << " to disable that flag. "
          ,anchor ! [href tipLink] << "More info"
          ]
        tipLink = "https://cabal.readthedocs.io/en/latest/setup-commands.html#controlling-flag-assignments"
        flagsTable flags =
          table ! [theclass "flags-table"] <<
          [thead << flagsHeadings
          ,tbody << map flagRow flags]
        flagsHeadings = [th << "Name"
                        ,th << "Description"
                        ,th << "Default"
                        ,th << "Type"]
        flagRow flag =
          tr << [td ! [theclass "flag-name"]   << code (unFlagName (flagName flag))
                ,td ! [theclass "flag-desc"]   << renderHaddock (moduleToDocUrl render docURL) (flagDescription flag)
                ,td ! [theclass (if flagDefault flag then "flag-enabled" else "flag-disabled")] <<
                 if flagDefault flag then "Enabled" else "Disabled"
                ,td ! [theclass (if flagManual flag then "flag-manual" else "flag-automatic")] <<
                 if flagManual flag then "Manual" else "Automatic"]
        code = (thespan ! [theclass "code"] <<)

moduleSection :: PackageRender -> Maybe TarIndex -> URL -> Bool -> [Html]
moduleSection render mdocIndex docURL quickNav =
    maybeToList $ fmap msect (rendModules render mdocIndex)
  where msect ModSigIndex{ modIndex = m, sigIndex = s } = toHtml $
            (if not (null s)
                then [ h2 << "Signatures"
                     , renderModuleForest docURL s ]
                else []) ++
            (if not (null m)
                then [ h2 << "Modules"] ++
                     [renderDocIndexLink] ++
                     [renderModuleForest docURL m ]
                else [])
        renderDocIndexLink = case mdocIndex of
          Just tindex ->
            let docIndexURL | isJust (Tar.lookup tindex "doc-index-All.html") = docURL </> "doc-index-All.html"
                            | otherwise = docURL </> "doc-index.html"
            in  paragraph ! [thestyle "font-size: small"]
                  << ("[" +++ anchor ! [href docIndexURL] << "Index" +++ "]" +++
                      (if quickNav
                       then " [" +++ anchor ! [identifier "quickjump-trigger", href "#"] << "Quick Jump" +++ "]"
                       else mempty))
          Nothing -> mempty

propertySection :: [(String, Html)] -> [Html]
propertySection sections =
    [ h2 << "Properties"
    , tabulate $ filter (not . isNoHtml . snd) sections
    ]

tabulate :: [(String, Html)] -> Html
tabulate items = table ! [theclass "properties"] <<
        [tr << [th << t, td << d] | (t, d) <- items]


renderDependencies :: PackageRender -> (String, Html)
renderDependencies render =
    ("Dependencies", summary +++ detailsLink)
  where
    summary = case rendDepends render of
                []   -> toHtml "None"
                deps -> showDependencies deps
    detailsLink = thespan ! [thestyle "font-size: small"]
                    << (" [" +++ anchor ! [href detailURL] << "details" +++ "]")
    detailURL = rendPkgUri render </> "dependencies"

showDependencies :: [Dependency] -> Html
showDependencies deps = commaList (map showDependency deps)

showDependency ::  Dependency -> Html
showDependency (Dependency (unPackageName -> pname) vs _) = nonbreakingSpan << (showPkg +++ vsHtml)
  where vsHtml = if vs == anyVersion then noHtml
                                     else toHtml (" (" ++ display vs ++ ")")
        -- mb_vers links to latest version in range. This is a bit computationally
        -- expensive, not cache-friendly, and perhaps unexpected in some cases
        {-mb_vers = maybeLast $ filter (`withinRange` vs) $ map packageVersion $
                    PackageIndex.lookupPackageName vmap (PackageName pname)-}
        -- nonetheless, we should ensure that the package exists /before/
        -- passing along the PackageRender, which is not the case here
        showPkg = anchor ! [href . packageURL $ PackageIdentifier (mkPackageName pname) nullVersion] << pname

nonbreakingSpan :: Html -> Html
nonbreakingSpan str = thespan ! [thestyle "white-space: nowrap"] << str

renderDetailedDependencies :: PackageRender -> Html
renderDetailedDependencies pkgRender =
    tabulate $ map (second (fromMaybe noDeps . render)) targets
  where
    targets :: [(String, DependencyTree)]
    targets = maybeToList library
        ++ rendSublibraryDeps pkgRender
        ++ rendExecutableDeps pkgRender
      where
        library = (\lib -> ("library", lib)) `fmap` rendLibraryDeps pkgRender

    noDeps = list [toHtml "No dependencies"]

    render :: DependencyTree -> Maybe Html
    render (P.CondNode isBuildable deps components)
        | null deps && null comps && isBuildable == Buildable = Nothing
        | otherwise = Just $ list items
      where
        items = buildable ++ map showDependency deps ++ comps
        comps = mapMaybe renderComponent components
        buildable = case isBuildable of
                      Buildable -> []
                      NotBuildable -> [strong << "buildable:" +++ " False"]

    list :: [Html] -> Html
    list items = thediv ! [identifier "detailed-dependencies"] << unordList items

    renderComponent :: (CondBranch ConfVar [Dependency] IsBuildable)
                    -> Maybe Html
    renderComponent (CondBranch condition then' else')
        | Just thenHtml <- render then' =
                           Just $ strong << "if "
                             +++ renderCond condition
                             +++ thenHtml
                             +++ maybe noHtml ((strong << "else") +++)
                                 (render =<< else')
        | Just elseHtml <- render =<< else' =
                           Just $ strong << "if "
                             +++ renderCond (notCond condition)
                             +++ elseHtml
        | otherwise = Nothing
      where
        notCond (CNot c) = c
        notCond c = CNot c

        renderCond :: Condition ConfVar -> Html
        renderCond cond =
            case cond of
              (Var v) -> toHtml $ displayConfVar v
              (Lit b) -> toHtml $ show b
              (CNot c) -> "!" +++ renderParens 3 c
              (COr c1 c2) -> renderParens 1 c1 +++ " || " +++ renderParens 1 c2
              (CAnd c1 c2) -> renderParens 2 c1 +++ " && " +++ renderParens 2 c2
          where
            renderParens :: Int -> Condition ConfVar -> Html
            renderParens precedence c
                | COr _ _ <- c, precedence > 1 = parenthesized
                | CAnd _ _ <- c, precedence > 2 = parenthesized
                | otherwise = renderCond c
              where
                parenthesized = "(" +++ renderCond c +++ ")"

            displayConfVar (OS os) = "os(" ++ display os ++ ")"
            displayConfVar (Arch arch) = "arch(" ++ display arch ++ ")"
            displayConfVar (Flag fn) = "flag(" ++ unFlagName fn ++ ")"
            displayConfVar (Impl compilerFlavor versionRange) =
                "impl(" ++ display compilerFlavor ++ ver ++ ")"
              where
                ver = if isAnyVersion versionRange
                        then ""
                        else display versionRange

renderVersion :: PackageId -> [(Version, VersionStatus)] -> Maybe String -> (String, Html)
renderVersion (PackageIdentifier pname pversion) allVersions info =
    (if null earlierVersions && null laterVersions then "Version" else "Versions", versionList +++ infoHtml)
  where (earlierVersions, laterVersionsInc) = span ((<pversion) . fst) allVersions
        (mThisVersion, laterVersions) = case laterVersionsInc of
            (v:later) | fst v == pversion -> (Just v, later)
            later -> (Nothing, later)
        versionList = commaList $ map versionedLink earlierVersions
                               ++ (case pversion of
                                      v | v == nullVersion -> []
                                      _ -> [strong ! (maybe [] (status . snd) mThisVersion) << display pversion]
                                  )
                               ++ map versionedLink laterVersions
        versionedLink (v, s) = anchor ! (status s ++ [href $ packageURL $ PackageIdentifier pname v]) << display v
        status st = case st of
            NormalVersion -> [theclass "normal"]
            DeprecatedVersion  -> [theclass "deprecated"]
            UnpreferredVersion -> [theclass "unpreferred"]
        infoHtml = case info of Nothing -> noHtml; Just str -> " (" +++ (anchor ! [href str] << "info") +++ ")"

-- This is currently only used by the candidate view as the normal
-- package view is using the new template-based rendering
--
-- TODO: when converting the candidate view to the template-based
-- rendering the "warning" needs to be reimplemented
renderChangelog :: PackageRender -> (String, Html)
renderChangelog render =
    ("Change log", case rendChangeLog render of
                     Nothing            -> strong ! [theclass "warning"] << toHtml "None available"
                     Just (_,_,_,fname) -> anchor ! [href changeLogURL]
                                                 << takeFileName fname)
  where
    changeLogURL  = rendPkgUri render </> "changelog"

-- We don't keep currently per-version downloads in memory; if we decide that
-- it is important to show this all the time, we can reenable
renderDownloads :: Int -> Int -> {- Int -> Version -> -} (String, Html)
renderDownloads totalDown recentDown {- versionDown version -} =
    ("Downloads", toHtml $ {- show versionDown ++ " for " ++ display version ++
                      " and " ++ -} show totalDown ++ " total (" ++
                      show recentDown ++ " in last 30 days)")

renderFields :: PackageRender -> [(String, Html)]
renderFields render = [
        -- Cabal-Version
        ("License",     rendLicense render),
        ("Copyright",   toHtml $ P.copyright desc),
        ("Author",      toHtml $ author desc),
        ("Maintainer",  maintainField $ rendMaintainer render),
--        ("Stability",   toHtml $ stability desc),
        ("Category",    commaList . map categoryField $ rendCategory render),
        ("Home page",   linkField . fromShortText $ homepage desc),
        ("Bug tracker", linkField . fromShortText $ bugReports desc),
        ("Source repository", vList $ map sourceRepositoryField $ sourceRepos desc),
        ("Executables", commaList . map toHtml $ rendExecNames render),
        ("Uploaded", uncurry renderUploadInfo (rendUploadInfo render))
      ]
   ++ [ ("Updated", renderUpdateInfo revisionNo utime uinfo)
      | (revisionNo, utime, uinfo) <- maybeToList (rendUpdateInfo render) ]
  where
    desc = rendOther render
    renderUploadInfo utime uinfo =
        formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" utime +++ " by " +++ user
      where
        uname   = maybe "Unknown" (display . userName) uinfo
        uactive = maybe False (isActiveAccount . userStatus) uinfo
        user | uactive   = anchor ! [href $ "/user/" ++ uname] << uname
             | otherwise = toHtml uname

    renderUpdateInfo revisionNo utime uinfo =
        renderUploadInfo utime uinfo +++ " to " +++
        anchor ! [href revisionsURL] << ("revision " +++ show revisionNo)
      where
        revisionsURL = display (rendPkgId render) </> "revisions/"

linkField :: String -> Html
linkField url = case url of
    [] -> noHtml
    _  -> anchor ! [href url] << url

sourceRepositoryField :: SourceRepo -> Html
sourceRepositoryField sr = sourceRepositoryToHtml sr

categoryField :: String -> Html
categoryField cat = anchor ! [href $ "/packages/#cat:" ++ cat] << cat

maintainField :: Maybe String -> Html
maintainField mnt = case mnt of
    Nothing -> strong ! [theclass "warning"] << toHtml "none"
    Just n  -> toHtml n

rendLicense :: PackageRender -> Html
rendLicense render = case rendLicenseFiles render of
  []            -> toHtml (rendLicenseName render)
  [licenseFile] -> anchor ! [ href (rendPkgUri render </> "src" </> licenseFile) ]
                          << rendLicenseName render
  _licenseFiles -> toHtml (rendLicenseName render)
                    +++ "["
                    +++ anchor ! [ href (rendPkgUri render </> "src") ]
                              << "multiple license files"
                    +++ "]"


sourceRepositoryToHtml :: SourceRepo -> Html
sourceRepositoryToHtml sr
    = toHtml (display (repoKind sr) ++ ": ")
  +++ case repoType sr of
      Just Darcs
       | (Just url, Nothing, Nothing) <-
         (repoLocation sr, repoModule sr, repoBranch sr) ->
          concatHtml [toHtml "darcs clone ",
                      anchor ! [href url] << toHtml url,
                      case repoTag sr of
                          Just tag' -> toHtml (" --tag " ++ tag')
                          Nothing   -> noHtml,
                      case repoSubdir sr of
                          Just sd -> toHtml " ("
                                 +++ (anchor ! [href (url </> sd)]
                                      << toHtml sd)
                                 +++ toHtml ")"
                          Nothing   -> noHtml]
      Just Git
       | (Just url, Nothing) <-
         (repoLocation sr, repoModule sr) ->
          concatHtml [toHtml "git clone ",
                      anchor ! [href url] << toHtml url,
                      case repoBranch sr of
                          Just branch -> toHtml (" -b " ++ branch)
                          Nothing     -> noHtml,
                      case repoTag sr of
                          Just tag' -> toHtml ("(tag " ++ tag' ++ ")")
                          Nothing   -> noHtml,
                      case repoSubdir sr of
                          Just sd -> toHtml ("(" ++ sd ++ ")")
                          Nothing -> noHtml]
      Just SVN
       | (Just url, Nothing, Nothing, Nothing) <-
         (repoLocation sr, repoModule sr, repoBranch sr, repoTag sr) ->
          concatHtml [toHtml "svn checkout ",
                      anchor ! [href url] << toHtml url,
                      case repoSubdir sr of
                          Just sd -> toHtml ("(" ++ sd ++ ")")
                          Nothing   -> noHtml]
      Just CVS
       | (Just url, Just m, Nothing, Nothing) <-
         (repoLocation sr, repoModule sr, repoBranch sr, repoTag sr) ->
          concatHtml [toHtml "cvs -d ",
                      anchor ! [href url] << toHtml url,
                      toHtml (" " ++ m),
                      case repoSubdir sr of
                          Just sd -> toHtml ("(" ++ sd ++ ")")
                          Nothing   -> noHtml]
      Just Mercurial
       | (Just url, Nothing) <-
         (repoLocation sr, repoModule sr) ->
          concatHtml [toHtml "hg clone ",
                      anchor ! [href url] << toHtml url,
                      case repoBranch sr of
                          Just branch -> toHtml (" -b " ++ branch)
                          Nothing     -> noHtml,
                      case repoTag sr of
                          Just tag' -> toHtml (" -u " ++ tag')
                          Nothing   -> noHtml,
                      case repoSubdir sr of
                          Just sd -> toHtml ("(" ++ sd ++ ")")
                          Nothing   -> noHtml]
      Just Bazaar
       | (Just url, Nothing, Nothing) <-
         (repoLocation sr, repoModule sr, repoBranch sr) ->
          concatHtml [toHtml "bzr branch ",
                      anchor ! [href url] << toHtml url,
                      case repoTag sr of
                          Just tag' -> toHtml (" -r " ++ tag')
                          Nothing -> noHtml,
                      case repoSubdir sr of
                          Just sd -> toHtml ("(" ++ sd ++ ")")
                          Nothing   -> noHtml]
      _ ->
          -- We don't know how to show this SourceRepo.
          -- This is a kludge so that we at least show all the info.
          toHtml (show sr)

commaList :: [Html] -> Html
commaList = concatHtml . intersperse (toHtml ", ")

vList :: [Html] -> Html
vList = concatHtml . intersperse br
-----------------------------------------------------------------------------

renderModuleForest :: URL -> ModuleForest -> Html
renderModuleForest docUrl forest =
    thediv ! [identifier "module-list"] << renderForest [] forest
    where
      renderForest _       [] = noHtml
      renderForest pathRev ts = myUnordList $ map renderTree ts
          where
            renderTree (Node s isModule hasDocs subs) =
                    ( if isModule then moduleEntry hasDocs newPath
                                  else italics << s )
                +++ renderForest newPathRev subs
                where
                  newPathRev = s:pathRev
                  newPath = reverse newPathRev

      moduleEntry False path =
          thespan ! [theclass "module"] << modName path
      moduleEntry True path =
          thespan ! [theclass "module"] << linkedName path
      modName path = mconcat (intersperse (itag "wbr") (map toHtml (mapTail ('.':) path)))
        where
          mapTail f (x:xs) = x : map f xs
          mapTail _ xs = xs
      linkedName path = anchor ! [href modUrl] << modName path
          where
            modUrl = docUrl ++ "/" ++ intercalate "-" path ++ ".html"
      myUnordList :: HTML a => [a] -> Html
      myUnordList = unordList ! [theclass "modules"]

------------------------------------------------------------------------------
-- TODO: most of these should be available from the CoreFeature
-- so pass it in to this module

-- | URL describing a package.
packageURL :: PackageIdentifier -> URL
packageURL pkgId = "/package" </> display pkgId

--cabalLogoURL :: URL
--cabalLogoURL = "/built-with-cabal.png"
