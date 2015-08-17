{-# LANGUAGE PatternGuards, RecordWildCards #-}
module Distribution.Server.Pages.PackageFromTemplate
  ( packagePageTemplate
  , renderVersion
  , latestVersion
  ) where

import Distribution.Server.Framework.Templating
import Distribution.Server.Features.PreferredVersions

import Distribution.Server.Pages.Template (hackagePageWith)
import Distribution.Server.Pages.Package.HaddockParse (parseHaddockParagraphs)
import Distribution.Server.Pages.Package.HaddockLex  (tokenise)
import Distribution.Server.Pages.Package.HaddockHtml
import Distribution.Server.Packages.ModuleForest
import Distribution.Server.Packages.Render
import Distribution.Server.Users.Types (userStatus, userName, isActiveAccount)
import Data.TarIndex (TarIndex)
import Distribution.Server.Features.Distro.Types

import Distribution.Package
import Distribution.PackageDescription as P
import Distribution.Simple.Utils ( cabalVersion )
import Distribution.Version
import Distribution.Text        (display)
import Text.XHtml.Strict hiding (p, name, title, content)

import Data.Monoid              (Monoid(..))
import Data.Maybe               (fromMaybe, maybeToList, isJust, mapMaybe)
import Data.List                (intersperse, intercalate)
import Control.Arrow            (second)
import System.FilePath.Posix    ((</>), (<.>), takeFileName)
import Data.Time.Locale.Compat  (defaultTimeLocale)
import Data.Time.Format         (formatTime)
import System.FilePath.Posix    (takeExtension)

import qualified Cheapskate      as Markdown (markdown, Options(..))
import qualified Cheapskate.Html as Markdown (renderDoc)

import qualified Text.Blaze.Html.Renderer.Pretty as Blaze (renderHtml)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString.Lazy as BS (ByteString, toStrict)

import qualified Distribution.Server.Pages.Package as Old
import Data.Time.Clock (UTCTime)
import Distribution.Server.Users.Types (UserInfo)
import Distribution.Server.Util.ServeTarball (loadTarEntry)
import Control.Monad.Trans (liftIO)

packagePageTemplate :: PackageRender
            -> Maybe TarIndex -> Maybe BS.ByteString
            -> URL -> [(DistroName, DistroPackageInfo)]
            -> [TemplateAttr]
packagePageTemplate render
            mdocIndex mreadme
            docURL distributions =
  [ "pkgName"         $= pkgName
  , "docTitle"        $= pkgName ++ case synopsis (rendOther render) of
      ""    -> ""
      short -> ": " ++ short
  , "pkgDescription"  $= Old.renderHaddock (description $ rendOther render)
  ]
  ++
  [ "moduleList"      $= Old.moduleSection render mdocIndex docURL
  , "dependencyList"  $= snd (Old.renderDependencies render)
  ]
  ++
  [ "package"         $= optionalPackageInfoTemplate ]
  ++
  [ "license"         $= Old.rendLicense render
  , "author"          $= (toHtml $ author desc)
  , "maintainer"      $= (Old.maintainField $ rendMaintainer render)
  , "categories"      $= (commaList . map Old.categoryField $ rendCategory render)
  , "sourceRepo"      $= (vList $ map sourceRepositoryToHtml (sourceRepos desc))
  , "executables"     $= (commaList . map toHtml $ rendExecNames render)
  , "uploadTime"      $= (uncurry renderUploadInfo $ rendUploadInfo render)
  , "downloadSection" $= Old.downloadSection render
  ]
  ++
  [ "cabalVersion"    $= display cabalVersion
  ]
  where
    optionalPackageInfoTemplate = templateDict $
      [ templateVal "hasReadme"
          (if rendReadme render == Nothing then False else True)
      , templateVal "readme"
          (readmeSection render mreadme)
      ] ++
      [ templateVal "hasHomePage"
          (if (homepage desc  == []) then False else True)
      , templateVal "homePageUrl"
          (homepage desc)
      ] ++
      [ templateVal "hasBugTracker"
          (if bugReports desc == [] then False else True)
      , templateVal "bugTracker"
          (bugReports desc)
      ] ++
      [ templateVal "hasDistributions"
          (if distributions == [] then False else True)
      , templateVal "distributions"
          (concatHtml . intersperse (toHtml ", ") $ map showDist distributions)
      ] ++
      [ templateVal "hasUpdateTime"
          (case rendUpdateInfo render of Nothing -> False; _ -> True)
      , templateVal "updateTime" [ renderUpdateInfo revisionNo utime uinfo
          | (revisionNo, utime, uinfo) <- maybeToList (rendUpdateInfo render) ]
      ] ++
      [ templateVal "hasChangelog"
          (if rendChangeLog render == Nothing then False else True)
      , templateVal "changelog"
          (renderChangelog render)
      ] ++
      [ templateVal "hasCopyright"
          (if P.copyright desc == "" then False else True)
      , templateVal "copyright"
          renderCopyright
      ] ++
      [ templateVal "hasStability"
          (if stability desc == "" then False else True)
      , templateVal "stability"
          (renderStability desc)
      ] ++
      [ templateVal "hasFlags"
          (if rendFlags render == [] then False else True)
      , templateVal "flags"
          (Old.renderPackageFlags render)
      ]
      where
        showDist (dname, info) = toHtml (display dname ++ ":") +++
            anchor ! [href $ distroUrl info] << toHtml (display $ distroVersion info)

    pkgid   = rendPkgId render
    pkgVer  = display $ pkgVersion pkgid
    pkgName = display $ packageName pkgid

    desc = rendOther render

    renderCopyright :: Html
    renderCopyright = toHtml $ case text of
      "" -> "None provided"
      _ -> text
      where text = P.copyright desc

    renderUpdateInfo :: Int -> UTCTime -> Maybe UserInfo -> Html
    renderUpdateInfo revisionNo utime uinfo =
        renderUploadInfo utime uinfo +++ " to " +++
        anchor ! [href revisionsURL] << ("revision " +++ show revisionNo)
      where
        revisionsURL = display (rendPkgId render) </> "revisions/"

    renderUploadInfo :: UTCTime -> Maybe UserInfo-> Html
    renderUploadInfo utime uinfo =
        formatTime defaultTimeLocale "%c" utime +++ " by " +++ user
      where
        uname   = maybe "Unknown" (display . userName) uinfo
        uactive = maybe False (isActiveAccount . userStatus) uinfo
        user  | uactive   = anchor ! [href $ "/user/" ++ uname] << uname
              | otherwise = toHtml uname

    renderChangelog :: PackageRender -> Html
    renderChangelog render = case rendChangeLog render of
      Nothing            -> toHtml "None available"
      Just (_,_,_,fname) -> anchor ! [href (rendPkgUri render </> "changelog")] << takeFileName fname

    renderStability :: PackageDescription -> Html
    renderStability desc = case actualStability of
      "" -> toHtml "Unknown"
      _  -> toHtml actualStability
      where actualStability = stability desc

-- #ToDo: Pick out several interesting versions to display, with a link to
-- display all versions.
renderVersion :: PackageId -> [(Version, VersionStatus)] -> Maybe String -> Html
renderVersion (PackageIdentifier pname pversion) allVersions info =
  versionList +++ infoHtml
  where
    (earlierVersions, laterVersionsInc) = span ((<pversion) . fst) allVersions

    (mThisVersion, laterVersions) = case laterVersionsInc of
            (v:later) | fst v == pversion -> (Just v, later)
            later -> (Nothing, later)

    versionList = commaList $ map versionedLink earlierVersions
      ++ (case pversion of
            Version [] [] -> []
            _ -> [strong ! (maybe [] (status . snd) mThisVersion) << display pversion]
        )
      ++ map versionedLink laterVersions

    versionedLink (v, s) = anchor !
      (status s ++ [href $ packageURL $ PackageIdentifier pname v]) <<
        display v

    status st = case st of
        NormalVersion -> []
        DeprecatedVersion  -> [theclass "deprecated"]
        UnpreferredVersion -> [theclass "unpreferred"]

    infoHtml = case info of
      Nothing -> noHtml
      Just str -> " (" +++ (anchor ! [href str] << "info") +++ ")"

sourceRepositoryToHtml :: SourceRepo -> Html
sourceRepositoryToHtml sr
    = toHtml (display (repoKind sr) ++ ": ")
  +++ case repoType sr of
      Just Darcs
       | (Just url, Nothing, Nothing) <-
         (repoLocation sr, repoModule sr, repoBranch sr) ->
          concatHtml [toHtml "darcs get ",
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

-- | Handle how version links are displayed.

latestVersion :: PackageId -> [Version] -> Html
latestVersion (PackageIdentifier pname pversion) allVersions =
  versionLink (last allVersions)
  where
    versionLink v = anchor ! [href $ packageURL $ PackageIdentifier pname v] << display v

readmeSection :: PackageRender -> Maybe BS.ByteString -> [Html]
readmeSection PackageRender { rendReadme = Just (_, _etag, _, filename)
                            , rendPkgId  = pkgid }
              (Just content) =
    [ h2 ! [identifier "readme"] << ("Readme for " ++ display pkgid)
    , thediv ! [theclass "embedded-author-content"]
            << if supposedToBeMarkdown filename
                 then renderMarkdown content
                 else pre << unpackUtf8 content
    ]
readmeSection _ _ = []

renderMarkdown :: BS.ByteString -> Html
renderMarkdown = primHtml . Blaze.renderHtml
               . Markdown.renderDoc . Markdown.markdown opts
               . T.decodeUtf8With T.lenientDecode . BS.toStrict
  where
    opts =
      Markdown.Options
        { Markdown.sanitize           = True
        , Markdown.allowRawHtml       = False
        , Markdown.preserveHardBreaks = False
        , Markdown.debug              = False
        }

supposedToBeMarkdown :: FilePath -> Bool
supposedToBeMarkdown fname = takeExtension fname `elem` [".md", ".markdown"]

unpackUtf8 :: BS.ByteString -> String
unpackUtf8 = T.unpack
           . T.decodeUtf8With T.lenientDecode
           . BS.toStrict
-----------------------------------------------------------------------------
commaList :: [Html] -> Html
commaList = concatHtml . intersperse (toHtml ", ")

vList :: [Html] -> Html
vList = concatHtml . intersperse br

-- | URL describing a package.
packageURL :: PackageIdentifier -> URL
packageURL pkgId = "/package" </> display pkgId
