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

packagePageTemplate :: PackageRender -> [Html] -> [Html] -> [(String, Html)]
            -> [(String, Html)] -> Maybe TarIndex -> Maybe BS.ByteString
            -> URL -> Bool
            -> [TemplateAttr]
packagePageTemplate render headLinks deprHtml sections
            bottom mdocIndex mreadMe
            docURL isCandidate =
  [ "docTitle"        $= pkgName ++ case synopsis (rendOther render) of
      ""    -> ""
      short -> ": " ++ short
  , "pkgName"         $= pkgName
  , "headLinks"       $= case headLinks of
      []    -> []
      items -> (map (\item -> "[" +++ item +++ "] ") items)
  , "deprecatedMsg"   $= deprHtml
  , "pkgDescription"  $= descriptionSection render
  , "skipToReadme"    $= skipToReadme render
  ]
  ++
  [ "moduleList"      $= Old.moduleSection render mdocIndex docURL
  , "dependencyList"  $= snd (Old.renderDependencies render)
  ]
  ++
  [ "readme"          $= Old.readmeSection render mreadMe
  , "license"         $= Old.rendLicense render
  , "copyright"       $= renderCopyright
  , "author"          $= (toHtml $ author desc)
  , "maintainer"      $= (Old.maintainField $ rendMaintainer render)
  , "stability"       $= determineStability desc
  , "categories"      $= (commaList . map Old.categoryField $ rendCategory render)
  , "homePage"        $= (Old.linkField $ homepage desc)
  , "bugTracker"      $= (Old.linkField $ bugReports desc)
  , "sourceRepo"      $= (vList $ map sourceRepositoryToHtml (sourceRepos desc))
  , "executables"     $= (commaList . map toHtml $ rendExecNames render)
  , "uploadTime"      $= (uncurry renderUploadInfo $ rendUploadInfo render)
  ]
  ++ mapTuples
    [ ("updated", renderUpdateInfo revisionNo utime uinfo)
      | (revisionNo, utime, uinfo) <- maybeToList (rendUpdateInfo render) ]

  ++ mapTuples sections
  ++ [ "cabalVersion"    $= display cabalVersion
    ]
    where
      pkgid   = rendPkgId render
      pkgVer  = display $ pkgVersion pkgid
      pkgName = display $ packageName pkgid

      desc = rendOther render

      renderCopyright :: Html
      renderCopyright = toHtml $ case text of
        "" -> "None Provided"
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

descriptionSection :: PackageRender -> [Html]
descriptionSection PackageRender{..} =
        Old.renderHaddock (description rendOther)

skipToReadme :: PackageRender -> [Html]
skipToReadme r = case rendReadme r of
  Just _ -> [ hr
            , toHtml "["
            , anchor ! [href "#readme"] << "Skip to ReadMe"
            , toHtml "]"
            ]
  _      -> []

mapTuples :: [(String, Html)] -> [TemplateAttr]
mapTuples = map (\(a,b) -> a $= b)

determineStability :: PackageDescription -> Html
determineStability desc
  | actualStability == "" = toHtml "Unknown"
  | otherwise             = toHtml actualStability
  where actualStability = stability desc

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

renderVersion :: PackageId -> [(Version, VersionStatus)] -> Maybe String -> (String, Html)
renderVersion (PackageIdentifier pname pversion) allVersions info =
  ( if null earlierVersions && null laterVersions then "Version" else "Versions"
  , unordList versionList +++ infoHtml
  )
  where
    (earlierVersions, laterVersionsInc) = span ((<pversion) . fst) allVersions

    (mThisVersion, laterVersions) = case laterVersionsInc of
        (v:later) | fst v == pversion -> (Just v, later)
        later -> (Nothing, later)

    vmax = 2
    versionList =
      if length olderVersions <= vmax then olderVersions
        else [versionedLink' (head earlierVersions) "..."] ++ reverse (take vmax (reverse olderVersions))
      ++ currentVersion ++
      if length newerVersions <= vmax then newerVersions
      else take vmax newerVersions ++ [versionedLink' (last laterVersions) "..."]
      where
        olderVersions = map versionedLink earlierVersions
        currentVersion = case pversion of
          Version [] [] -> []
          _ -> [strong ! (maybe [] (status . snd) mThisVersion) << display pversion]
        newerVersions = map versionedLink laterVersions

    versionedLink (v, s) = anchor ! (status s ++ [href $ packageURL $ PackageIdentifier pname v]) << display v
    versionedLink' (v, s) str  = anchor ! (status s ++ [href $ packageURL $ PackageIdentifier pname v]) << str

    status st = case st of
        NormalVersion -> []
        DeprecatedVersion  -> [theclass "deprecated"]
        UnpreferredVersion -> [theclass "unpreferred"]

    infoHtml = case info of
      Nothing -> noHtml;
      Just str -> " (" +++ (anchor ! [href str] << "info") +++ ")"



-----------------------------------------------------------------------------
commaList :: [Html] -> Html
commaList = concatHtml . intersperse (toHtml ", ")

vList :: [Html] -> Html
vList = concatHtml . intersperse br

-- | URL describing a package.
packageURL :: PackageIdentifier -> URL
packageURL pkgId = "/package" </> display pkgId
