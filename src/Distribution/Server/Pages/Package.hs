-- Helpers for the body of the HTML page for a package
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PatternGuards, RecordWildCards, ViewPatterns #-}
module Distribution.Server.Pages.Package
  ( renderPackageFlags
  , renderDependencies
  , renderDetailedDependencies
  , renderVersion
  , moduleSection
  , rendLicense
  , maintainField
  , categoryField
  , renderHaddock
  , downloadSection
  , moduleToDocUrl
  ) where

import Distribution.Server.Features.PreferredVersions

import qualified Distribution.Server.Pages.Package.HaddockParse as Haddock
import Distribution.Server.Pages.Package.HaddockHtml
import Distribution.Server.Packages.ModuleForest
import Distribution.Server.Packages.Render
import Data.TarIndex (TarIndex)
import qualified Data.TarIndex as Tar

import qualified Distribution.ModuleName as Module
import Distribution.ModuleName (ModuleName)
import Distribution.Package
import Distribution.PackageDescription as P
import Distribution.Version
import Distribution.Text        (display)
import Distribution.Utils.ShortText (fromShortText, ShortText)
import Text.XHtml.Strict hiding (p, name, title, content)
import qualified Text.XHtml.Strict

import Data.Bool (bool)
import Data.Maybe               (fromMaybe, isJust, mapMaybe, catMaybes)
import Data.List                (intersperse, intercalate, partition)
import Control.Arrow            (Arrow (..))
import System.FilePath.Posix    ((</>), (<.>))

import qualified Documentation.Haddock.Markup as Haddock

instance HTML ShortText where
   toHtml = toHtml . fromShortText

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

-- | Render a table of the package's flags and along side it a tip
-- indicating how to enable/disable flags with Cabal.
renderPackageFlags :: PackageRender -> URL -> [Html]
renderPackageFlags render docURL =
      let (manualFlags, autoFlags) = partition flagManual $ rendFlags render
          manualSection = whenNotNull manualFlags
                          [h3 << "Manual Flags", flagsTable ["manual-flags"] manualFlags]
          autoSection = whenNotNull autoFlags
                        [collapsible "Automatic Flags" (flagsTable ["automatic-flags"] autoFlags)]
          combined = manualSection ++ autoSection
          flagsSection = whenNotNull combined
                         $ [h2 << "Flags"] ++ combined ++  [tip]
      in  flagsSection
  where
        tip =
          paragraph ! [theclass "tip"] <<
          [thespan << "Use "
          ,code "-f <flag>"
          ,thespan << " to enable a flag, or "
          ,code "-f -<flag>"
          ,thespan << " to disable that flag. "
          ,anchor ! [href tipLink] << "More info"
          ]
        tipLink = "https://cabal.readthedocs.io/en/latest/setup-commands.html#controlling-flag-assignments"
        flagsTable classes flags =
          table ! [theclass . unwords $ "flags-table" : classes] <<
          [thead << flagsHeadings
          ,tbody << map flagRow flags]
        collapsible summary item =
          tag "details" $ tag "summary" (toHtml summary) +++ item
        flagsHeadings = [th << "Name"
                        ,th << "Description"
                        ,th << "Default"]
        flagRow flag =
          tr << [td ! [theclass "flag-name"]   << code (unFlagName (flagName flag))
                ,td ! [theclass "flag-desc"]   << renderHaddock (moduleToDocUrl render docURL) (flagDescription flag)
                ,td ! [theclass (if flagDefault flag then "flag-enabled" else "flag-disabled")] <<
                 if flagDefault flag then "Enabled" else "Disabled"]
        code = (thespan ! [theclass "code"] <<)
        whenNotNull xs a = if null xs then [] else a

moduleSection :: PackageRender -> Maybe TarIndex -> URL -> Maybe PackageId -> Bool -> [Html]
moduleSection render mdocIndex docURL mPkgId quickNav = case renderedModules of
  [(LMainLibName, mods)] -> [msect mods]
  renderedLibs -> concatMap renderNamedLib renderedLibs

  where msect (ModSigIndex{ modIndex = m, sigIndex = s }) =
          let heading = bool h3 h2 containsSubLibraries in
          toHtml $
            (if not (null s)
                then [ heading << "Signatures"
                     , renderModuleForest docURL s ]
                else []) ++
            (if not (null m)
                then [ heading << "Modules"] ++
                     [renderDocIndexLink] ++
                     [renderModuleForest docURL m ]
                else [])
        renderDocIndexLink = case concatLinks indexLinks of
            Nothing -> mempty
            Just links -> paragraph ! [thestyle "font-size: small"] << ("[" +++ links +++ "]")
          where
            indexLinks = catMaybes $ case mdocIndex of
                Just tindex ->
                  let docIndexURL | isJust (Tar.lookup tindex "doc-index-All.html") = docURL </> "doc-index-All.html"
                                  | otherwise = docURL </> "doc-index.html"
                  in  [ Just $ anchor ! [href docIndexURL] << "Index"
                      , if quickNav
                            then Just $ anchor ! [identifier "quickjump-trigger", href "#"] << "Quick Jump"
                            else Nothing
                      ]
                Nothing -> []
              ++ [fmap (\pkgId -> anchor ! [href (packageURL pkgId)] << "Last Documentation") mPkgId]

            concatLinks []     = Nothing
            concatLinks [h]    = Just h
            concatLinks (h:hs) = (h +++) . ("] [" +++) <$> concatLinks hs

        renderNamedLib :: (LibraryName, ModSigIndex) -> [Html]
        renderNamedLib (name, mods) =
          [ h2 << ("library " ++ rendLibName render name)
          , thediv ! [theclass "lib-contents"] << msect mods
          ]

        containsSubLibraries :: Bool
        containsSubLibraries = map fst renderedModules == [LMainLibName]

        renderedModules :: [(LibraryName, ModSigIndex)]
        renderedModules = rendModules render mdocIndex

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
renderDetailedDependencies pkgRender
  = mconcat (mapMaybe renderComponentType componentsByType)

  where
    componentsByType :: [(String, [(ComponentName, DependencyTree)])]
    componentsByType =
      [ ("Libraries", first CLibName <$> rendLibraryDeps pkgRender)
      , ("Executables", rendExecutableDeps pkgRender)
      ]

    renderComponentType :: (String, [(ComponentName, DependencyTree)]) -> Maybe Html
    renderComponentType (_, []) = Nothing
    renderComponentType (componentType, items) = Just $ mconcat
      [ h2 << componentType
      , flip foldMap items $ \(componentName, deptree) ->
        h3 << rendComponentName pkgRender componentName +++ fromMaybe noDeps (render deptree)
      ]

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
    list items = unordList items ! [identifier "detailed-dependencies"]

    renderComponent :: CondBranch ConfVar [Dependency] IsBuildable
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
            displayConfVar (PackageFlag fn) = "flag(" ++ unFlagName fn ++ ")"
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

commaList :: [Html] -> Html
commaList = concatHtml . intersperse (toHtml ", ")

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
      modName path = toHtml (intercalate "." path)
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
