-- Body of the HTML page for a package
module Distribution.Server.Pages.Package (
    packagePage
  ) where

import Distribution.Server.Features.Packages

import Distribution.Server.Pages.Package.HaddockParse (parseHaddockParagraphs)
import Distribution.Server.Pages.Package.HaddockLex  (tokenise)
import Distribution.Server.Pages.Package.HaddockHtml
import Distribution.Server.Packages.ModuleForest
import Distribution.Server.Pages.Template (hackagePage)
import Distribution.Server.Distributions.Distributions (DistroPackageInfo(..), DistroName)

import Distribution.Package
import Distribution.PackageDescription as P
import Distribution.Version (Version (..), VersionRange (..))
import Distribution.Text        (display)
import Text.XHtml.Strict hiding (p, name)

import Data.Char                (toUpper)
import Data.List                (intersperse, intercalate)
import System.FilePath.Posix    ((</>), (<.>))
import System.Locale            (defaultTimeLocale)
import Data.Time.Format         (formatTime)

packagePage :: PackageRender -> [(DistroName, DistroPackageInfo)] -> Maybe URL -> Html
packagePage render distributions docURL = hackagePage (display $ rendPkgId render) (pkgBody render distributions docURL)

-- | Body of the package page
pkgBody :: PackageRender -> [(DistroName, DistroPackageInfo)] -> Maybe URL -> [Html]
pkgBody render distributions docURL =
    (thediv ! [theclass "floatright"] << cabalLink) :
    (h2 << docTitle) :
    prologue (description desc) ++
    propertySection render distributions ++
    moduleSection render docURL ++
    downloadSection render ++
    reportsSection pkgid ++
    adminSection pkgid
  where
    desc  = rendOther render
    pkgid = rendPkgId render
    docTitle = display (packageName pkgid) ++ case synopsis desc of
        "" -> ""
        short  -> ": " ++ short
    cabalLink = anchor ! [href cabalHomeURL] <<
        (image ! [alt "Built with Cabal", src cabalLogoURL])

prologue :: String -> [Html]
prologue [] = []
prologue desc = case parseHaddockParagraphs (tokenise desc) of
    Left _ -> [paragraph << p | p <- paragraphs desc]
    Right doc -> [markup htmlMarkup doc]

-- Break text into paragraphs (separated by blank lines)
paragraphs :: String -> [String]
paragraphs = map unlines . paras . lines
  where paras xs = case dropWhile null xs of
                [] -> []
                xs' -> case break null xs' of
                        (para, xs'') -> para : paras xs''

downloadSection :: PackageRender -> [Html]
downloadSection render = [
        h3 << "Downloads",
        ulist << [li << i | i <- download_items]]
  where download_items =
            [if tarExists then [anchor ! [href downloadURL] << (display pkgId ++ ".tar.gz"),
                                toHtml << " (Cabal source package)"]
                          else [toHtml << "Package tarball not uploaded"],
             [anchor ! [href cabalURL] << "Package description",
             toHtml $ if tarExists then " (included in the package)" else ""]]
        downloadURL = packageFile pkgId
        cabalURL = cabalFile pkgId
        tarExists = rendHasTarball render
        pkgId = rendPkgId render

moduleSection :: PackageRender -> Maybe URL -> [Html]
moduleSection render docURL = maybe [] msect (rendModules render)
  where msect lib = [h3 << "Modules", renderModuleForest docURL lib]

reportsSection :: PackageId -> [Html]
reportsSection pkgid =
  [ h3 << "Reports"
  , ulist << [li << buildReports ] ]
  where
    buildReports = [anchor ! [href (packageURL pkgid </> "buildreports")] << "Build reports"]

adminSection :: PackageId -> [Html]
adminSection pkgid =
  [ h3 << "Administration"
  , ulist << [li << admin]
  ]
  where
    admin = [anchor ! [href (packageNameURL pkgid </> "admin")] << "Administration"]


propertySection :: PackageRender -> [(DistroName, DistroPackageInfo)] -> [Html]
propertySection render distributions = [
        -- h3 << "Package properties",
        tabulate $
                [(if null earlierVersions && null laterVersions then "Version" else "Versions",
                 commaList (map versionedLink earlierVersions ++
                            (bold << display pversion) :
                            map versionedLink laterVersions))] ++
                [("Dependencies", htmlDepsList)] ++
                [(fname, fvalue) |
                        (fname, fvalue) <- showFields render,
                        not (isNoHtml fvalue)] ++
                [(capitalize fname, dispTagVal fname fvalue) | (fname, fvalue) <- []] ++ -- for additional (String, String) tags
                distHtml
        ]
  where allVersions = rendAllVersions render
        earlierVersions = takeWhile  (< pversion) allVersions
        laterVersions   = dropWhile (<= pversion) allVersions
        versionedLink v = anchor ! [href (packageURL (PackageIdentifier pname v))] << display v
        PackageIdentifier pname pversion = rendPkgId render
        htmlDepsList = foldr (+++) noHtml $
                intersperse (toHtml " " +++ bold (toHtml "or") +++ br) $
                map showDependencies (rendDepends render)
        distHtml = case distributions of
            [] -> []
            _  -> [("Distributions:", commaList $ map showDist distributions )]
        {-(if null successes then []
         else [("Built on", commaList (map toHtml successes))]) ++
        (if null failures then []
         else [("Build failure", commaList (map showLog failures))])
        successes = Map.keys (pdBuilds pd)
        failures = Map.toList (Map.difference (pdBuildFailures pd) (pdBuilds pd))
        showLog (desc, url) =
                toHtml (desc ++ " (") +++
                (anchor ! [href url] << "log") +++
                toHtml ")"-}
        capitalize (c:cs) = toUpper c : cs
        capitalize "" = ""
        dispTagVal "superseded by" v = anchor ! [href (packageURL (PackageIdentifier (PackageName v) (Version [] [])))] << v
        dispTagVal _ v = toHtml v
        showDist (name, info)
            = let version = distroVersion info
                  url = distroUrl info
              in toHtml (display name ++ ":") +++
                 anchor ! [href url] << toHtml (display version)

tabulate :: [(String, Html)] -> Html
-- tabulate items = dlist << concat [[dterm << t, ddef << d] | (t, d) <- items]
tabulate items = table ! [theclass "properties"] <<
        [tr ! [theclass (if odd n then "odd" else "even")] <<
                [th ! [theclass "horizontal"] << t, td << d] |
                (n, (t, d)) <- zip [(1::Int)..] items]

showDependencies :: [Dependency] -> Html
showDependencies deps = commaList (map showDependency deps)

showDependency ::  Dependency -> Html
showDependency (Dependency (PackageName pname) vs) = showPkg +++ showVersion vs
  where showVersion AnyVersion = noHtml
        showVersion vs' = toHtml (" (" ++ display vs' ++ ")")
        -- mb_vers links to latest version in range. This is a bit computationally
        -- expensive, not cache-friendly, and perhaps unexpected in some cases
        {-mb_vers = maybeLast $ filter (`withinRange` vs) $ map packageVersion $
                    PackageIndex.lookupPackageName vmap (PackageName pname)-}
        -- nonetheless, we should ensure that the package exists /before/
        -- passing along the PackageRender, which is not the case here
        showPkg = anchor ! [href . packageURL $ PackageIdentifier (PackageName pname) (Version [] [])] << pname

showFields :: PackageRender -> [(String, Html)]
showFields render = [
        -- Cabal-Version
        ("License",     toHtml $ rendLicenseName render),
        ("Copyright",   toHtml $ P.copyright desc),
        ("Author",      toHtml $ author desc),
        ("Maintainer",  maintainField $ rendMaintainer render),
        ("Stability",   toHtml $ stability desc),
        ("Category",    commaList . map toHtml $ rendCategory render), 
        ("Home page",   linkField $ homepage desc),
        ("Bug tracker", linkField $ bugReports desc),
        ("Executables", commaList . map toHtml $ rendExecNames render),
        ("Upload date", toHtml $ showTime utime),
        ("Uploaded by", toHtml $ uname)
      ]
  where desc = rendOther render
        (utime, uname) = rendUploadInfo render
        linkField url = case url of
            [] -> noHtml
            _  -> anchor ! [href url] << url
        maintainField mnt = case mnt of
            Nothing -> strong ! [theclass "warning"] << toHtml "none"
            Just n  -> toHtml n
        showTime = formatTime defaultTimeLocale "%c"

commaList :: [Html] -> Html
commaList = concatHtml . intersperse (toHtml ", ")
-----------------------------------------------------------------------------

renderModuleForest :: Maybe URL -> ModuleForest -> Html
renderModuleForest mb_url = renderForest []
    where
      renderForest _       [] = noHtml
      renderForest pathRev ts = myUnordList $ map renderTree ts
          where
            renderTree (Node s isModule subs) =
                    ( if isModule then moduleEntry newPath else italics << s )
                +++ renderForest newPathRev subs
                where
                  newPathRev = s:pathRev
                  newPath = reverse newPathRev

      moduleEntry = maybe modName linkedName mb_url
      modName path = toHtml (intercalate "." path)
      linkedName url path = anchor ! [href modUrl] << modName path
          where
            modUrl = url ++ "/" ++ intercalate "-" path ++ ".html"
      myUnordList :: HTML a => [a] -> Html
      myUnordList = unordList ! [theclass "modules"]

------------------------------------------------------------------------------

-- | URL describing a package.
packageURL :: PackageIdentifier -> URL
packageURL pkgId = "/package" </> display pkgId

packageNameURL :: PackageIdentifier -> URL
packageNameURL pkgId = "/package" </> display (pkgName pkgId)

-- | The name of the package file for a given package identifier
packageFile :: PackageIdentifier -> URL
packageFile pkgId = "/package" </> display pkgId </> display pkgId <.> "tar.gz"

-- | The name of the Cabal file for a given package identifier
cabalFile :: PackageIdentifier -> URL
cabalFile pkgId = packageURL pkgId </> display (pkgName pkgId) <.> "cabal"

cabalLogoURL :: URL
cabalLogoURL = "/built-with-cabal.png"

-- global URLs
cabalHomeURL :: URL
cabalHomeURL = "http://haskell.org/cabal/"
