-- Body of the HTML page for a package
module Distribution.Server.Pages.PackageAdmin (
    packageAdminPage
  ) where

import Text.XHtml.Strict
import System.FilePath.Posix ((</>))


import Distribution.Server.Pages.Package.HaddockParse	( parseHaddockParagraphs )
import Distribution.Server.Pages.Package.HaddockHtml
import Distribution.Server.Pages.Package.HaddockLex	( tokenise )
import Distribution.Server.Pages.Template		( hackagePage )
import Distribution.Server.Packages.Types
import qualified Distribution.Server.Users.Types as Users

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Text

packageAdminPage :: [Users.UserName] -> PkgInfo -> Html
packageAdminPage users pkg =
    hackagePage (display $ packageName pkg) (adminBody users pkg)

-- | Body of the page
adminBody :: [Users.UserName] -> PkgInfo -> [Html]
adminBody users pkgInfo = concat
        [ return (h2 << docTitle)
        , prologue pkg
        , listMaintainers users
        , forms pkgInfo
        ]
  where	pkg = packageDescription (pkgDesc pkgInfo)
	short = synopsis pkg
	pname = display (packageName pkgInfo)
	docTitle
	  | null short = pname
	  | otherwise = pname ++ ": " ++ short

prologue :: PackageDescription -> [Html]
prologue pkg
  | null desc = []
  | otherwise = html_desc
  where desc = description pkg
	html_desc = case parseHaddockParagraphs (tokenise desc) of
	              Left _ -> [paragraph << p | p <- paragraphs desc]
	              Right doc -> [markup htmlMarkup doc]


forms :: PkgInfo -> [Html]
forms pkg = concat
    [ addMaintainer pkg
    , removeMaintainer pkg
    ]

addMaintainer :: PkgInfo -> [Html]
addMaintainer pkg =
    [ h3 << "Add Maintainer"
    , gui (adminAction pkg "addMaintainer") ! [theclass "box"] <<
        [ p << [stringToHtml "User: ", textfield "user"]
        , submit "submit" "Add maintainer"
        ]
    ]

removeMaintainer :: PkgInfo -> [Html]
removeMaintainer pkg =
    [ h3 << "Remove Maintainer"
    , gui (adminAction pkg "removeMaintainer") ! [theclass "box"] <<
       [ p << [stringToHtml "User: ", textfield "user"]
       , submit "submit" "Remove maintainer"
       ]
    ]

listMaintainers :: [Users.UserName] -> [Html]
listMaintainers [] = []
listMaintainers users =
    [ h3 << "Package Maintainers"
    , p << unordList (map display users)
    ]

adminAction :: PkgInfo -> String -> String
adminAction pkg act =packageNameURL (packageId pkg) </> "admin" </> act

-- Break text into paragraphs (separated by blank lines)
paragraphs :: String -> [String]
paragraphs = map unlines . paras . lines
  where paras xs = case dropWhile null xs of
		[] -> []
		xs' -> case break null xs' of
			(para, xs'') -> para : paras xs''
{-
commaList :: [Html] -> Html
commaList = concatHtml . intersperse (toHtml ", ")

-- Same as @sortBy (comparing f)@, but without recomputing @f@.
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f xs = map snd (sortBy (comparing fst) [(f x, x) | x <- xs])
-}


-- UTILS:
{-
maybeLast :: [a] -> Maybe a
maybeLast = listToMaybe . reverse
-}

packageNameURL :: PackageIdentifier -> URL
packageNameURL pkgId = "/package" </> display (pkgName pkgId)

