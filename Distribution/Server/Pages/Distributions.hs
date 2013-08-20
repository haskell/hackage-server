
module Distribution.Server.Pages.Distributions
    ( homePage
    , adminHomePage
    , distroListing
    , distroPage
    , adminDistroPage
    )
    where

import Distribution.Server.Pages.Template (hackagePage)
import Distribution.Server.Features.Distro.Distributions
import Distribution.Server.Users.Types
import Distribution.Text

import Distribution.Package
import qualified Happstack.Server.SURI as SURI

import System.FilePath.Posix
import Text.XHtml.Strict

-- | List of known distributions
homePage :: [DistroName] -> Html
homePage = hackagePage "Distributions" . listing "/distro"

-- | List of known distributions. Includes a form
-- to add a new distribution.
adminHomePage :: [DistroName] -> Html
adminHomePage distros
    = hackagePage "Distributions" $ concat
      [ listing "/admin/distro" distros
      , addDistroForm
      ]

-- | Display the packages in a distribution. The passed-in URL is the
-- link to the admin page.
distroListing :: DistroName -> [(PackageName, DistroPackageInfo)] -> URL -> Html
distroListing distro packages adminLink
    = hackagePage (display distro) $ concat
      [ packageListing
      , adminLinkHtml
      ]

 where
   packageListing :: [Html]
   packageListing
       = [ h3 << ("Packages in " ++ display distro)
         , ulist << map (uncurry packageHtml) packages
         ]

   packageHtml pName pInfo
       = li << (display pName ++ " " ++ display (distroVersion pInfo))

   adminLinkHtml
       = [ h3 << "Admin Tasks"
         , anchor ! [href adminLink] << "Administrative tasks"
         ]

-- | Admin page for a distribution. Includes a list
-- of the maintainers and a form to add maintainers.
distroPage :: DistroName -> [UserName] -> Html
distroPage distro users
    = hackagePage (display distro) $ concat
      [ addPackageForm distro
      , userList distro users
      , addUserForm distro
      ]

addPackageForm :: DistroName -> [Html]
addPackageForm distro  =
    let actionUri =
            "/distro" </> SURI.escape distro </> "admin" </> "addPackage"
    in [ h3 << "Add a package"
       , gui actionUri ! [theclass "box"] <<
         [ p << [stringToHtml "Package: ", textfield "packageName"]
         , p << [stringToHtml "Version: ", textfield "version"]
         , p << [stringToHtml "URL: ", textfield "uri"]
         , submit "submit" "Add package"
         ]
       ]

addDistroForm :: [Html]
addDistroForm =
    [ h3 << "Add Distribution"
    , gui "/admin/createDistro" ! [theclass "box"] <<
        [ p << [stringToHtml "Name: ", textfield "distroName"]
        , submit "submit" "Add distribution"
        ]
    ]

{-
This should be updated to match the current URI scheme in the Distro feature.

displayDir :: Text a => a -> String
displayDir = escapeString f . display
 where f c = okInPath c && c /= '/'

-- | Admin form for a distribution. Includes a list
-- of the maintainers, a form to add maintainers and
-- a button to destroy this distribution.
adminDistroPage :: DistroName -> [UserName] -> Html
adminDistroPage distro users
    = hackagePage (display distro) $ concat
      [ userList distro users
      , addUserForm distro
      , deleteDistro distro
      ]

addUserForm :: DistroName -> [Html]
addUserForm distro =
    [ h3 << "Add a maintainer"
    , gui ("/distro" </> displayDir distro </> "admin" </> "addMember") ! [theclass "box"]
      << [ p << [stringToHtml "User: ", textfield "userName"]
         , submit "submit" "Add user"
         ]
    ]

deleteDistro :: DistroName -> [Html]
deleteDistro distro
    = [ h3 << "Delete distribution"
      , gui ("/admin/distro" </> displayDir distro </> "delete") <<
        submit "submit" "Delete Distribution"
      ]

userList :: DistroName -> [UserName] -> [Html]
userList distro users
    = [ h3 << "Maintainers"
      , ulist << map (userHtml distro) users
      ]

userHtml :: DistroName -> UserName -> Html
userHtml distro user
    = li << [ stringToHtml $ display user
            , removeUser user distro
            ]

removeUser :: UserName -> DistroName -> Html
removeUser user distro
    = gui ("/distro" </> displayDir distro </> "admin" </> "removeMember")
      << [ hidden "userName" $ display user
         , submit "submit" "Remove"
         ]-}

listing :: FilePath -> [DistroName] -> [Html]
listing rootPath distros
    = [ h3 << "Distributions"
      , ulist << map (distroHtml rootPath) distros
      ]

distroHtml :: FilePath -> DistroName -> Html
distroHtml rootPath distro
    = li << anchor ! [href $ rootPath </> SURI.escape distro ]
      << display distro
