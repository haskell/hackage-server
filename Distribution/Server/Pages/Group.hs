-- Body of the HTML page for a package
module Distribution.Server.Pages.Group (
    groupPage
  ) where

import Text.XHtml.Strict
import System.FilePath.Posix ((</>))

import Distribution.Server.Pages.Template (hackagePage)
import qualified Distribution.Server.Users.Types as Users
import Distribution.Server.Users.Group (GroupDescription(..))
import Distribution.Text
import Data.Maybe

-- Primitive access control: the URI to post a new user request to, or the the URI/<username> to DELETE
groupPage :: [Users.UserName] -> Maybe String -> Maybe String -> GroupDescription -> Html
groupPage users addUri removeUri desc = hackagePage (groupTitle desc) (groupBody users addUri removeUri desc)

-- | Body of the page
groupBody :: [Users.UserName] -> Maybe String -> Maybe String -> GroupDescription -> [Html]
groupBody users addUri removeUri desc = concat
        [ return (h2 << docTitle)
        , groupPrologue desc
        , listGroup users removeUri
        , forms addUri
        ]
  where	docTitle = groupTitle desc ++ case groupShort desc of
                                        []    -> ""
                                        short -> ": " ++ short

forms :: Maybe String -> [Html]
forms Nothing    = []
forms (Just uri) = addUser uri

addUser :: String -> [Html]
addUser uri =
    [ h3 << "Add user"
    , gui uri ! [theclass "box"] <<
        [ p << [stringToHtml "User: ", textfield "user"]
        , submit "submit" "Add maintainer"
        ]
    ]

removeUser :: Users.UserName -> String -> [Html]
removeUser name uri =
    [ gui (uri </> display name) <<
       [ hidden "_method" "DELETE"
       , submit "submit" "Remove"
       ]
    ]

listGroup :: [Users.UserName] -> Maybe String -> [Html]
listGroup [] _ = []
listGroup users muri =
    [ p << unordList (map displayName users)
    ]
  where displayName uname = [ toHtml $ display uname ++ " " ] ++
                            fromMaybe [] (fmap (removeUser uname) muri)

