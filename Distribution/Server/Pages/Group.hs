-- Body of the HTML page for a package
module Distribution.Server.Pages.Group (
    groupPage,
    renderGroupName
    -- renderGroupNameWithCands
  ) where

import Text.XHtml.Strict
import System.FilePath.Posix ((</>))
import Distribution.Server.Pages.Template (hackagePage)
import qualified Distribution.Server.Users.Types as Users
import Distribution.Server.Users.Group (GroupDescription(..))
import qualified Distribution.Server.Users.Group as Group
import Distribution.Text
import Data.Maybe

renderGroupName :: GroupDescription -> Maybe String -> Html
renderGroupName desc murl =
    maybeUrl (groupTitle desc) murl
      +++
    maybe noHtml (\(for, mfor) -> " for " +++ maybeUrl for mfor ) (groupEntity desc)
      +++
    maybe noHtml (\(_, mfor) -> " : " +++ candUrl "No Candidates" mfor ) (groupEntity desc)
  where maybeUrl text = maybe (toHtml text) (\url -> anchor ! [href url] << text)
        candUrl text = maybe (toHtml text) (\url -> anchor ! [href $ url ++ "/candidates"] << "candidates")

-- Primitive access control: the URI to post a new user request to, or the the URI/user/<username> to DELETE
-- if neither adding or removing is enabled, a link to a URI/edit page is provided
groupPage :: [Users.UserName] -> String -> (Bool, Bool) -> GroupDescription -> Html
groupPage users baseUri controls desc = hackagePage (Group.groupName desc) (groupBody users baseUri controls desc)

-- | Body of the page
-- If either addUri or removeUri are true, it can be assumed that we are one the
-- \/edit subpage of the group.
groupBody :: [Users.UserName] -> String -> (Bool, Bool) -> GroupDescription -> [Html]
groupBody users baseUri (addUri, removeUri) desc =
  [ h2 << renderGroupName desc (if addUri || removeUri then Just baseUri else Nothing)
  , paragraph <<
      [ toHtml $ groupPrologue desc
      , if addUri || removeUri then noHtml else thespan ! [thestyle "color: gray"] <<
          [ toHtml " ["
          , anchor ! [href $ baseUri </> "edit"] << "edit"
          , toHtml "]"
          ]
      ]
  , if addUri then concatHtml $ addUser baseUri else noHtml
  , listGroup users (if removeUri then Just baseUri else Nothing)
  ]

addUser :: String -> [Html]
addUser uri =
    [ h3 << "Add user"
    , gui uri ! [theclass "box"] <<
        [ p << [stringToHtml "User: ", textfield "user"]
        , p << [ stringToHtml "Reason: ", textfield "reason"
               , toHtml " publicly visible in audit log"]
        , submit "submit" "Add member"
        ]
    ]

removeUser :: Users.UserName -> String -> [Html]
removeUser uname uri =
    [ toHtml " "
    , gui (uri </> "user" </> display uname) <<
        [ p << [ stringToHtml "Reason: ", textfield "reason"
               , toHtml " publicly visible in audit log"]
        , hidden "_method" "DELETE"
        , submit "submit" "Remove"
        ]
    ]

listGroup :: [Users.UserName] -> Maybe String -> Html
listGroup [] _ = p << "No member exist presently"
listGroup users muri = unordList (map displayName users)
  where displayName uname = (anchor ! [href $ "/user/" ++ display uname] << display uname) +++
                            fromMaybe [] (fmap (removeUser uname) muri)
