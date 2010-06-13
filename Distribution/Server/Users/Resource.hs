module Distribution.Server.Users.Resource (
    makeGroupResources,
    makeDynamicGroupResources
  ) where

import Distribution.Server.Resource
import Distribution.Server.Types
import Distribution.Server.Users.Types (UserId, UserName)
import Distribution.Server.Users.Group (UserGroup(..))
import Distribution.Server.Users.State
import Distribution.Server.Features.Users
import qualified Distribution.Server.ResourceTypes as Resource
import qualified Distribution.Server.Users.Group as Group
import qualified Distribution.Server.Pages.Group as Pages
import qualified Distribution.Server.Auth.Basic as Auth

import Happstack.Server
import Happstack.State (query)
import Control.Monad.Trans (liftIO)
import Control.Monad
import Data.Maybe (fromJust)
import Distribution.Text

-- the deference is a bit of a cop-out, but this is a convenience function after all
makeGroupResources :: BranchPath -> UserGroup -> [Resource]
makeGroupResources branch group = makeDynamicGroupResources branch (return . return . return $ group)

makeDynamicGroupResources :: BranchPath -> (DynamicPath -> IO (Maybe UserGroup)) -> [Resource]
makeDynamicGroupResources branch groupGen = [viewList, modifyList]
  where
    viewList   = (defaultResource branch) { resourceGet = [("html", getList)], resourcePost = [("", postUser)] }
    modifyList = (defaultResource (DynamicBranch "user":branch)) { resourceDelete = [("", deleteFromGroup)] } --perhaps getUser as well
    -- Resource functions
    getList _ dpath = require (groupGen dpath) $ \group -> do
        doDisplayUsers group (makeGroupURI dpath)
    postUser _ dpath = require (groupGen dpath) $ \group -> do
        doAddUser group dpath
        seeOther (makeGroupURI dpath) (toResponse ())
    deleteFromGroup _ dpath = require (groupGen dpath) $ \group -> do
        doDeleteUser group dpath
        seeOther (makeGroupURI dpath) (toResponse ())
    makeGroupURI dpath = fromJust $ renderURI branch dpath

-- todo: this should return more than (). e.g. if user exists or not
doDeleteUser :: UserGroup -> DynamicPath -> ServerPart Response
doDeleteUser group dpath = withUserPath dpath $ \uid _ -> do
    users  <- query GetUserDb
    -- alternatively, specify which groups can remove from this group in the UserGroup itself
    admins <- query GetHackageAdmins
    Auth.requireHackageAuth users (Just admins) Nothing
    liftIO $ removeUserList group uid
    return $ toResponse ()

doAddUser :: UserGroup -> DynamicPath -> ServerPart Response
doAddUser group dpath = withUserPath dpath $ \uid _ -> do
    users <- query GetUserDb
    ulist <- liftIO $ queryUserList group
    Auth.requireHackageAuth users (Just ulist) Nothing
    liftIO $ addUserList group uid
    return $ toResponse ()

doDisplayUsers :: UserGroup -> String -> ServerPart Response
doDisplayUsers group uri = do
    ulist  <- liftIO $ queryUserList group
    users  <- query GetUserDb
    admins <- query GetHackageAdmins
    auth <- Auth.getHackageAuth users
    let (canAdd, canDel) = either (const (False, False)) (\uid -> (uid `Group.member` ulist, uid `Group.member` admins)) auth
        maybeUri bool = if bool then Just uri else Nothing
    unames <- query $ ListGroupMembers ulist
    ok $ toResponse $ Resource.XHtml $ Pages.groupPage unames (maybeUri canAdd) (maybeUri canDel) (groupDesc group)

