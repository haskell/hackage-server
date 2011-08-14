{-# LANGUAGE DoRec #-}
module Distribution.Server.Features.Users (
    UserFeature,
    userResource,
    adminGroup,
    groupAddUser,
    groupDeleteUser,
    UserResource(..),
    initUsersFeature,
    withUserNamePath,
    withUserPath,
    withUserName,

    changePassword,
    canChangePassword,
    newUserWithAuth,
    adminAddUser,
    enabledAccount,
    deleteAccount,
    runUserFilter,

    GroupResource(..),
    GroupGen,
    groupResourceAt,
    groupResourcesAt,
    withGroupEditAuth,
    getGroupIndex,
    getIndexDesc
  ) where

import Control.Applicative ((<$>), (<*>), optional)

import Distribution.Server.Acid (query, update)
import Distribution.Server.Framework
import Distribution.Server.Features.Core
import qualified Distribution.Server.Framework.Cache as Cache

import Distribution.Server.Users.State as State
import Distribution.Server.Users.Types
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), UserList, nullDescription)

import qualified Distribution.Server.Auth.Basic as Auth
import qualified Distribution.Server.Auth.Types as Auth
import qualified Distribution.Server.Auth.Crypt as Auth

import Happstack.Server hiding (port)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (fix)
import Control.Monad (liftM)

import Distribution.Text (display, simpleParse)

import Control.Monad (MonadPlus(..), liftM3)
import Control.Monad.Trans (MonadIO(..))

-- | A feature to allow manipulation of the database of users.
--
data UserFeature = UserFeature {
    userResource :: UserResource,
    userAdded :: Hook (IO ()),
    groupAddUser :: UserGroup -> DynamicPath -> ServerPartE (),
    groupDeleteUser :: UserGroup -> DynamicPath -> ServerPartE (),
    groupIndex :: Cache.Cache GroupIndex,
    adminGroup :: UserGroup,
    -- Filters for all features modifying the package index
    packageMutate :: Filter (UserId -> IO Bool)
}

data UserResource = UserResource {
    userList :: Resource,
    userPage :: Resource,
    passwordResource :: Resource,
    enabledResource  :: Resource,
    adminResource :: GroupResource,

    userListUri :: String -> String,
    userPageUri :: String -> UserName -> String,
    userPasswordUri :: String -> UserName -> String,
    userEnabledUri  :: String -> UserName -> String,
    adminPageUri :: String -> String
}

instance IsHackageFeature UserFeature where
    getFeatureInterface userf = (emptyHackageFeature "users") {
        featureResources = map ($userResource userf)
            [userList, userPage, passwordResource, enabledResource,
             groupResource . adminResource, groupUserResource . adminResource]
      , featureDumpRestore = Nothing  --ReplaceIndexUsers
      }

-- TODO: add renaming
initUsersFeature :: ServerEnv -> CoreFeature -> IO UserFeature
initUsersFeature _ _ = do
    addHook <- newHook
    mutateFilter <- newHook
    groupCache <- Cache.newCache emptyGroupIndex id
    rec let users = UserFeature
              { userResource = fix $ \r -> UserResource
                  { userList = resourceAt "/users/.:format"
                  , userPage = resourceAt "/user/:username.:format"
                  , passwordResource = resourceAt "/user/:username/password.:format"
                  , enabledResource = resourceAt "/user/:username/enabled.:format"
                  , adminResource = adminR

                  , userListUri = \format -> renderResource (userList r) [format]
                  , userPageUri = \format uname -> renderResource (userPage r) [display uname, format]
                  , userPasswordUri = \format uname -> renderResource (passwordResource r) [display uname, format]
                  , userEnabledUri  = \format uname -> renderResource (enabledResource  r) [display uname, format]
                  , adminPageUri = \format -> renderResource (groupResource $ adminResource r) [format]
                  }
              , userAdded = addHook
              , groupAddUser = doGroupAddUser
              , groupDeleteUser = doGroupDeleteUser
              , groupIndex = groupCache
              , adminGroup = adminG
              , packageMutate = mutateFilter
              }
        (adminG, adminR) <- groupResourceAt users "/users/admins/" adminGroupDesc
    return users

{- result: see-other for user page or authentication error
requireAuth :: ServerPartE Response
requireAuth = do
    users <- query GetUserDb
    Auth.withHackageAuth users Nothing Nothing $ \_ info -> do
        fmap Right $ seeOther ("/user/" ++ display (userName info)) $ toResponse ()
-}

-- result: either not-found, not-authenticated, or 204 (success)
deleteAccount :: UserName -> ServerPartE ()
deleteAccount uname = withUserName uname $ \uid _ -> do
    users <- query GetUserDb
    admins <- query State.GetHackageAdmins
    Auth.withHackageAuth users (Just admins) Nothing $ \_ _ -> do
        update (DeleteUser uid)
        return ()

-- result: not-found, not authenticated, or ok (success)
enabledAccount :: UserName -> ServerPartE ()
enabledAccount uname = withUserName uname $ \uid _ -> do
    users <- query GetUserDb
    admins <- query State.GetHackageAdmins
    Auth.withHackageAuth users (Just admins) Nothing $ \_ _ -> do
        enabled <- optional $ look "enabled"
        -- for a checkbox, prescence in data string means 'checked'
        case enabled of
            Nothing -> update (SetEnabledUser uid False)
            Just _  -> update (SetEnabledUser uid True)
        return ()

-- | Resources representing the collection of known users.
--
-- Features:
--
-- * listing the collection of users
-- * adding and deleting users
-- * enabling and disabling accounts
-- * changing user's name and password
--

withUserNamePath :: DynamicPath -> (UserName -> ServerPartE a) -> ServerPartE a
withUserNamePath dpath = require (return $ simpleParse =<< lookup "username" dpath)

withUserName :: UserName -> (UserId -> UserInfo -> ServerPartE a) -> ServerPartE a
withUserName uname func = do
    users <- query GetUserDb
    case Users.lookupName uname users of
      Nothing  -> userLost "Could not find user: not presently registered"
      Just uid -> case Users.lookupId uid users of
        Nothing   -> userLost "Could not find user: internal server error"
        Just info -> func uid info
  where userLost = errNotFound "User not found" . return . MText

withUserPath :: DynamicPath -> (UserId -> UserInfo -> ServerPartE a) -> ServerPartE a
withUserPath dpath func = withUserNamePath dpath $ \name -> withUserName name func

instance FromReqURI UserName where
  fromReqURI = simpleParse

adminAddUser :: ServerPartE Response
adminAddUser = do
    -- with these lines commented out, self-registration is allowed
  --admins <- query State.GetHackageAdmins
  --users <- query State.GetUserDb
  --Auth.withHackageAuth users (Just admins) Nothing $ \_ _ -> do
    reqData <- getDataFn lookUserNamePasswords
    case reqData of
        (Left errs) -> errBadRequest "Error registering user"
                   ((MText "Username, password, or repeated password invalid.") : map MText errs)
        (Right (ustr, pwd1, pwd2)) -> do
            uname <- newUserWithAuth ustr (PasswdPlain pwd1) (PasswdPlain pwd2)
            seeOther ("/user/" ++ display uname) (toResponse ())
   where lookUserNamePasswords = do
             (,,) <$> look "username" 
                  <*> look "password"
                  <*> look "repeat-password"

newUserWithAuth :: String -> PasswdPlain -> PasswdPlain -> ServerPartE UserName
newUserWithAuth _ pwd1 pwd2 | pwd1 /= pwd2 = errBadRequest "Error registering user" [MText "Entered passwords do not match"]
newUserWithAuth userNameStr password _ = case simpleParse userNameStr of
    Nothing -> errBadRequest "Error registering user" [MText "Not a valid user name!"]
    Just uname -> do
      let userAuth = Auth.newPasswdHash "Hackage"uname password
      muid <- update $ AddUser uname (UserAuth userAuth)
      case muid of
        Nothing  -> errForbidden "Error registering user" [MText "User already exists"]
        Just _   -> return uname

data ChangePassword = ChangePassword { first :: String, second :: String, newAuthType :: Auth.AuthType } deriving (Eq, Show)
instance FromData ChangePassword where
	fromData = liftM3 ChangePassword (look "password" `mplus` return "") (look "repeat-password" `mplus` return "")
                                     (fmap (maybe Auth.BasicAuth (const Auth.DigestAuth) . lookup "auth") lookPairs) --checked: digest auth

-- Arguments: the auth'd user id, the user path id (derived from the :username)
canChangePassword :: MonadIO m => UserId -> UserId -> m Bool
canChangePassword uid userPathId = do
    admins <- query State.GetHackageAdmins
    return $ uid == userPathId || (uid `Group.member` admins)

changePassword :: UserName -> ServerPartE ()
changePassword userPathName = do
    users  <- query State.GetUserDb
    Auth.withHackageAuth users Nothing Nothing $ \uid _ ->
        case Users.lookupName userPathName users of
          Just userPathId -> do
            -- if this user's id corresponds to the one in the path, or is an admin
            canChange <- canChangePassword uid userPathId
            if canChange
              then do
                pwd <- either (const $ return $ ChangePassword "not" "valid" Auth.BasicAuth) return =<< getData
                if (first pwd == second pwd && first pwd /= "")
                  then do
                    let passwd = PasswdPlain (first pwd)
                    auth <- case newAuthType pwd of 
                        Auth.DigestAuth -> return $ newDigestPass userPathName passwd
                    res <- update $ ReplaceUserAuth userPathId auth
                    if res
                        then return ()
                        else forbidChange "Error changing password"
                  else forbidChange "Copies of new password do not match or is an invalid password (ex: blank)"
              else forbidChange $ "Not authorized to change password for " ++ display userPathName
          Nothing -> errForbidden "Error changing password" 
                         [MText $ "User " ++ display userPathName ++ " doesn't exist"]
  where
    forbidChange = errForbidden "Error changing password" . return . MText

newDigestPass :: UserName -> PasswdPlain -> UserAuth
newDigestPass name pwd = UserAuth (Auth.newPasswdHash Auth.authorizationRealm name pwd)

--
runUserFilter :: UserFeature -> UserId -> IO (Maybe ErrorResponse)
runUserFilter userf uid = runFilter' (packageMutate userf) uid >>= \bs -> case or bs of
    True  -> return . Just $ ErrorResponse 403 "Upload failed" [MText "Your account can't upload packages."]
    False -> return Nothing

------ User group management
adminGroupDesc :: UserGroup
adminGroupDesc = UserGroup {
    groupDesc = nullDescription { groupTitle = "Hackage admins" },
    queryUserList = query GetHackageAdmins,
    addUserList = update . AddHackageAdmin,
    removeUserList = update . RemoveHackageAdmin,
    groupExists = return True,
    canAddGroup = [],
    canRemoveGroup = []
}

doGroupAddUser :: UserGroup -> DynamicPath -> ServerPartE ()
doGroupAddUser group _ = do
    users <- query GetUserDb
    muser <- optional $ look "user"
    case muser of
        Nothing -> addError "Bad request (could not find 'user' argument)"
        Just ustr -> case simpleParse ustr >>= \uname -> Users.lookupName uname users of
            Nothing -> addError $ "No user with name " ++ show ustr ++ " found"
            Just uid -> do                    
                ulist <- liftIO . Group.queryGroups $ canAddGroup group
                Auth.withHackageAuth users (Just ulist) Nothing $ \_ _ -> do
                    liftIO $ addUserList group uid
                    return ()
   where addError = errBadRequest "Failed to add user" . return . MText

doGroupDeleteUser :: UserGroup -> DynamicPath -> ServerPartE ()
doGroupDeleteUser group dpath = withUserPath dpath $ \uid _ -> do
    users <- query GetUserDb
    ulist <- liftIO . Group.queryGroups $ canRemoveGroup group
    Auth.withHackageAuth users (Just ulist) Nothing $ \_ _ -> do
        liftIO $ removeUserList group uid
        return ()

withGroupEditAuth :: UserGroup -> (Bool -> Bool -> ServerPartE a) -> ServerPartE a
withGroupEditAuth group func = do
    users  <- query GetUserDb
    addList    <- liftIO . Group.queryGroups $ canAddGroup group
    removeList <- liftIO . Group.queryGroups $ canRemoveGroup group
    Auth.withHackageAuth users Nothing Nothing $ \uid _ -> do
        let (canAdd, canDelete) = (uid `Group.member` addList, uid `Group.member` removeList)
        if not (canAdd || canDelete)
            then errForbidden "Forbidden" [MText "Can't edit permissions for user group"]
            else func canAdd canDelete

------------ Encapsulation of resources related to editing a user group.
data GroupResource = GroupResource {
    groupResource :: Resource,
    groupUserResource :: Resource,
    getGroup :: GroupGen
}

type GroupGen = DynamicPath -> UserGroup

-- This is a mapping of UserId -> group URI and group URI -> description.
-- Like many reverse mappings, it is probably rather volatile. Still, it is
-- a secondary concern, as user groups should be defined by each feature
-- and not global, to be perfectly modular.
data GroupIndex = GroupIndex {
    usersToGroupUri :: IntMap (Set String),
    groupUrisToDesc :: Map String GroupDescription
}
emptyGroupIndex :: GroupIndex
emptyGroupIndex = GroupIndex IntMap.empty Map.empty

-- | Registers a user group for external display. It takes the index group
-- mapping (groupIndex from UserFeature), the base uri of the group, and a
-- UserGroup object with all the necessary hooks. The base uri shouldn't
-- contain any dynamic or varying components. It returns the GroupResource
-- object, and also an adapted UserGroup that updates the cache. You should
-- use this in order to keep the index updated.
groupResourceAt :: UserFeature -> String -> UserGroup -> IO (UserGroup, GroupResource)
groupResourceAt users uri group = do
    let mainr = resourceAt uri
        descr = groupDesc group
        groupUri = renderResource mainr []
        group' = group
          { addUserList = \uid -> do
                addGroupIndex (groupIndex users) uid groupUri descr
                addUserList group uid
          , removeUserList = \uid -> do
                removeGroupIndex (groupIndex users) uid groupUri
                removeUserList group uid
          }
    ulist <- queryUserList group
    initGroupIndex (groupIndex users) ulist groupUri descr
    return $ (,) group' $ GroupResource
      { groupResource = extendResourcePath "/.:format" mainr
      , groupUserResource = extendResourcePath "/user/:username.:format" mainr
      , getGroup = \_ -> group'
      }

-- | Registers a collection of user groups for external display. These groups
-- are usually backing a separate collection. Like groupResourceAt, it takes the
-- index group mapping and a base uri The base uri can contain varying path
-- components, so there should be a group-generating function that, given a
-- DynamicPath, yields the proper UserGroup. The final argument is the initial
-- list of DynamicPaths to build the initial group index. Like groupResourceAt,
-- this function returns an adaptor function that keeps the index updated.
groupResourcesAt :: UserFeature -> String
                 -> GroupGen -> [DynamicPath]
                 -> IO (GroupGen, GroupResource)
groupResourcesAt users uri groupGen dpaths = do
    let mainr = resourceAt uri
        collectUserList genpath = do
            let group = groupGen genpath
            exists <- groupExists group
            case exists of
                False -> return ()
                True  -> queryUserList group >>= \ulist ->
                    initGroupIndex (groupIndex users) ulist (renderResource' mainr genpath) (groupDesc group)
        getGroupFunc dpath =
            let group = groupGen dpath
            in  group
              { addUserList = \uid -> do
                    addGroupIndex (groupIndex users) uid (renderResource' mainr dpath) (groupDesc group)
                    addUserList group uid
              , removeUserList = \uid -> do
                    removeGroupIndex (groupIndex users) uid (renderResource' mainr dpath)
                    addUserList group uid
              }
    mapM_ collectUserList dpaths
    return $ (,) getGroupFunc $ GroupResource
      { groupResource = extendResourcePath "/.:format" mainr
      , groupUserResource = extendResourcePath "/user/:username.:format" mainr
      , getGroup = getGroupFunc
      }

---------------------------------------------------------------
addGroupIndex :: MonadIO m => Cache.Cache GroupIndex -> UserId -> String -> GroupDescription -> m ()
addGroupIndex users (UserId uid) uri desc =
    Cache.modifyCache users $ adjustGroupIndex
        (IntMap.insertWith Set.union uid (Set.singleton uri))
        (Map.insert uri desc)

removeGroupIndex :: MonadIO m => Cache.Cache GroupIndex -> UserId -> String -> m ()
removeGroupIndex users (UserId uid) uri =
    Cache.modifyCache users $ adjustGroupIndex
        (IntMap.update (keepSet . Set.delete uri) uid)
        id
  where keepSet m = if Set.null m then Nothing else Just m

initGroupIndex :: MonadIO m => Cache.Cache GroupIndex -> UserList -> String -> GroupDescription -> m ()
initGroupIndex users ulist uri desc =
    Cache.modifyCache users $ adjustGroupIndex
        (IntMap.unionWith Set.union (IntMap.fromList . map mkEntry $ Group.enumerate ulist))
        (Map.insert uri desc)
  where mkEntry (UserId uid) = (uid, Set.singleton uri)

getGroupIndex :: (Functor m, MonadIO m) => UserFeature -> UserId -> m [String]
getGroupIndex users (UserId uid) = liftM (maybe [] Set.toList . IntMap.lookup uid . usersToGroupUri) $ Cache.getCache (groupIndex users)

getIndexDesc :: MonadIO m => UserFeature -> String -> m GroupDescription
getIndexDesc users uri = liftM (Map.findWithDefault nullDescription uri . groupUrisToDesc) $ Cache.getCache (groupIndex users)

-- partitioning index modifications, a cheap combinator
adjustGroupIndex :: (IntMap (Set String) -> IntMap (Set String))
                 -> (Map String GroupDescription -> Map String GroupDescription)
                 -> GroupIndex -> GroupIndex
adjustGroupIndex f g (GroupIndex a b) = GroupIndex (f a) (g b)
