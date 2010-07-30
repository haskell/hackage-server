module Distribution.Server.Features.Users (
    UserFeature(..),
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

    GroupResource(..),
    groupResourceAt,
    groupResourcesAt,
    withGroup,
    withGroupEditAuth,
    getGroupIndex
  ) where

import Distribution.Server.Feature
import Distribution.Server.Features.Core
import Distribution.Server.Resource
import Distribution.Server.Hook
import Distribution.Server.Error
import Distribution.Server.Types
import qualified Distribution.Server.Cache as Cache

import Distribution.Server.Users.State as State
import Distribution.Server.Users.Types
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..), UserList, nullDescription)

import qualified Distribution.Server.Auth.Basic as Auth
import qualified Distribution.Server.Auth.Types as Auth
import qualified Distribution.Server.Auth.Crypt as Auth

import Happstack.Server hiding (port)
import Happstack.State hiding (Version)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (fix)
import System.Random (newStdGen)

import Distribution.Text (display, simpleParse)

import Control.Monad (MonadPlus(..), liftM3)
import Control.Monad.Trans (MonadIO(..))

-- | A feature to allow manipulation of the database of users.
--

data UserFeature = UserFeature {
    userResource  :: UserResource,
    userAdded :: Hook (IO ()),
    groupAddUser :: UserGroup -> DynamicPath -> MServerPart (),
    groupDeleteUser :: UserGroup -> DynamicPath -> MServerPart (),
    groupIndex :: Cache.Cache GroupIndex,
    adminGroup :: UserGroup
    -- This group allows users to upload packages, as opposed to merely use social features
    -- It's here because it should either be set on signing up or not
  --uploadGroup :: UserGroup
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

instance HackageFeature UserFeature where
    getFeature userf = HackageModule
      { featureName = "users"
      , resources   = map ($userResource userf)
            [userList, userPage, passwordResource, enabledResource,
             groupResource . adminResource, groupUserResource . adminResource]
      , dumpBackup    = Nothing
      , restoreBackup = Nothing
      }

-- TODO: add renaming
initUsersFeature :: Config -> CoreFeature -> IO UserFeature
initUsersFeature _ _ = do
    addHook <- newHook
    groupCache <- Cache.newCacheable IntMap.empty
    (adminG, adminR) <- groupResourceAt groupCache "/users/admins/" adminGroupDesc
    return $ UserFeature
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
      }

{- result: see-other for user page or authentication error
requireAuth :: MServerPart Response
requireAuth = do
    users <- query GetUserDb
    Auth.withHackageAuth users Nothing Nothing $ \_ info -> do
        fmap Right $ seeOther ("/user/" ++ display (userName info)) $ toResponse ()
-}

-- result: either not-found, not-authenticated, or 204 (success)
deleteAccount :: UserName -> MServerPart ()
deleteAccount uname = withUserName uname $ \uid _ -> do
    users <- query GetUserDb
    admins <- query State.GetHackageAdmins
    Auth.withHackageAuth users (Just admins) Nothing $ \_ _ -> do
        update (DeleteUser uid)
        return $ Right ()

-- result: not-found, not authenticated, or ok (success)
enabledAccount :: UserName -> MServerPart ()
enabledAccount uname = withUserName uname $ \uid _ -> do
    users <- query GetUserDb
    admins <- query State.GetHackageAdmins
    Auth.withHackageAuth users (Just admins) Nothing $ \_ _ -> do
        enabled <- getDataFn (look "enabled")
        -- for a checkbox, prescence in data string means 'checked'
        case enabled of
            Nothing -> update (SetEnabledUser uid False)
            Just _  -> update (SetEnabledUser uid True)
        return $ Right ()

-- | Resources representing the collection of known users.
--
-- Features:
--
-- * listing the collection of users
-- * adding and deleting users
-- * enabling and disabling accounts
-- * changing user's name and password
--

withUserNamePath :: DynamicPath -> (UserName -> ServerPart a) -> ServerPart a
withUserNamePath dpath = require (return $ simpleParse =<< lookup "username" dpath)

withUserName :: UserName -> (UserId -> UserInfo -> MServerPart a) -> MServerPart a
withUserName uname func = do
    users <- query GetUserDb
    case Users.lookupName uname users of
      Nothing  -> userLost "Could not find user: not presently registered"
      Just uid -> case Users.lookupId uid users of
        Nothing   -> userLost "Could not find user: internal server error"
        Just info -> func uid info
  where userLost = returnError 404 "User not found" . return . MText

withUserPath :: DynamicPath -> (UserId -> UserInfo -> MServerPart a) -> MServerPart a
withUserPath dpath func = withUserNamePath dpath $ \name -> withUserName name func

instance FromReqURI UserName where
  fromReqURI = simpleParse

adminAddUser :: MServerPart Response
adminAddUser = do
    reqData <- getDataFn lookUserNamePasswords
    case reqData of
        Nothing -> returnError 400 "Error registering user"
                   [MText "Username, password, or repeated password invalid."]
        Just (ustr, pwd1, pwd2) ->
            responseWith (newUserWithAuth ustr (PasswdPlain pwd1) (PasswdPlain pwd2)) $ \uname ->
            fmap Right $ seeOther ("/user/" ++ display uname) (toResponse ())
   where lookUserNamePasswords = do
             uname <- look "username"
             pwd1 <- look "password"
             pwd2 <- look "repeat-password"
             return (uname, pwd1, pwd2)

newUserWithAuth :: String -> PasswdPlain -> PasswdPlain -> MServerPart UserName
newUserWithAuth _ pwd1 pwd2 | pwd1 /= pwd2 = returnError 400 "Error registering user" [MText "Entered passwords do not match"]
newUserWithAuth userNameStr password _ = case simpleParse userNameStr of
    Nothing -> returnError 400 "Error registering user" [MText "Not a valid user name!"]
    Just uname -> do
      let userAuth = Auth.newDigestPass uname password "hackage"
      muid <- update $ AddUser uname (UserAuth userAuth DigestAuth)
      case muid of
        Nothing  -> returnError 403 "Error registering user" [MText "User already exists"]
        Just _   -> returnOk uname

data ChangePassword = ChangePassword { first :: String, second :: String, newAuthType :: Auth.AuthType } deriving (Eq, Show)
instance FromData ChangePassword where
	fromData = liftM3 ChangePassword (look "password" `mplus` return "") (look "repeat-password" `mplus` return "")
                                     (fmap (maybe Auth.BasicAuth (const Auth.DigestAuth) . lookup "auth") lookPairs) --checked: digest auth

-- Arguments: the auth'd user id, the user path id (derived from the :username)
canChangePassword :: MonadIO m => UserId -> UserId -> m Bool
canChangePassword uid userPathId = do
    admins <- query State.GetHackageAdmins
    return $ uid == userPathId || (uid `Group.member` admins)

changePassword :: UserName -> MServerPart ()
changePassword userPathName = do
    users  <- query State.GetUserDb
    Auth.withHackageAuth users Nothing Nothing $ \uid _ ->
        case Users.lookupName userPathName users of
          Just userPathId -> do
            -- if this user's id corresponds to the one in the path, or is an admin
            canChange <- canChangePassword uid userPathId
            if canChange
              then do
                pwd <- maybe (return $ ChangePassword "not" "valid" Auth.BasicAuth) return =<< getData
                if (first pwd == second pwd && first pwd /= "")
                  then do
                    let passwd = PasswdPlain (first pwd)
                    auth <- case newAuthType pwd of 
                        Auth.BasicAuth  -> newBasicPass passwd
                        Auth.DigestAuth -> return $ newDigestPass userPathName passwd
                    res <- update $ ReplaceUserAuth userPathId auth
                    if res
                        then returnOk ()
                        else forbidChange "Error changing password"
                  else forbidChange "Copies of new password do not match or is an invalid password (ex: blank)"
              else forbidChange $ "Not authorized to change password for " ++ display userPathName
          Nothing -> returnError 403 "Error changing password" 
                         [MText $ "User " ++ display userPathName ++ " doesn't exist"]
  where
    forbidChange = returnError 403 "Error changing password" . return . MText

newBasicPass :: MonadIO m => Auth.PasswdPlain -> m UserAuth
newBasicPass pwd = do
    gen <- liftIO newStdGen
    return $ UserAuth (Auth.newBasicPass gen pwd) Auth.BasicAuth

newDigestPass :: UserName -> PasswdPlain -> UserAuth
newDigestPass name pwd = UserAuth (Auth.newDigestPass name pwd "hackage") Auth.DigestAuth

------ User group management
adminGroupDesc :: UserGroup
adminGroupDesc = UserGroup {
    groupDesc = nullDescription { groupTitle = "Hackage admins", groupEntityURL = "/" },
    queryUserList = query GetHackageAdmins,
    addUserList = update . AddHackageAdmin,
    removeUserList = update . RemoveHackageAdmin,
    canAddGroup = [],
    canRemoveGroup = []
}

withGroup :: MServerPart UserGroup -> (UserGroup -> MServerPart a) -> MServerPart a
withGroup groupGen func = groupGen >>= \mgroup -> case mgroup of
    Left err -> returnError' err
    Right group -> func group

doGroupAddUser :: UserGroup -> DynamicPath -> MServerPart ()
doGroupAddUser group _ = do
    users <- query GetUserDb
    muser <- getDataFn $ look "user"
    case muser of
        Nothing -> addError "Bad request (could not find 'user' argument)"
        Just ustr -> case simpleParse ustr >>= \uname -> Users.lookupName uname users of
            Nothing -> addError $ "No user with name " ++ show ustr ++ " found"
            Just uid -> do                    
                ulist <- liftIO . Group.queryGroups $ canAddGroup group
                Auth.withHackageAuth users (Just ulist) Nothing $ \_ _ -> do
                    liftIO $ addUserList group uid
                    returnOk ()
   where addError = returnError 400 "Failed to add user" . return . MText

doGroupDeleteUser :: UserGroup -> DynamicPath -> MServerPart ()
doGroupDeleteUser group dpath = withUserPath dpath $ \uid _ -> do
    users <- query GetUserDb
    ulist <- liftIO . Group.queryGroups $ canRemoveGroup group
    Auth.withHackageAuth users (Just ulist) Nothing $ \_ _ -> do
        liftIO $ removeUserList group uid
        returnOk ()

withGroupEditAuth :: UserGroup -> (Bool -> Bool -> MServerPart a) -> MServerPart a
withGroupEditAuth group func = do
    users  <- query GetUserDb
    addList    <- liftIO . Group.queryGroups $ canAddGroup group
    removeList <- liftIO . Group.queryGroups $ canRemoveGroup group
    Auth.withHackageAuth users Nothing Nothing $ \uid _ -> do
        let (canAdd, canDelete) = (uid `Group.member` addList, uid `Group.member` removeList)
        if not (canAdd || canDelete)
            then returnError 403 "Forbidden" [MText "Can't edit permissions for user group"]
            else func canAdd canDelete

---- Encapsulation of resources related to editing a user group.
type GroupIndex = IntMap (Set String)

data GroupResource = GroupResource {
    groupResource :: Resource,
    groupUserResource :: Resource,
    getGroup :: DynamicPath -> MIO UserGroup
}

groupResourceAt :: Cache.Cache GroupIndex -> String -> UserGroup -> IO (UserGroup, GroupResource)
groupResourceAt users uri group = do
    let mainr = resourceAt uri
        groupUri = renderResource mainr []
        group' = group
          { addUserList = \uid -> do
                addGroupIndex users uid groupUri
                addUserList group uid
          , removeUserList = \uid -> do
                removeGroupIndex users uid groupUri
                addUserList group uid
          }
    ulist <- queryUserList group
    initGroupIndex users ulist groupUri
    return $ (,) group' $ GroupResource
      { groupResource = extendResourcePath "/.:format" mainr
      , groupUserResource = extendResourcePath "/user/:username.:format" mainr
      , getGroup = \_ -> returnOk group'
      }

groupResourcesAt :: Cache.Cache GroupIndex -> String -> (DynamicPath -> MIO UserGroup) -> [DynamicPath] -> IO GroupResource
groupResourcesAt users uri groupGen dpaths = do
    let mainr = resourceAt uri
        collectUserList genpath =
            groupGen genpath >>=
            either (\_ -> return Group.empty) (liftIO . queryUserList) >>= \ulist ->
            initGroupIndex users ulist (renderResource' mainr genpath)
    mapM_ collectUserList dpaths
    return $ GroupResource
      { groupResource = extendResourcePath "/.:format" mainr
      , groupUserResource = extendResourcePath "/user/:username.:format" mainr
      , getGroup = \dpath -> do
          mgroup <- groupGen dpath
          return $ fmap (\group -> group
              { addUserList = \uid -> do
                    addGroupIndex users uid (renderResource' mainr dpath)
                    addUserList group uid
              , removeUserList = \uid -> do
                    removeGroupIndex users uid (renderResource' mainr dpath)
                    addUserList group uid
              }) mgroup
      }

---------------------------------------------------------------
addGroupIndex :: MonadIO m => Cache.Cache GroupIndex -> UserId -> String -> m ()
addGroupIndex users (UserId uid) uri =
    Cache.modifyCache users (IntMap.insertWith Set.union uid (Set.singleton uri))

removeGroupIndex :: MonadIO m => Cache.Cache GroupIndex -> UserId -> String -> m ()
removeGroupIndex users (UserId uid) uri =
    Cache.modifyCache users (IntMap.update (keepSet . Set.delete uri) uid)
  where keepSet m = if Set.null m then Nothing else Just m

initGroupIndex :: MonadIO m => Cache.Cache GroupIndex -> UserList -> String -> m ()
initGroupIndex users ulist uri =
    Cache.modifyCache users (IntMap.unionWith Set.union (IntMap.fromList . map mkEntry $ Group.enumerate ulist))
  where mkEntry (UserId uid) = (uid, Set.singleton uri)

getGroupIndex :: (Functor m, MonadIO m) => Cache.Cache GroupIndex -> UserId -> m [String]
getGroupIndex users (UserId uid) = fmap (maybe [] Set.toList . IntMap.lookup uid) $ Cache.getCache users

