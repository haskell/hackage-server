module Distribution.Server.Features.Users (
    UserFeature(..),
    UserResource(..),
    initUsersFeature,
    withUserNamePath,
    withUserPath,
    withUserName,

    textDisplayUsers,
    GroupResource(..),
    groupResourceAt,
    withGroup,
    withGroupEditAuth
  ) where

import Distribution.Server.Feature
import Distribution.Server.Features.Core
import Distribution.Server.Resource
import Distribution.Server.Hook
import Distribution.Server.Error
import Distribution.Server.Types

import Distribution.Server.Users.State as State
import Distribution.Server.Users.Types
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserGroup(..), GroupDescription(..))

import qualified Distribution.Server.Auth.Basic as Auth
import qualified Distribution.Server.Auth.Types as Auth
import qualified Distribution.Server.Auth.Crypt as Auth

import Happstack.Server hiding (port)
import Happstack.State hiding (Version)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Function (fix)
import System.Random (newStdGen)

import Distribution.Text (display, simpleParse)

import Control.Monad (MonadPlus(..), liftM3)
import Control.Monad.Trans (MonadIO(..))

-- | A feature to allow manipulation of the database of users.
--

-- FIXME: require authentication here
data UserFeature = UserFeature {
    userResource  :: UserResource,
    userAdded :: Hook (IO ()),
    groupAddUser :: UserGroup -> DynamicPath -> MServerPart (),
    groupDeleteUser :: UserGroup -> DynamicPath -> MServerPart ()
}

data UserResource = UserResource {
    userList :: Resource,
    userPage :: Resource,
    passwordResource :: Resource,
    enabledResource  :: Resource,
    loginResource :: Resource,
    adminResource :: GroupResource,

    userListUri :: String -> String,
    userPageUri :: String -> UserName -> String,
    userPasswordUri :: UserName -> String,
    userEnabledUri  :: UserName -> String,
    userLoginUri :: Maybe AuthType -> String,
    adminPageUri :: String -> String
}

instance HackageFeature UserFeature where
    getFeature userf = HackageModule
      { featureName = "users"
      , resources   = map ($userResource userf)
            [userList, userPage, passwordResource, enabledResource, loginResource,
             groupResource . adminResource, groupUserResource . adminResource]
      , dumpBackup    = Nothing
      , restoreBackup = Nothing
      }

initUsersFeature :: Config -> CoreFeature -> IO UserFeature
initUsersFeature _ core = do
    addHook <- newHook
    return UserFeature
      { userResource = fix $ \r -> UserResource
          { userList = (resourceAt "/users/.:format") { resourceGet = [("txt", textUserList)], resourcePost = [("txt", textResponse . adminAddUser)] }
          , userPage = (resourceAt "/user/:username.:format") { resourceGet = [("txt", textUserPage)], resourceDelete = [("txt", textDeleteAccount)] }
          , passwordResource = (resourceAt "/user/:username/password") { resourcePut = [("txt", textChangePassword)] }
          , enabledResource = (resourceAt "/user/:username/enabled") { resourcePut = [("txt", textEnabledAccount)] }
          , loginResource = (resourceAt "/users/login") { resourceGet = [("txt", \_ -> textResponse requireAuth)] } -- also split into basic/digest
          , adminResource = groupResourceAt "/users/admins/" (\_ -> returnOk $ adminGroup core)

          , userListUri = \format -> renderResource (userList r) [format]
          , userPageUri = \format uname -> renderResource (userPage r) [display uname, format]
          , userPasswordUri = \uname -> renderResource (passwordResource r) [display uname]
          , userEnabledUri  = \uname -> renderResource (enabledResource  r) [display uname]
          , userLoginUri = \_ -> renderResource (loginResource r) []
          , adminPageUri = \format -> renderResource (groupResource $ adminResource r) [format]
          }
      , userAdded = addHook
      , groupAddUser = doGroupAddUser
      , groupDeleteUser = doGroupDeleteUser
      }

  where
    -- result: list of users
    textUserList _ = fmap (toResponse . intercalate ", " . map display . Map.keys . Users.userNameMap) (query GetUserDb)
    -- result: either not-found error or user info, with links to account management if logged in
    textUserPage dpath = textResponse $
                         withUserPath dpath $ \_ info ->
        returnOk . toResponse $ "User page for " ++ display (userName info)
    textChangePassword = textTransaction changePassword "Password changed"
    textDeleteAccount = textTransaction deleteAccount "Account deleted"
    textEnabledAccount = textTransaction enabledAccount "Account status set"
    textTransaction :: (DynamicPath -> MServerPart a) -> String -> DynamicPath -> ServerPart Response
    textTransaction func success dpath = func dpath >>= \res -> case res of
        Left err -> makeTextError err
        Right {} -> ok . toResponse $ success

-- result: see-other for user page or authentication error
requireAuth :: MServerPart Response
requireAuth = do
    users <- query GetUserDb
    Auth.withHackageAuth users Nothing Nothing $ \_ info -> do
        fmap Right $ seeOther ("/user/" ++ display (userName info)) $ toResponse ()

-- result: either not-found, not-authenticated, or 204 (success)
deleteAccount :: DynamicPath -> MServerPart ()
deleteAccount dpath = withUserPath dpath $ \uid _ -> do
    users <- query GetUserDb
    admins <- query State.GetHackageAdmins
    Auth.withHackageAuth users (Just admins) Nothing $ \_ _ -> do
        update (DeleteUser uid)
        return $ Right ()

-- result: not-found, not authenticated, or ok (success)
enabledAccount :: DynamicPath -> MServerPart ()
enabledAccount dpath = withUserPath dpath $ \uid _ -> do
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

adminAddUser :: DynamicPath -> MServerPart Response
adminAddUser _ = do
    reqData <- getDataFn lookUserNamePasswords
    case reqData of
        Nothing -> returnError 400 "Error registering user"
                   [MText "Username, password, or repeated password invalid."]
        Just (uname, pwd1, pwd2) -> doAdminAddUser uname (PasswdPlain pwd1) (PasswdPlain pwd2)
   where lookUserNamePasswords = do
             uname <- look "username"
             pwd1 <- look "password"
             pwd2 <- look "repeat-password"
             return (uname, pwd1, pwd2)


doAdminAddUser :: String -> PasswdPlain -> PasswdPlain -> MServerPart Response
doAdminAddUser _ pwd1 pwd2 | pwd1 /= pwd2 = returnError 400 "Error registering user" [MText "Entered passwords do not match"]
doAdminAddUser userNameStr password _ | userNameStr /= "edit" = case simpleParse userNameStr of
    Nothing -> returnError 400 "Error registering user" [MText "Not a valid user name!"]
    Just uname -> do
      let userAuth = Auth.newDigestPass uname password "hackage"
      muid <- update $ AddUser uname (UserAuth userAuth DigestAuth)
      case muid of
        Nothing  -> returnError 403 "Error registering user" [MText "User already exists"]
        Just _   -> fmap Right $ seeOther ("/user/" ++ userNameStr) (toResponse ())
doAdminAddUser _ _ _ = returnError 400 "Error registering user" [MText "Users can't be named 'edit' at the moment (used by group URI)"]

data ChangePassword = ChangePassword { first :: String, second :: String, newAuthType :: Auth.AuthType } deriving (Eq, Show)
instance FromData ChangePassword where
	fromData = liftM3 ChangePassword (look "password" `mplus` return "") (look "repeat-password" `mplus` return "")
                                     (fmap (maybe Auth.BasicAuth (const Auth.DigestAuth) . lookup "auth") lookPairs) --checked: digest auth

changePassword :: DynamicPath -> MServerPart ()
changePassword dpath = do
    users  <- query State.GetUserDb
    admins <- query State.GetHackageAdmins
    Auth.withHackageAuth users Nothing Nothing $ \uid _ ->
        let muserPathName = simpleParse =<< lookup "username" dpath
            -- ^ the name specified in the dynamic path
            muserPathId = flip Users.lookupName users =<< muserPathName
            -- ^ the id of that name
        in case (muserPathId, muserPathName) of
          (Just userPathId, Just userPathName) ->
            -- if this user's id corresponds to the one in the path, or is an admin
            if uid == userPathId || (uid `Group.member` admins)
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
          (Nothing, Just userPathName) -> returnError 403 "Error changing password" 
                                          [MText $ "User " ++ display userPathName ++ " doesn't exist"]
          _ -> mzero
  where
    forbidChange = returnError 403 "Error changing password" . return . MText

newBasicPass :: MonadIO m => Auth.PasswdPlain -> m UserAuth
newBasicPass pwd = do
    gen <- liftIO newStdGen
    return $ UserAuth (Auth.newBasicPass gen pwd) Auth.BasicAuth

newDigestPass :: UserName -> PasswdPlain -> UserAuth
newDigestPass name pwd = UserAuth (Auth.newDigestPass name pwd "hackage") Auth.DigestAuth

------ User group management
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
data GroupResource = GroupResource {
    groupResource :: Resource,
    groupUserResource :: Resource,
    getGroup :: DynamicPath -> MServerPart UserGroup
}

groupResourceAt :: String -> (DynamicPath -> MServerPart UserGroup) -> GroupResource
groupResourceAt uri groupGen = let mainr = resourceAt uri in GroupResource
  { groupResource = (extendResourcePath "/.:format" mainr) { resourceGet = [("txt", textResponse . getList)], resourcePost = [("txt", textResponse . postUser)] }
  , groupUserResource = (extendResourcePath "/user/:username.:format" mainr) { resourceDelete = [("txt", textResponse . deleteFromGroup)] }
  , getGroup = groupGen
  }
  where
    getList dpath = withGroup (groupGen dpath) $ \group -> do
        fmap Right $ textDisplayUsers group
    postUser dpath = withGroup (groupGen dpath) $ \group -> do
        res <- doGroupAddUser group dpath
        case res of
            Left err -> returnError' err
            Right {} -> returnOk . toResponse $ "Added user"
    deleteFromGroup dpath = withGroup (groupGen dpath) $ \group -> do
        res <- doGroupDeleteUser group dpath
        case res of
            Left err -> returnError' err
            Right {} -> returnOk . toResponse $ "Deleted user"

textDisplayUsers :: UserGroup -> ServerPart Response
textDisplayUsers group = do
    ulist <- liftIO . queryUserList $ group
    unames <- query $ ListGroupMembers ulist
    ok . toResponse $ groupTitle (groupDesc group) ++
        if null unames then "\nThis group has no members.\n"
                       else "\nThis group has " ++ memberAmount (length unames) ++ ":" ++
                                intercalate ", " (map display unames) ++ "\n"
  where memberAmount n = if n == 1 then "1 member" else show n ++ " members"
