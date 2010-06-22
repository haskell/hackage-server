module Distribution.Server.Features.Users (
    UserFeature(..),
    initUsersFeature,
    withUserNamePath,
    withUserPath,
    withUserName
  ) where

import Distribution.Server.Feature
import Distribution.Server.Features.Core
import Distribution.Server.Resource
import Distribution.Server.Hook
import Distribution.Server.Types

import Distribution.Server.Users.State as State
import Distribution.Server.Users.Types
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Users (Users)

import qualified Distribution.Server.Auth.Basic as Auth
import qualified Distribution.Server.Auth.Types as Auth
import qualified Distribution.Server.Auth.Crypt as Auth
import Distribution.Server.Auth.Types

import Happstack.Server hiding (port)
import Happstack.State hiding (Version)
import Data.List (intercalate)
import qualified Data.Map as Map
import System.Random (newStdGen)

import Distribution.Text (display, simpleParse)

import Control.Monad (MonadPlus(..), liftM3)
import Control.Monad.Trans (MonadIO(..))

-- | A feature to allow manipulation of the database of users.
--

-- FIXME: require authentication here
data UserFeature = UserFeature {
    userResource  :: UserResource,
    userAdded :: HookList (IO ())
}

data UserResource = UserResource {
    userList :: Resource,
    userPage :: Resource,
    passwordResource :: Resource,
    enabledResource  :: Resource,
    loginResource :: Resource
}

instance HackageFeature UserFeature where
    getFeature userf = HackageModule
      { featureName = "users"
      , resources   = map ($userResource userf) [userList, userPage, passwordResource, enabledResource, loginResource]
      , dumpBackup    = Nothing
      , restoreBackup = Nothing
      }

initUsersFeature :: CoreFeature -> IO UserFeature
initUsersFeature _ = do
    addHook <- newHookList
    return UserFeature
      { userResource = UserResource
          { userList = (resourceAt "/users/") { resourceGet = [("txt", textUserList)], resourcePost = [("", adminAddUser)] }
          , userPage = (resourceAt "/user/:username/") { resourceGet = [("txt", textUserPage)], resourceDelete = [("", deleteAccount)] }
          , passwordResource = (resourceAt "/user/:username/password") { resourcePut = [("", changePassword)] }
          , enabledResource = (resourceAt "/user/:username/enabled") { resourcePut = [("", enabledAccount)] }
          , loginResource = (resourceAt "/users/login") { resourceGet = [("", requireAuth)] } -- also split into basic/digest
          }
      , userAdded = addHook
      }
  where
    textUserList _ _ = fmap (toResponse . intercalate ", " . map display . Map.keys . Users.userNameMap) (query GetUserDb)
    textUserPage _ dpath = withUserPath dpath $ \_ info -> return . toResponse $ "User page for " ++ display (userName info) ++ ", and your secret information is " ++ show info
    requireAuth _ _ = query GetUserDb >>= \users -> do
        uid <- Auth.requireHackageAuth users Nothing Nothing
        -- below would return a 404 if the user wasn't found, which is impossible if xe was
        -- just authenticated. perhaps requireHackageAuth should return UserInfo as a type-level
        -- token of this unpossibility
        info <- maybe mzero return $ Users.lookupId uid users
        seeOther ("/user/" ++ display (userName info)) $ toResponse ()
    deleteAccount _ dpath = withUserPath dpath $ \uid _ -> do
        update (DeleteUser uid)
        return (result 204 "")
    enabledAccount _ dpath = withUserPath dpath $ \uid _ -> do
        enabled <- getDataFn (look "enabled")
        -- for a checkbox, prescence in data string means 'checked'
        case enabled of
            Nothing -> update (SetEnabledUser uid False)
            Just _  -> update (SetEnabledUser uid True)
        return (toResponse ())

-- | Resources representing the collection of known users.
--
-- Features:
--
-- * listing the collection of users
-- * adding and deleting users
-- * enabling and disabling accounts
-- * changing user's name and password
--

withUserNamePath :: DynamicPath -> (UserName -> ServerPart Response) -> ServerPart Response
withUserNamePath dpath func = case simpleParse =<< lookup "username" dpath of
    Nothing    -> notFound $ toResponse "Could not find user: not a valid username"
    Just uname -> func uname

withUserName :: UserName -> (UserId -> UserInfo -> ServerPart Response) -> ServerPart Response
withUserName uname func = do
    users <- query GetUserDb
    case Users.lookupName uname users of
      Nothing  -> notFound $ toResponse "Could not find user: not presently registered"
      Just uid -> case Users.lookupId uid users of
        Nothing   -> notFound $ toResponse "Could not find user: internal server error"
        Just info -> func uid info

withUserPath :: DynamicPath -> (UserId -> UserInfo -> ServerPart Response) -> ServerPart Response
withUserPath dpath func = withUserNamePath dpath $ \name -> withUserName name func

instance FromReqURI UserName where
  fromReqURI = simpleParse

adminAddUser :: Config -> DynamicPath -> ServerPart Response
adminAddUser _ _ = do
    reqData <- getDataFn lookUserNamePasswords
    case reqData of
        Nothing -> ok $ toResponse "try to fill out all the fields"
        Just (uname, pwd1, pwd2) -> doAdminAddUser uname (PasswdPlain pwd1) (PasswdPlain pwd2)
   where lookUserNamePasswords = do
             uname <- look "username"
             pwd1 <- look "password"
             pwd2 <- look "repeat-password"
             return (uname, pwd1, pwd2)


doAdminAddUser :: String -> PasswdPlain -> PasswdPlain -> ServerPart Response
doAdminAddUser _ pwd1 pwd2 | pwd1 /= pwd2 = ok $ toResponse "Entered passwords do not match"
doAdminAddUser userNameStr password _ = case simpleParse userNameStr of
    Nothing -> ok $ toResponse "Not a valid user name!"
    Just uname -> do
      let userAuth = Auth.newDigestPass uname password "hackage"
      muid <- update $ AddUser uname (UserAuth userAuth DigestAuth)
      case muid of
        Nothing  -> forbidden $ toResponse "already exists"
        Just _   -> seeOther ("/user/" ++ userNameStr) (toResponse ())

data ChangePassword = ChangePassword { first :: String, second :: String, newAuthType :: Auth.AuthType } deriving (Eq, Show)
instance FromData ChangePassword where
	fromData = liftM3 ChangePassword (look "password" `mplus` return "") (look "repeat-password" `mplus` return "")
                                     (fmap (maybe Auth.BasicAuth (const Auth.DigestAuth) . lookup "auth") lookPairs) --checked: digest auth

changePassword :: Config -> DynamicPath -> ServerPart Response
changePassword _ dpath = do
    users  <- query State.GetUserDb
    admins <- query State.GetHackageAdmins
    uid <- Auth.requireHackageAuth users Nothing Nothing
    let -- maybe the name specified in the path here
        muserPathName = simpleParse =<< lookup "username" dpath
        -- maybe the id of that name
        muserPathId = flip Users.lookupName users =<< muserPathName
    case (muserPathId, muserPathName) of
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
                    then ok $ toResponse "Password Changed"
                    else ok $ toResponse "Error changing password"
              else forbidden $ toResponse "Copies of new password do not match or is an invalid password (ex: blank)"
          else forbidden . toResponse $ "Cannot change password for " ++ display userPathName
      (Nothing, Just userPathName) -> notFound . toResponse $ "User " ++ display userPathName ++ " doesn't exist"
      _ -> notFound . toResponse $ "Invalid user name - doesn't exist"

newBasicPass :: MonadIO m => Auth.PasswdPlain -> m UserAuth
newBasicPass pwd = do
    gen <- liftIO newStdGen
    return $ UserAuth (Auth.newBasicPass gen pwd) Auth.BasicAuth

newDigestPass :: UserName -> PasswdPlain -> UserAuth
newDigestPass name pwd = UserAuth (Auth.newDigestPass name pwd "hackage") Auth.DigestAuth

