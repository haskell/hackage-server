module Distribution.Server.Users.ServerParts (
    userAdmin,
    changePassword,
    usersFeature
  ) where

import Happstack.Server hiding (port)
import Happstack.State hiding (Version)

import Distribution.Server.Users.State as State
import qualified Distribution.Server.Auth.Basic as Auth
import qualified Distribution.Server.Auth.Types as Auth
import qualified Distribution.Server.Auth.Crypt as Auth

import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Types
import Distribution.Server.Feature
import Distribution.Server.Types
import qualified Distribution.Server.Auth.Types as Auth
import qualified Distribution.Server.Auth.Crypt as Auth
import qualified Distribution.Server.Auth.Basic as Auth

import Distribution.Text (simpleParse, display)

import System.Random (newStdGen)
import Data.Maybe
import Control.Monad.Trans
import Control.Monad (msum, liftM3, mplus)

usersFeature :: HackageModule
usersFeature = HackageModule {
    featureName = "users",
    -- todo: add checking
    resources   = [ {- (resourceAt "/users/") { resourceGet = Just serveUserList, resourcePost = Just adminAddUser }
                  , (resourceAt "/user/:username") { resourceGet = Just serveUserPage, resourceDelete = Nothing }
                  , (resourceAt "/user/:username/enabled")
                  , (resourceAt "/user/:username/password") { resourcePut = Just changePassword } -}
                  ], -- ++ makeGroupResources (trunkAt "/users/admins") (Group.UserGroup "Site administrators" GetJustHackageAdmins AddHackageAdmin RemoveHackageAdmin),
    dumpBackup    = return [],  
    restoreBackup = Nothing
}

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

-- Assumes that the user has already been autheniticated
-- and has proper permissions
userAdmin :: ServerPart Response
userAdmin = msum
      [ dir "toggle-admin" $ msum
          [ methodSP POST $ do
              reqData <- getDataFn $ do
                uname <- look "user-name"
                makeAdmin <- lookRead "admin"
                return (uname, makeAdmin)
              
              case reqData of
                Nothing -> ok $ toResponse "Bad inputs, somehow"
                Just (uname, makeAdmin) ->
                    adminToggleAdmin (UserName uname) makeAdmin
          ]
      ]

adminToggleAdmin :: UserName -> Bool -> ServerPart Response
adminToggleAdmin uname makeAdmin = do
    mUser <- query $ LookupUserName uname
    if isNothing mUser then ok $ toResponse "Unknown user name" else do
    let Just user = mUser
    if makeAdmin
        then update $ AddHackageAdmin user
        else update $ RemoveHackageAdmin user
    ok $ toResponse "Success!"

