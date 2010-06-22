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

{-usersFeature :: HackageModule
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
}-}

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

