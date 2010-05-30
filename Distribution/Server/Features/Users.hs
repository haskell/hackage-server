module Distribution.Server.Features.Users (
    usersFeature
  ) where

import Distribution.Server.Feature
import qualified Distribution.Server.ResourceTypes as Resource

import Distribution.Server.Users.State as State
import Distribution.Server.Users.Types
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Users (Users)

import qualified Distribution.Server.Auth.Basic as Auth
import qualified Distribution.Server.Auth.Types as Auth
import qualified Distribution.Server.Auth.Crypt as Auth

import Distribution.Server.Users.Permissions(GroupName(..))
import Distribution.Server.Auth.Types (PasswdPlain(..))

import Happstack.Server hiding (port)
import Happstack.State hiding (Version)

import Distribution.Text (display, simpleParse)

import System.Random (newStdGen)
import Control.Monad.Trans
import Control.Monad (MonadPlus(..), msum)

import Text.XHtml.Strict

-- | A feature to allow manipulation of the database of users.
--
usersFeature :: HackageFeature
usersFeature = HackageFeature {

    featureName = "users",

    serverPart  = \_config -> serveUsers,

    dumpBackup    = error "need to implement users dump/restore",
    restoreBackup = error "need to implement users dump/restore"
}

-- | Resources representing the collection of known users.
--
-- Features:
--
-- * listing the collection of users
-- * adding and deleting users
-- * enabling and disabling accounts
-- * changing user's name and password
--

instance FromReqURI UserName where
  fromReqURI = simpleParse


-- /users/
--   GET:    list all the users
--
-- /user/${username}  -- individual user, identified by user name
--   GET:    info about user, in various representations
--   PUT:    create user or replace user details (needs admin auth)
--   DELETE: delete user (needs admin auth)

serveUsers :: ServerPart Response
serveUsers =
    dir "users" $ msum
    [
      -- individual users
      path $ \userName -> msum
        [
          methodSP GET $ do
            users <- query GetUsers
            case Users.lookupName userName users of
              Nothing  -> notFound $ toResponse "No such user"
              Just uid -> ok $ toResponse (userPage uid userName)

        , methodSP PUT $ do
            muid <- update (AddUser userName)
            case muid of
              Nothing  -> notFound $ toResponse "already exists" --FIXME
              Just uid -> do
                setResponseCode 201
                return $ toResponse (userPage uid userName)

        , methodSP DELETE $ do
            users <- query GetUsers
            case Users.lookupName userName users of
              Nothing  -> notFound $ toResponse "No such user"
              Just uid -> do
                update (DeleteUser uid)
                return (result 204 "")

        ]

      -- list all users
    , methodSP GET $ do
        trailingSlash
        users <- query GetUsers
        ok $ toResponse (listAllUsers users)
    ]

listAllUsers :: Users -> Resource.XHtml
listAllUsers users = Resource.XHtml $
  concatHtml
    [ header
      << thetitle << "Hackage: Users"

    , body
      << [ thediv
           << anchor ! [href (display name)]
              << display name
         | (name, _uid) <- Users.enumerate users ]
    ]

userPage :: UserId -> UserName -> Resource.XHtml
userPage uid username = Resource.XHtml $
  concatHtml
    [ header
      << thetitle << ("Hackage: " ++ display username)

    , body
      << [ thediv << display username 
         , thediv << display username ]
    ]

{-
    [ path $ \userName -> msum  -- a specific member of the collection
        [ dir "enabled" $ msum
            [ GET -- bool
            , PUT -- bool
            ]
        
        , methodSP GET

        , methodSP PUT

        , methodSP DELETE $
        ]

    , --methodSP GET  -- list all users
    
    , dir "add" $ msum
        [ methodSP POST $ do
            reqData <- getDataFn lookUserNamePasswords
            case reqData of
              Nothing -> ok $ toResponse "try to fill out all the fields"
              Just (userName, pwd1, pwd2) ->

                  adminAddUser userName
                         (Auth.PasswdPlain pwd1) (Auth.PasswdPlain pwd2)
        ]
    , dir "change-password" $ msum
        [ methodSP POST $ do
            reqData <- getDataFn lookUserNamePasswords
            case reqData of
              Nothing -> ok $ toResponse "try to fill out all the fields"
              Just (userName, pwd1, pwd2) ->

                  adminChangePassword (Users.UserName userName)
                           (Auth.PasswdPlain pwd1) (Auth.PasswdPlain pwd2)
        ]
    -- , dir "disable" $ undefined
    -- , dir "enable" $ undefined
    -- , dir "delete" $ undefined
    , dir "toggle-admin" $ msum
        [ methodSP POST $ do
            reqData <- getDataFn $ do
              userName <- lookUserName
              makeAdmin <- lookRead "admin"
              return (userName, makeAdmin)

            case reqData of
              Nothing -> ok $ toResponse "Bad inputs, somehow"
              Just (userName, makeAdmin) ->
                  adminToggleAdmin (Users.UserName userName) makeAdmin
        ]
    ]
-}

{-

 where
   lookUserNamePasswords = do
     userName <- lookUserName
     pwd1 <- look "password"
     pwd2 <- look "repeat-password"
     return (userName, pwd1, pwd2)

   lookUserName = look "user-name"


data ChangePassword = ChangePassword { first, second :: String } deriving (Eq, Ord, Show)
instance FromData ChangePassword where
	fromData = liftM2 ChangePassword (look "new" `mplus` return "") (look "new2" `mplus` return "")

changePassword :: ServerPart Response
changePassword =
  methodSP POST $ do
    users <- query GetUsers
    uid <- Auth.hackageAuth users Nothing
    pwd <- getData >>= maybe (return $ ChangePassword "not" "valid") return
    if (first pwd == second pwd && first pwd /= "")
        then do let passwd = PasswdPlain (first pwd)
                auth <- newPasswd passwd
                res <- update $ ReplaceUserAuth uid auth
                if res
                 then ok $ toResponse "Password Changed"
                 else ok $ toResponse "Error changing password"
        else forbidden $ toResponse "Copies of new password do not match or is an invalid password (ex: blank)"


adminToggleAdmin :: Users.UserName -> Bool -> ServerPart Response
adminToggleAdmin userName makeAdmin
    = do
  mUser <- query $ LookupUserName userName

  if isNothing mUser then ok $ toResponse "Unknown user name" else do

  let Just user = mUser

  if makeAdmin
   then update $ AddToGroup Administrator user
   else update $ RemoveFromGroup Administrator user

  ok $ toResponse "Success!"

adminChangePassword
    :: Users.UserName -> Auth.PasswdPlain -> Auth.PasswdPlain
    -> ServerPart Response
adminChangePassword _ pwd1 pwd2
    | pwd1 /= pwd2
        = ok $ toResponse "Entered passwords do not match"
adminChangePassword userName password _
    = do

  mUser <- query $ LookupUserName userName

  case mUser of
    Nothing -> ok $ toResponse "Unknown user name"
    Just user ->
        do
          auth <- newPasswd password
          res <- update $ ReplaceUserAuth user auth

          ok $ toResponse $
           if res then "Success!"
           else "Failure!"


adminAddUser :: String -> Auth.PasswdPlain -> Auth.PasswdPlain
        -> ServerPart Response
adminAddUser _ pwd1 pwd2
    | pwd1 /= pwd2
        = ok $ toResponse "Entered passwords do not match"
adminAddUser userNameStr password _
    = case simpleParse userNameStr of
        Nothing -> ok $ toResponse "Not a valid user name!"
        Just userName
            -> do
          userAuth <- newPasswd password
          res <- update $ AddUser userName userAuth  

          case res of
            Nothing -> ok $ toResponse "Failed!"
            Just _  -> ok $ toResponse "Ok!"
          
newPasswd :: MonadIO m => Auth.PasswdPlain -> m Auth.PasswdHash
newPasswd pwd =
    do
      gen <- liftIO newStdGen
      return $ Auth.newPasswd gen pwd

-}
