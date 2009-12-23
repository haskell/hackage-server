module Distribution.Server.Users.ServerParts (
    userAdmin,
    changePassword,
    newPasswd,
  ) where

import Happstack.Server hiding (port)
import qualified Happstack.Server
import Happstack.State hiding (Version)

import Distribution.Server.Users.State as State
import Distribution.Server.Packages.State as State
import qualified Distribution.Server.Cache as Cache
import qualified Distribution.Server.Auth.Basic as Auth
import qualified Distribution.Server.Auth.Types as Auth
import qualified Distribution.Server.Auth.Crypt as Auth

import qualified Distribution.Server.Users.Types as Users
import Distribution.Server.Users.Permissions(GroupName(..))
import Distribution.Server.Auth.Types (PasswdPlain(..))
import Distribution.Server.Export.ServerParts (export)
import Distribution.Server.Util.BlobStorage (BlobStorage)

import Distribution.Text (simpleParse)

import System.Random (newStdGen)
import Data.Maybe
import Control.Monad.Trans
import Control.Monad (msum,liftM2,mplus)
import Network.URI
         ( URIAuth )


data ChangePassword = ChangePassword { first, second :: String } deriving (Eq, Ord, Show)
instance FromData ChangePassword where
	fromData = liftM2 ChangePassword (look "new" `mplus` return "") (look "new2" `mplus` return "")

changePassword :: ServerPart Response
changePassword =
  methodSP POST $ do
    state <- query GetPackagesState
    let users = userDb state
    uid <- Auth.hackageAuth users Nothing
    pwd <- getData >>= maybe (return $ ChangePassword "not" "valid") return
    if (first pwd == second pwd && first pwd /= "")
        then do let passwd = PasswdPlain (first pwd)
                auth <- newPasswd passwd
                update $ ReplaceUserAuth uid auth
                ok $ toResponse "Password Changed"
        else forbidden $ toResponse "Copies of new password do not match or is an invalid password (ex: blank)"


-- Assumes that the user has already been autheniticated
-- and has proper permissions
userAdmin :: ServerPart Response
userAdmin
    = msum
      [ dir "add" $ msum
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

 where lookUserNamePasswords = do
         userName <- lookUserName
         pwd1 <- look "password"
         pwd2 <- look "repeat-password"
         return (userName, pwd1, pwd2)

       lookUserName = look "user-name"

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

