module Distribution.Server.Users.ServerParts (
    admin,
    changePassword,
    newPasswd,
    guardAuth,
  ) where

import Distribution.Package ( PackageIdentifier(..), Package(packageId)
                            , packageName, packageVersion, PackageName(..) )
import Distribution.Text    (simpleParse, display)
import Happstack.Server hiding (port)
import qualified Happstack.Server
import Happstack.State hiding (Version)

import Distribution.Server.Users.State as State
import Distribution.Server.Packages.State as State
import qualified  Distribution.Server.State as State
import qualified Distribution.Server.Cache as Cache
import qualified Distribution.Simple.PackageIndex as PackageIndex
import qualified Distribution.Server.Auth.Basic as Auth
import qualified Distribution.Server.Auth.Types as Auth
import qualified Distribution.Server.Auth.Crypt as Auth
import Distribution.Server.Packages.Types
         ( PkgInfo(..) )
import qualified Distribution.Server.ResourceTypes as Resource
import qualified Distribution.Server.Pages.Index   as Pages (packageIndex)
import qualified Distribution.Server.Pages.Package as Pages
import qualified Distribution.Server.Pages.PackageAdmin as Pages
import qualified Distribution.Server.Pages.Recent  as Pages
import qualified Distribution.Server.Pages.BuildReports as Pages
import qualified Distribution.Server.Packages.Index as Packages.Index (write)
import qualified Distribution.Server.PackageUpload.Unpack as Upload (unpackPackage)
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import Distribution.Server.Util.BlobStorage (BlobStorage)
import Distribution.Server.Util.Serve (serveTarball)
import qualified Distribution.Server.BuildReport.BuildReport as BuildReport
import qualified Distribution.Server.BuildReport.BuildReports as BuildReports
import qualified Distribution.Server.BulkImport as BulkImport
import qualified Distribution.Server.BulkImport.UploadLog as UploadLog

import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Types as Users
import qualified Distribution.Server.Users.Group as Groups

import Distribution.Server.Auth.Types (PasswdPlain(..))


import System.FilePath ((</>))
import System.Directory
         ( createDirectoryIfMissing, doesDirectoryExist )
import System.Random (newStdGen)
import Control.Concurrent.MVar (MVar)
import Data.Maybe; import Data.Version
import Control.Monad.Trans
import Control.Monad (when,msum,mzero,liftM2,mplus)
import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Time.Clock
import Network.URI
         ( URIAuth(URIAuth) )
import Network.BSD
         ( getHostName )

import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import qualified Codec.Compression.GZip as GZip


data ChangePassword = ChangePassword { first, second :: String } deriving (Eq, Ord, Show)
instance FromData ChangePassword where
	fromData = liftM2 ChangePassword (look "new" `mplus` return "") (look "new2" `mplus` return "")

changePassword :: ServerPart Response
changePassword =
  methodSP POST $ do
    state <- query GetPackagesState
    let users = userDb state
    uid <- Auth.hackageAuth users Nothing
    let name = Users.idToName users uid
    pwd <- getData >>= maybe (return $ ChangePassword "not" "valid") return
    if (first pwd == second pwd && first pwd /= "")
        then do let passwd = PasswdPlain (first pwd)
                auth <- newPasswd passwd
                update $ ReplaceUserAuth uid auth
                ok $ toResponse "Password Changed"
        else forbidden $ toResponse "Copies of new password do not match or is an invalid password (ex: blank)"

{-
{-
/groups/pkg/packageName
/groups/admin
/groups/trustee
-}
groupInterface :: [ServerPart Response]
groupInterface =
    [ dir "pkg"
      [ path $ \pkgName -> groupMethods (PackageMaintainer (PackageName pkgName)) ]
    , dir "admin" (groupMethods Administrator)
    , dir "trustee" (groupMethods Trustee)
    ]
    where groupMethods groupName
              = [ methodSP GET $
                  do userGroup <- query $ LookupUserGroup groupName
                     userNames <- query $ ListGroupMembers userGroup
                     ok $ toResponse $ unlines (map display userNames)
                , methodSP PUT $ ...
                , methodSP DELETE $ ...
                ]
-}

guardAuth :: [GroupName] -> ServerPart ()
guardAuth gNames = do
  state <- query GetPackagesState
  group <- query $ LookupUserGroups gNames
  _ <- Auth.hackageAuth (userDb state) (Just group)
  return ()

-- Top level server part for administrative actions under the "admin"
-- directory
admin :: Cache.Cache -> URIAuth -> ServerPart Response
admin cache host = do

  guardAuth [Administrator]

  msum
   [ dir "users" (userAdmin cache host)
   ]

-- Assumes that the user has already been autheniticated
-- and has proper permissions
userAdmin :: Cache.Cache -> URIAuth -> ServerPart Response
userAdmin cache host
    = msum
      [ dir "add" $ msum
          [ methodSP POST $ do
              reqData <- getDataFn lookUserNamePasswords
              case reqData of
                Nothing -> ok $ toResponse "try to fill out all the fields"
                Just (userName, pwd1, pwd2) ->

                    adminAddUser cache host (Users.UserName userName)
                           (Auth.PasswdPlain pwd1) (Auth.PasswdPlain pwd2)
          ]
      , dir "change-password" $ msum
          [ methodSP POST $ do
              reqData <- getDataFn lookUserNamePasswords
              case reqData of
                Nothing -> ok $ toResponse "try to fill out all the fields"
                Just (userName, pwd1, pwd2) ->

                    adminChangePassword cache host (Users.UserName userName)
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
    :: Cache.Cache -> URIAuth
    -> Users.UserName -> Auth.PasswdPlain -> Auth.PasswdPlain
    -> ServerPart Response
adminChangePassword _ _ _ pwd1 pwd2
    | pwd1 /= pwd2
        = ok $ toResponse "Entered passwords do not match"
adminChangePassword cache host userName password _
    = do

  mUser <- query $ LookupUserName userName

  case mUser of
    Nothing -> ok $ toResponse "Unknown user name"
    Just user ->
        do
          auth <- newPasswd password
          result <- update $ ReplaceUserAuth user auth

          ok $ toResponse $
           if result then "Success!"
           else "Failure!"


adminAddUser :: Cache.Cache -> URIAuth
        -> Users.UserName -> Auth.PasswdPlain -> Auth.PasswdPlain
        -> ServerPart Response
adminAddUser _ _ _ pwd1 pwd2
    | pwd1 /= pwd2
        = ok $ toResponse "Entered passwords do not match"
adminAddUser cache host userName password _
    = do

  userAuth <- newPasswd password
  result <- update $ AddUser userName userAuth  

  case result of
    Nothing -> ok $ toResponse "Failed!"
    Just _  -> ok $ toResponse "Ok!"
          
newPasswd :: MonadIO m => Auth.PasswdPlain -> m Auth.PasswdHash
newPasswd pwd =
    do
      gen <- liftIO newStdGen
      return $ Auth.newPasswd gen pwd
