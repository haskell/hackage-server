module Distribution.Server.Users.UserBackup (
    -- Importing user data
    userBackup,
    importGroup,
    -- Exporting user data
    usersToCSV,
    groupToCSV
  ) where

import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Users (Users(..))
import Distribution.Server.Users.Group (UserList(..))
import Distribution.Server.Users.Types
import Distribution.Server.Auth.Types
import Distribution.Server.Users.State (ReplaceUserDb(..))

import Happstack.State (update)
import Data.ByteString.Lazy.Char8 (ByteString)
import Distribution.Server.Backup.Import
import Distribution.Text (display)
import Data.Version
import Control.Monad.State (get, put)
import Text.CSV
import qualified Data.IntSet as IntSet

{-importUsers :: [BackupEntry] -> IO (Maybe String)
importUsers (entry:entries) = case entry of
    [("auth.csv", bs)] -> do
        res <- runImport Users.empty (importAuth bs)
        case res of
            Right users -> update (ReplaceUserDb users) >> importUsers entries
            Left  e     -> return (Just e)
    _ -> importUsers entries-}

importGroup :: BackupEntry -> Import s UserList
importGroup (file, contents) = importCSV (last file) contents $ \csv ->
    fmap (UserList . IntSet.fromList) $ mapM parseUserId (concat csv)
  where
    parseUserId id = case reads id of
        [(num, "")] -> return num
        _ -> fail $ "Unable to parse user id : " ++ show id

--constructImportMap :: [HackageFeature] -> Map String RestoreBackup
--constructImportMap = Map.fromList . concatMap (\f -> [(featureName f, restoreBackup f)])

userBackup = updateUserBackup Users.empty

updateUserBackup :: Users -> RestoreBackup
updateUserBackup users = RestoreBackup
  { restoreEntry    = \entry -> do
        res <- doUserImport users entry
        return $ fmap updateUserBackup res
  , restoreComplete = update $ ReplaceUserDb users
  }

doUserImport :: Users -> BackupEntry -> IO (Either String Users)
doUserImport users (["auth.csv"], bs) = runImport users (importAuth bs)
doUserImport users _                  = return . Right $ users

importAuth :: ByteString -> Import Users ()
importAuth contents = importCSV "auth.csv" contents
                        $ \csv -> mapM_ fromRecord (drop 2 csv)

  where 
    fromRecord [nameStr, idStr, "deleted", "none", ""] = do
        name <- parse "user name" nameStr
        user <- parse "user id" idStr
        insertUser user $ UserInfo name Deleted

    fromRecord [nameStr, idStr, isEnabled, authType, auth] = do
        name <- parse "user name" nameStr
        user <- parse "user id" idStr
        authEn <- parseEnabled isEnabled
        atype <- parseAuth authType
        insertUser user $ UserInfo name (Active authEn $ UserAuth (PasswdHash auth) atype)

    fromRecord x = fail $ "Error processing auth record: " ++ show x

    parseEnabled "enabled"  = return Enabled
    parseEnabled "disabled" = return Disabled
    parseEnabled sts = fail $ "unable to parse whether user enabled: " ++ sts

    parseAuth "digest" = return DigestAuth
    parseAuth "basic"  = return BasicAuth
    parseAuth sts = fail $ "unable to parse auth status: " ++ sts

insertUser :: UserId -> UserInfo -> Import Users ()
insertUser user info = do
    users <- get
    case Users.insert user info users of
        Nothing     -> fail $ "Duplicate user id for user: " ++ display user
        Just users' -> put users

---------------- Exporting
-- group.csv
groupToCSV :: UserList -> CSV
groupToCSV (UserList list) = [map show (IntSet.toList list)]

-- auth.csv
{- | Produces a CSV file for the users DB.
   .
   Format:
   .
   User name,User Id,(enabled|disabled|deleted),(none|basic|digest),pwd-hash   
 -}
-- have a "safe" argument to this function that doesn't export password hashes?
usersToCSV :: Users -> CSV
usersToCSV users
    = ([showVersion userCSVVer]:) $
      (usersCSVKey:) $

      flip map (Users.enumerateAll users) $ \(user, userInfo) ->

      [ display . userName $ userInfo
      , display user
      , infoToStatus userInfo
      , infoToAuthType userInfo
      , infoToAuth userInfo
      ]

 where
    usersCSVKey =
       [ "name"
       , "id"
       , "status"
       , "auth-type"
       , "auth-info"
       ]
    userCSVVer = Version [0,1] ["unstable"]

    -- one of "enabled" "disabled" or "deleted"
    infoToStatus :: UserInfo -> String
    infoToStatus userInfo = case userStatus userInfo of
        Deleted  -> "deleted"
        Active Disabled _ -> "disabled"
        Active Enabled  _ -> "enabled"

    -- one of "none", "basic", or "digest"
    infoToAuthType :: UserInfo -> String
    infoToAuthType userInfo = case userStatus userInfo of
        Deleted -> "none"
        Active _ (UserAuth _ atype)-> case atype of
            BasicAuth -> "basic"
            DigestAuth -> "digest"

    -- may be null
    infoToAuth :: UserInfo -> String
    infoToAuth userInfo = case userStatus userInfo of
        Deleted{} -> ""
        Active _ (UserAuth (PasswdHash hash) _) -> hash

