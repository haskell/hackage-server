{-# LANGUAGE FlexibleContexts #-}
module Distribution.Server.Users.Backup (
    -- Importing user data
    userBackup,
    importGroup,
    groupBackup,
    -- Exporting user data
    usersToCSV,
    groupToCSV
  ) where

import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Users (Users(..))
import Distribution.Server.Users.Group (UserList(..))
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Types
import Distribution.Server.Users.State (ReplaceUserDb(..))

import Happstack.State (update, UpdateEvent)
import Data.ByteString.Lazy.Char8 (ByteString)
import Distribution.Server.Backup.Import
import Distribution.Text (display)
import Data.Version
import Data.Function (fix)
import Control.Monad.State (get, put)
import Text.CSV (CSV)
import qualified Data.IntSet as IntSet

-- Import for the user database
userBackup :: RestoreBackup
userBackup = updateUserBackup Users.empty

updateUserBackup :: Users -> RestoreBackup
updateUserBackup users = RestoreBackup
  { restoreEntry = \entry -> do
        res <- doUserImport users entry
        return $ fmap updateUserBackup res
  , restoreFinalize = return . Right $ updateUserBackup users
  , restoreComplete = update $ ReplaceUserDb users
  }

doUserImport :: Users -> BackupEntry -> IO (Either String Users)
doUserImport users (["users.csv"], bs) = runImport users (importAuth bs)
doUserImport users _ = return . Right $ users

importAuth :: ByteString -> Import Users ()
importAuth contents = importCSV "users.csv" contents $ \csv -> mapM_ fromRecord (drop 2 csv)
  where
    fromRecord [nameStr, idStr, "deleted", "none", ""] = do
        name <- parseText "user name" nameStr
        user <- parseText "user id" idStr
        insertUser user $ UserInfo name Deleted
    fromRecord [nameStr, idStr, "historical", "none", ""] = do
        name <- parseText "user name" nameStr
        user <- parseText "user id" idStr
        insertUser user $ UserInfo name Historical
    fromRecord [nameStr, idStr, isEnabled, authType, auth] = do
        name <- parseText "user name" nameStr
        user <- parseText "user id" idStr
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
        Just users' -> put users'

-- Import for a single group
groupBackup :: UpdateEvent a () => [FilePath] -> (UserList -> a)
                                -> RestoreBackup
groupBackup csvPath updateFunc = updateGroupBackup Group.empty
  where
    updateGroupBackup group = fix $ \restorer -> RestoreBackup
              { restoreEntry = \entry -> do
                    if fst entry == csvPath
                        then fmap (fmap updateGroupBackup) $ runImport group (importGroup entry >>= put)
                        else return . Right $ restorer
              , restoreFinalize = return . Right $ restorer
              , restoreComplete = update (updateFunc group)
              }

-- parses a rather lax format. Any layout of integer ids separated by commas.
importGroup :: BackupEntry -> Import s UserList
importGroup (file, contents) = importCSV (last file) contents $ \vals ->
    fmap (UserList . IntSet.fromList) $ mapM parseUserId (concat $ clean vals)
  where
    clean xs = if all null xs then [] else xs
    parseUserId uid = case reads uid of
        [(num, "")] -> return num
        _ -> fail $ "Unable to parse user id : " ++ show uid


-------------------------------------------------- Exporting
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
        Historical -> "historical"
        Active Disabled _ -> "disabled"
        Active Enabled  _ -> "enabled"

    -- one of "none", "basic", or "digest"
    infoToAuthType :: UserInfo -> String
    infoToAuthType userInfo = case userStatus userInfo of
        Active _ (UserAuth _ atype)-> case atype of
            BasicAuth -> "basic"
            DigestAuth -> "digest"
        _ -> "none"

    -- may be null
    infoToAuth :: UserInfo -> String
    infoToAuth userInfo = case userStatus userInfo of
        Active _ (UserAuth (PasswdHash hash) _) -> hash
        _ -> ""

