{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
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

import Distribution.Server.Framework.BackupRestore
import Distribution.Text (display)
import Data.Version
import Text.CSV (CSV, Record)
import qualified Data.IntSet as IntSet

-- Import for the user database
userBackup :: RestoreBackup Users
userBackup = updateUserBackup Users.empty

updateUserBackup :: Users -> RestoreBackup Users
updateUserBackup users = RestoreBackup {
    restoreEntry = \entry -> case entry of
      BackupByteString ["users.csv"] bs -> do
        csv <- importCSV "users.csv" bs
        users' <- importAuth csv users
        return (updateUserBackup users')
      _ ->
        return (updateUserBackup users)
  , restoreFinalize =
     return users
  }

importAuth :: CSV -> Users -> Restore Users
importAuth = concatM . map fromRecord . drop 2
  where
    fromRecord :: Record -> Users -> Restore Users
    fromRecord [nameStr, idStr, "deleted", "none", ""] users = do
        name <- parseText "user name" nameStr
        user <- parseText "user id" idStr
        insertUser users user $ UserInfo name Deleted
    fromRecord [nameStr, idStr, "historical", "none", ""] users = do
        name <- parseText "user name" nameStr
        user <- parseText "user id" idStr
        insertUser users user $ UserInfo name Historical
    fromRecord [nameStr, idStr, statusStr, auth] users = do
        name <- parseText "user name" nameStr
        user <- parseText "user id" idStr
        -- Legacy import: all hashes new hashes
        status <- parseStatus statusStr (NewUserAuth (PasswdHash auth))
        insertUser users user $ UserInfo name status
    fromRecord [nameStr, idStr, statusStr, authNewOldStr, auth] users = do
        name <- parseText "user name" nameStr
        user <- parseText "user id" idStr
        mkAuth <- parseNewOld authNewOldStr
        status <- parseStatus statusStr (mkAuth auth)
        insertUser users user $ UserInfo name status

    fromRecord x _ = fail $ "Error processing auth record: " ++ show x

    parseNewOld "new" = return $ NewUserAuth . PasswdHash
    parseNewOld "old" = return $ OldUserAuth . HtPasswdHash
    parseNewOld no    = fail $ "unable to parse whether new or old hash: " ++ no

    parseStatus "deleted"    _    = return Deleted
    parseStatus "historical" _    = return Historical
    parseStatus "enabled"    auth = return $ Active Enabled  auth
    parseStatus "disabled"   auth = return $ Active Disabled auth
    parseStatus sts _ = fail $ "unable to parse whether user enabled: " ++ sts

insertUser :: Users -> UserId -> UserInfo -> Restore Users
insertUser users user info =
    case Users.insert user info users of
        Left err     -> fail err
        Right users' -> return users'

-- Import for a single group
groupBackup :: [FilePath] -> RestoreBackup UserList
groupBackup csvPath = updateGroupBackup Group.empty
  where
    updateGroupBackup group = RestoreBackup {
        restoreEntry = \entry -> case entry of
          BackupByteString path bs | path == csvPath -> do
            csv    <- importCSV (last path) bs
            group' <- importGroup csv
            -- TODO: we just discard "group" here. Is that right?
            return (updateGroupBackup group')
          _ ->
            return (updateGroupBackup group)
      , restoreFinalize =
          return group
      }

-- parses a rather lax format. Any layout of integer ids separated by commas.
importGroup :: CSV -> Restore UserList
importGroup csv = do
    parsed <- mapM parseUserId (concat $ clean csv)
    return . UserList . IntSet.fromList $ parsed
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
   User name,User Id,(enabled|disabled|deleted),pwd-hash
 -}
-- have a "safe" argument to this function that doesn't export password hashes?
usersToCSV :: Users -> CSV
usersToCSV users
    = ([showVersion userCSVVer]:) $
      (usersCSVKey:) $

      flip map (Users.enumerateAll users) $ \(user, userInfo) ->
      let (authOldNew, auth) = infoToAuth userInfo in

      [ display . userName $ userInfo
      , display user
      , infoToStatus userInfo
      , authOldNew
      , auth
      ]

 where
    usersCSVKey =
       [ "name"
       , "id"
       , "status"
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

    -- may be null
    infoToAuth :: UserInfo -> (String, String)
    infoToAuth userInfo = case userStatus userInfo of
        Active _ (NewUserAuth (PasswdHash   hash)) -> ("new", hash)
        Active _ (OldUserAuth (HtPasswdHash hash)) -> ("old", hash)
        _ -> ("new", "")

