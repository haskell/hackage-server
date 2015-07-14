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
import Distribution.Server.Users.Users (Users)
import Distribution.Server.Users.UserIdSet (UserIdSet)
import qualified Distribution.Server.Users.UserIdSet as UserIdSet
import Distribution.Server.Users.Types
import qualified Distribution.Server.Framework.Auth as Auth

import Distribution.Server.Framework.BackupDump (BackupType(..))
import Distribution.Server.Framework.BackupRestore
import Distribution.Text (display)
import Data.Version
import Text.CSV (CSV, Record)


-- Import for the user database
userBackup :: RestoreBackup Users
userBackup = updateUserBackup Users.emptyUsers

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
    fromRecord [idStr, nameStr, "enabled", auth] users = do
        uid   <- parseText "user id"   idStr
        uname <- parseText "user name" nameStr
        let uauth = UserAuth (PasswdHash auth)
        insertUser users uid $ UserInfo uname (AccountEnabled uauth)
    fromRecord [idStr, nameStr, "disabled", auth] users = do
        uid   <- parseText "user id"   idStr
        uname <- parseText "user name" nameStr
        let uauth | null auth = Nothing
                  | otherwise = Just (UserAuth (PasswdHash auth))
        insertUser users uid $ UserInfo uname (AccountDisabled uauth)
    fromRecord [idStr, nameStr, "deleted", ""] users = do
        uid   <- parseText "user id"   idStr
        uname <- parseText "user name" nameStr
        insertUser users uid $ UserInfo uname AccountDeleted

    fromRecord x _ = fail $ "Error processing auth record: " ++ show x

insertUser :: Users -> UserId -> UserInfo -> Restore Users
insertUser users uid uinfo =
    case Users.insertUserAccount uid uinfo users of
        Left (Left Users.ErrUserIdClash)    -> fail $ "duplicate user id " ++ display uid
        Left (Right Users.ErrUserNameClash) -> fail $ "duplicate user name " ++ display (userName uinfo)
        Right users'                        -> return users'

-- Import for a single group
groupBackup :: [FilePath] -> RestoreBackup UserIdSet
groupBackup csvPath = updateGroupBackup UserIdSet.empty
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
importGroup :: CSV -> Restore UserIdSet
importGroup csv = do
    parsed <- mapM parseUserId (concat $ clean csv)
    return . UserIdSet.fromList $ parsed
  where
    clean xs = if all null xs then [] else xs
    parseUserId uid = case reads uid of
        [(num, "")] -> return num
        _ -> fail $ "Unable to parse user id : " ++ show uid

-------------------------------------------------- Exporting
-- group.csv
groupToCSV :: UserIdSet -> CSV
groupToCSV uidset = [map show (UserIdSet.toList uidset)]

-- auth.csv
{- | Produces a CSV file for the users DB.
   .
   Format:
   .
   User Id,User name,(enabled|disabled|deleted),pwd-hash
 -}
-- have a "safe" argument to this function that doesn't export password hashes?
usersToCSV :: BackupType -> Users -> CSV
usersToCSV backuptype users
    = ([showVersion userCSVVer]:) $
      (usersCSVKey:) $

      flip map (Users.enumerateAllUsers users) $ \(uid, uinfo) ->
      [ display uid
      , display (userName uinfo)
      , infoToStatus uinfo
      , if backuptype == FullBackup
        then infoToAuth uinfo
        else scrubbedAuth uinfo
      ]

 where
    usersCSVKey =
       [ "uid"
       , "name"
       , "status"
       , "auth-info"
       ]
    userCSVVer = Version [0,2] []

    scrubbedAuth :: UserInfo -> String
    scrubbedAuth userInfo = case userStatus userInfo of
      AccountEnabled        (UserAuth (PasswdHash _))  -> testHash userInfo
      AccountDisabled (Just (UserAuth (PasswdHash _))) -> testHash userInfo
      _                                                -> ""

    testHash :: UserInfo -> String
    testHash userInfo = case Auth.newPasswdHash Auth.hackageRealm
                             (userName userInfo) (PasswdPlain "test") of
                          PasswdHash pwd -> pwd

    -- one of "enabled" "disabled" or "deleted"
    infoToStatus :: UserInfo -> String
    infoToStatus userInfo = case userStatus userInfo of
        AccountEnabled  _ -> "enabled"
        AccountDisabled _ -> "disabled"
        AccountDeleted    -> "deleted"

    -- may be null
    infoToAuth :: UserInfo -> String
    infoToAuth userInfo = case userStatus userInfo of
        AccountEnabled        (UserAuth (PasswdHash hash))  -> hash
        AccountDisabled (Just (UserAuth (PasswdHash hash))) -> hash
        _                                                   -> ""

