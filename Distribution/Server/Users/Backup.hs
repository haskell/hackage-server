{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Distribution.Server.Users.Backup (
    -- Importing user data
    usersRestore,
    groupBackup,
    -- Exporting user data
    usersBackup,
    groupToCSV
  ) where

import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Users (Users)
import Distribution.Server.Users.UserIdSet (UserIdSet)
import qualified Distribution.Server.Users.UserIdSet as UserIdSet
import Distribution.Server.Users.Types
import qualified Distribution.Server.Framework.Auth as Auth

import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore
import Distribution.Text (display)
import Data.Version
import Text.CSV (CSV, Record)
import qualified Data.Map as M
import qualified Data.Text as T


-- Import for the user database
usersRestore :: RestoreBackup Users
usersRestore = updateUserBackup Users.emptyUsers

updateUserBackup :: Users -> RestoreBackup Users
updateUserBackup users = RestoreBackup {
    restoreEntry = \entry -> case entry of
      BackupByteString ["users.csv"] bs -> do
        csv <- importCSV "users.csv" bs
        users' <- importAuth csv users
        return (updateUserBackup users')
      BackupByteString ["authtokens.csv"] bs -> do
        csv <- importCSV "authtokens.csv" bs
        users' <- importAuthTokens csv users
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
        insertUser users uid $ UserInfo uname (AccountEnabled uauth) M.empty
    fromRecord [idStr, nameStr, "disabled", auth] users = do
        uid   <- parseText "user id"   idStr
        uname <- parseText "user name" nameStr
        let uauth | null auth = Nothing
                  | otherwise = Just (UserAuth (PasswdHash auth))
        insertUser users uid $ UserInfo uname (AccountDisabled uauth) M.empty
    fromRecord [idStr, nameStr, "deleted", ""] users = do
        uid   <- parseText "user id"   idStr
        uname <- parseText "user name" nameStr
        insertUser users uid $ UserInfo uname AccountDeleted M.empty

    fromRecord x _ = fail $ "Error processing auth record: " ++ show x

insertUser :: Users -> UserId -> UserInfo -> Restore Users
insertUser users uid uinfo =
    case Users.insertUserAccount uid uinfo users of
        Left (Left Users.ErrUserIdClash)    -> fail $ "duplicate user id " ++ display uid
        Left (Right Users.ErrUserNameClash) -> fail $ "duplicate user name " ++ display (userName uinfo)
        Right users'                        -> return users'

importAuthTokens :: CSV -> Users -> Restore Users
importAuthTokens = concatM . map fromRecord . drop 2
  where
    fromRecord :: Record -> Users -> Restore Users
    fromRecord [idStr, tokenStr, descr] users = do
        uid   <- parseText "user id"    idStr
        token <- parseText "auth token" tokenStr
        addAuthToken uid token (T.pack descr) users
    fromRecord x _ = fail $ "Error processing auth record: " ++ show x

    addAuthToken uid token descr users =
      case Users.addAuthToken uid token descr users of
        Right users' -> return users'
        Left Users.ErrNoSuchUserId ->
          fail $ "auth token for non-existant user id " ++ display uid

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


usersBackup :: BackupType -> Users -> [BackupEntry]
usersBackup backuptype users =
  [ csvToBackup ["users.csv"] (usersToCSV backuptype users)
  , csvToBackup ["authtokens.csv"] (authTokensToCSV backuptype users)
  ]

-- auth.csv
{- | Produces a CSV file for the users DB.
   .
   Format:
   .
   User Id,User name,(enabled|disabled|deleted),pwd-hash
 -}
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

-- authtokens.csv
{- | Produces a CSV file for the users auth tokens.
   .
   Format:
   .
   User Id,Token,Description
 -}
authTokensToCSV :: BackupType -> Users -> CSV
authTokensToCSV backuptype users =
    [ [showVersion (Version [0,1] [])]
    , [ "uid"
      , "token"
      , "description"
      ]
    ]
 ++ [ [ display uid
      , display token
      , T.unpack descr
      ]
    | backuptype == FullBackup
    , (uid, uinfo)   <- Users.enumerateAllUsers users
    , (token, descr) <- M.toList (userTokens uinfo) ]
