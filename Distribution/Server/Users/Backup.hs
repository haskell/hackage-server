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
import Distribution.Server.Users.State (ReplaceUserDb(..))

import Data.Acid (AcidState, EventState, UpdateEvent, update)
import Data.Acid.Advanced (MethodResult)
import Data.ByteString.Lazy.Char8 (ByteString)
import Distribution.Server.Framework.BackupRestore
import Distribution.Text (display)
import Data.Version
import Data.Function (fix)
import Control.Monad.State (get, put)
import Text.CSV (CSV)
import qualified Data.IntSet as IntSet

-- Import for the user database
userBackup :: AcidState Users -> RestoreBackup
userBackup usersState = updateUserBackup usersState Users.empty

updateUserBackup :: AcidState Users -> Users -> RestoreBackup
updateUserBackup usersState users = RestoreBackup
  { restoreEntry = \entry -> do
        res <- doUserImport users entry
        return $ fmap (updateUserBackup usersState) res
  , restoreFinalize = return . Right $ updateUserBackup usersState users
  , restoreComplete = update usersState $ ReplaceUserDb users
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
    fromRecord [nameStr, idStr, statusStr, auth] = do
        name <- parseText "user name" nameStr
        user <- parseText "user id" idStr
        -- Legacy import: all hashes new hashes
        status <- parseStatus statusStr (NewUserAuth (PasswdHash auth))
        insertUser user $ UserInfo name status
    fromRecord [nameStr, idStr, statusStr, authNewOldStr, auth] = do
        name <- parseText "user name" nameStr
        user <- parseText "user id" idStr
        mkAuth <- parseNewOld authNewOldStr
        status <- parseStatus statusStr (mkAuth auth)
        insertUser user $ UserInfo name status

    fromRecord x = fail $ "Error processing auth record: " ++ show x

    parseNewOld "new" = return $ NewUserAuth . PasswdHash
    parseNewOld "old" = return $ OldUserAuth . HtPasswdHash
    parseNewOld no    = fail $ "unable to parse whether new or old hash: " ++ no

    parseStatus "deleted"    _    = return Deleted
    parseStatus "historical" _    = return Historical
    parseStatus "enabled"    auth = return $ Active Enabled  auth
    parseStatus "disabled"   auth = return $ Active Disabled auth
    parseStatus sts _ = fail $ "unable to parse whether user enabled: " ++ sts

insertUser :: UserId -> UserInfo -> Import Users ()
insertUser user info = do
    users <- get
    case Users.insert user info users of
        Left err     -> fail err
        Right users' -> put users'

-- Import for a single group
groupBackup :: (UpdateEvent event, MethodResult event ~ ())
            => AcidState (EventState event)
            -> [FilePath] -> (UserList -> event)
            -> RestoreBackup
groupBackup state csvPath updateFunc = updateGroupBackup Group.empty
  where
    updateGroupBackup group = fix $ \restorer -> RestoreBackup
              { restoreEntry = \entry -> do
                    if fst entry == csvPath
                        then fmap (fmap updateGroupBackup) $ runImport group (importGroup entry >>= put)
                        else return . Right $ restorer
              , restoreFinalize = return . Right $ restorer
              , restoreComplete = update state (updateFunc group)
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

