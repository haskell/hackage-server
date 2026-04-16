{-# LANGUAGE RecordWildCards #-}

module Distribution.Server.Features.UserSignup.Backup where

import Distribution.Server.Features.UserSignup.Types
import qualified Distribution.Server.Features.UserSignup.Acid as Acid

import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore

import Distribution.Server.Util.Nonce

import qualified Data.Map as Map
import qualified Data.Text as T

import Distribution.Text (display)
import Text.CSV (CSV, Record)

----------------------------
-- Data backup and restore
--

signupResetBackup :: RestoreBackup Acid.SignupResetTable
signupResetBackup = go []
  where
   go :: [(Nonce, SignupResetInfo)] -> RestoreBackup Acid.SignupResetTable
   go st =
     RestoreBackup {
       restoreEntry = \entry -> case entry of
         BackupByteString ["signups.csv"] bs -> do
           csv <- importCSV "signups.csv" bs
           signups <- importSignupInfo csv
           return (go (signups ++ st))

         BackupByteString ["resets.csv"] bs -> do
           csv <- importCSV "resets.csv" bs
           resets <- importResetInfo csv
           return (go (resets ++ st))

         _ -> return (go st)

     , restoreFinalize =
        return (Acid.SignupResetTable (Map.fromList st))
     }

importSignupInfo :: CSV -> Restore [(Nonce, SignupResetInfo)]
importSignupInfo = mapM fromRecord . drop 2
  where
    fromRecord :: Record -> Restore (Nonce, SignupResetInfo)
    fromRecord [nonceStr, usernameStr, realnameStr, emailStr, timestampStr] = do
        timestamp <- parseUTCTime "timestamp" timestampStr
        nonce <- parseNonceM nonceStr
        let signupinfo = SignupInfo {
              signupUserName     = T.pack usernameStr,
              signupRealName     = T.pack realnameStr,
              signupContactEmail = T.pack emailStr,
              nonceTimestamp     = timestamp
            }
        return (nonce, signupinfo)
    fromRecord x = fail $ "Error processing signup info record: " ++ show x

signupInfoToCSV :: BackupType -> Acid.SignupResetTable -> CSV
signupInfoToCSV backuptype (Acid.SignupResetTable tbl)
    = ["0.1"]
    : [ "token", "username", "realname", "email", "timestamp" ]
    : [ [ if backuptype == FullBackup
          then renderNonce nonce
          else ""
        , T.unpack signupUserName
        , T.unpack signupRealName
        , if backuptype == FullBackup
          then T.unpack signupContactEmail
          else "hidden-email@nowhere.org"
        , formatUTCTime nonceTimestamp
        ]
      | (nonce, SignupInfo{..}) <- Map.toList tbl ]

importResetInfo :: CSV -> Restore [(Nonce, SignupResetInfo)]
importResetInfo = mapM fromRecord . drop 2
  where
    fromRecord :: Record -> Restore (Nonce, SignupResetInfo)
    fromRecord [nonceStr, useridStr, timestampStr] = do
        userid <- parseText "userid" useridStr
        timestamp <- parseUTCTime "timestamp" timestampStr
        nonce <- parseNonceM nonceStr
        let signupinfo = ResetInfo {
              resetUserId    = userid,
              nonceTimestamp = timestamp
            }
        return (nonce, signupinfo)
    fromRecord x = fail $ "Error processing signup info record: " ++ show x

resetInfoToCSV :: BackupType -> Acid.SignupResetTable -> CSV
resetInfoToCSV backuptype (Acid.SignupResetTable tbl)
    = ["0.1"]
    : [ "token", "userid", "timestamp" ]
    : [ [ if backuptype == FullBackup
          then renderNonce nonce
          else ""
        , display resetUserId
        , formatUTCTime nonceTimestamp
        ]
      | (nonce, ResetInfo{..}) <- Map.toList tbl ]

