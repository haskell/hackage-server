module Distribution.Server.Features.UserDetails.Backup where

import qualified Distribution.Server.Features.UserDetails.Acid as Acid
import Distribution.Server.Features.UserDetails.Types
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore

import Distribution.Server.Users.Types

import qualified Data.IntMap as IntMap
import qualified Data.Text as T

import Distribution.Text (display)
import Data.Version
import Text.CSV (CSV, Record)

----------------------------
-- Data backup and restore
--

userDetailsBackup :: RestoreBackup Acid.UserDetailsTable
userDetailsBackup = updateUserBackup Acid.emptyUserDetailsTable

updateUserBackup :: Acid.UserDetailsTable -> RestoreBackup Acid.UserDetailsTable
updateUserBackup users = RestoreBackup {
    restoreEntry = \entry -> case entry of
      BackupByteString ["users.csv"] bs -> do
        csv <- importCSV "users.csv" bs
        users' <- importUserDetails csv users
        return (updateUserBackup users')
      _ ->
        return (updateUserBackup users)
  , restoreFinalize =
     return users
  }

importUserDetails :: CSV -> Acid.UserDetailsTable -> Restore Acid.UserDetailsTable
importUserDetails = concatM . map fromRecord . drop 2
  where
    fromRecord :: Record -> Acid.UserDetailsTable -> Restore Acid.UserDetailsTable
    fromRecord [idStr, nameStr, emailStr, kindStr, notesStr] (Acid.UserDetailsTable tbl) = do
        UserId uid <- parseText "user id" idStr
        akind      <- parseKind kindStr
        let udetails = AccountDetails {
                        accountName         = T.pack nameStr,
                        accountContactEmail = T.pack emailStr,
                        accountKind         = akind,
                        accountAdminNotes   = T.pack notesStr
                      }
        return $! Acid.UserDetailsTable (IntMap.insert uid udetails tbl)

    fromRecord x _ = fail $ "Error processing user details record: " ++ show x

    parseKind ""        = return Nothing
    parseKind "real"    = return (Just AccountKindRealUser)
    parseKind "special" = return (Just AccountKindSpecial)
    parseKind sts       = fail $ "unable to parse account kind: " ++ sts

userDetailsToCSV :: BackupType -> Acid.UserDetailsTable -> CSV
userDetailsToCSV backuptype (Acid.UserDetailsTable tbl)
    = ([showVersion userCSVVer]:) $
      (userdetailsCSVKey:) $

      flip map (IntMap.toList tbl) $ \(uid, udetails) ->
      [ display (UserId uid)
      , T.unpack (accountName udetails)  --FIXME: apparently the csv lib doesn't do unicode properly
      , if backuptype == FullBackup
        then T.unpack (accountContactEmail udetails)
        else "hidden-email@nowhere.org"
      , infoToAccountKind udetails
      , T.unpack (accountAdminNotes udetails)
      ]

 where
    userdetailsCSVKey =
       [ "uid"
       , "realname"
       , "email"
       , "kind"
       , "notes"
       ]
    userCSVVer = Version [0,2] []

    -- one of "enabled" "disabled" or "deleted"
    infoToAccountKind :: AccountDetails -> String
    infoToAccountKind udetails = case accountKind udetails of
      Nothing                  -> ""
      Just AccountKindRealUser -> "real"
      Just AccountKindSpecial  -> "special"

