module Distribution.Server.Features.LegacyPasswds.Backup where

import qualified Distribution.Server.Features.LegacyPasswds.Acid as Acid

import Prelude hiding (abs)

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BackupRestore

import qualified Distribution.Server.Features.LegacyPasswds.Auth as LegacyAuth

import Distribution.Server.Users.Types

import qualified Data.IntMap as IntMap

import Distribution.Text (display)
import Data.Version
import Text.CSV (CSV, Record)

----------------------------
-- Data backup and restore
--

legacyPasswdsBackup :: RestoreBackup Acid.LegacyPasswdsTable
legacyPasswdsBackup = updatePasswdsBackup []

updatePasswdsBackup :: [(UserId, LegacyAuth.HtPasswdHash)] -> RestoreBackup Acid.LegacyPasswdsTable
updatePasswdsBackup upasswds = RestoreBackup {
    restoreEntry = \entry -> case entry of
      BackupByteString ["htpasswd.csv"] bs -> do
        when (not (null upasswds)) (fail "legacyPasswdsBackup: found multiple htpasswd.csv files")
        csv <- importCSV "htpasswd.csv" bs
        upasswds' <- importHtPasswds csv
        return (updatePasswdsBackup upasswds')
      _ ->
        return (updatePasswdsBackup upasswds)
  , restoreFinalize =
      let tbl =  IntMap.fromList [ (uid, htpasswd)
                                 | (UserId uid, htpasswd) <- upasswds ] in
      return $! Acid.LegacyPasswdsTable tbl
  }

importHtPasswds :: CSV -> Restore [(UserId, LegacyAuth.HtPasswdHash)]
importHtPasswds = mapM fromRecord . drop 2
  where
    fromRecord :: Record -> Restore (UserId, LegacyAuth.HtPasswdHash)
    fromRecord [idStr, htpasswdStr] = do
        uid <- parseText "user id" idStr
        return (uid, LegacyAuth.HtPasswdHash htpasswdStr)

    fromRecord x = fail $ "Error processing user details record: " ++ show x

legacyPasswdsToCSV :: BackupType -> Acid.LegacyPasswdsTable -> CSV
legacyPasswdsToCSV backuptype (Acid.LegacyPasswdsTable tbl)
    = ([showVersion version]:) $
      (headers:) $

      flip map (IntMap.toList tbl) $ \(uid, LegacyAuth.HtPasswdHash passwdhash) ->
      [ display (UserId uid)
      , if backuptype == FullBackup
        then passwdhash
        else ""
      ]
 where
    headers = ["uid", "htpasswd"]
    version = Version [0,1] []
