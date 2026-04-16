module Distribution.Server.Features.AdminLog.Backup where

import qualified Distribution.Server.Features.AdminLog.Acid as Acid
import Distribution.Server.Features.AdminLog.Types
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Framework.BackupRestore

import Data.Maybe(mapMaybe)
import Data.Time (UTCTime)
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.Read (readMaybe)
import Distribution.Server.Util.Parse

restoreAdminLogBackup :: RestoreBackup Acid.AdminLog
restoreAdminLogBackup =
    go (Acid.AdminLog [])
  where
    go logs =
      RestoreBackup {
        restoreEntry = \entry -> case entry of
                        BackupByteString ["adminLog.txt"] bs
                          -> return . go $ importLogs logs bs
                        _ -> return (go logs)
      , restoreFinalize = return logs
      }

importLogs :: Acid.AdminLog -> BS.ByteString -> Acid.AdminLog
importLogs (Acid.AdminLog ls) =
    Acid.AdminLog . (++ls) . mapMaybe fromRecord . lines . unpackUTF8
  where
    fromRecord :: String -> Maybe (UTCTime,UserId,AdminAction,BS.ByteString)
    fromRecord = readMaybe

backupLogEntries :: [(UTCTime,UserId,AdminAction,BS.ByteString)] -> BS.ByteString
backupLogEntries = packUTF8 . unlines . map show

