module Distribution.Server.Features.AdminLog.Backup where

import Distribution.Server.Features.AdminLog.Types
import qualified Distribution.Server.Features.AdminLog.State as State
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Users.Types (UserId)

import Data.Maybe(mapMaybe)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Time (UTCTime)
import Text.Read (readMaybe)
import Distribution.Server.Util.Parse

restoreAdminLogBackup :: RestoreBackup State.AdminLog
restoreAdminLogBackup =
    go (State.AdminLog [])
  where
    go logs =
      RestoreBackup {
        restoreEntry = \entry -> case entry of
                        BackupByteString ["adminLog.txt"] bs
                          -> return . go $ importLogs logs bs
                        _ -> return (go logs)
      , restoreFinalize = return logs
      }

importLogs :: State.AdminLog -> BS.ByteString -> State.AdminLog
importLogs (State.AdminLog ls) =
    State.AdminLog . (++ls) . mapMaybe fromRecord . lines . unpackUTF8
  where
    fromRecord :: String -> Maybe (UTCTime,UserId,AdminAction,BS.ByteString)
    fromRecord = readMaybe

backupLogEntries :: [(UTCTime,UserId,AdminAction,BS.ByteString)] -> BS.ByteString
backupLogEntries = packUTF8 . unlines . map show
