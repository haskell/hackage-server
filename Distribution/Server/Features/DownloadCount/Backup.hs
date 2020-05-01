module Distribution.Server.Features.DownloadCount.Backup (
    onDiskBackup
  , onDiskRestore
  , inMemBackup
  , inMemRestore
  ) where

import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Features.DownloadCount.State
import Distribution.Server.Util.CountingMap
import Distribution.Text (display, simpleParse)
import Data.Version
import Text.CSV (CSV)

onDiskBackup :: OnDiskStats -> [BackupEntry]
onDiskBackup onDisk = [csvToBackup ["ondisk.csv"] $ cmToCSV onDisk]

onDiskRestore :: RestoreBackup OnDiskStats
onDiskRestore = importOne "ondisk.csv" cmFromCSV

inMemBackup :: InMemStats -> [BackupEntry]
inMemBackup (InMemStats day inMemStats) =
  [csvToBackup ["inmem.csv"] $
      [showVersion versionCSV]
    : [display day]
    : cmToCSV inMemStats
  ]
  where
    versionCSV = Version [0,1] ["unstable"]

inMemRestore :: RestoreBackup InMemStats
inMemRestore = importOne "inmem.csv" importInMemStats

importInMemStats :: Monad m => CSV -> m InMemStats
importInMemStats (_version : [dayStr] : inMemStatsCSV) = do
  day <- case simpleParse dayStr of
           Just day -> return day
           Nothing  -> fail "importInMemStats: Invalid day"
  inMemStats <- cmFromCSV inMemStatsCSV
  return (InMemStats day inMemStats)
importInMemStats _ =
  fail "Invalid format for inmem.csv"

{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

-- TODO: should probably move this to the RestoreBackup module and use it
-- elsewhere too
importOne :: String -> (CSV -> Restore a) -> RestoreBackup a
importOne name importA = aux Nothing
  where
    aux ma = RestoreBackup {
        restoreEntry = \entry -> case entry of
          BackupByteString name' bs | name' == [name] -> do
            csv <- importCSV name bs
            a   <- importA csv
            return $ aux (Just a)
          _ ->
            return $ aux ma
      , restoreFinalize = case ma of
          Just a  -> return a
          Nothing -> fail $ "Missing " ++ name
      }
