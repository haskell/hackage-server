module Distribution.Server.Packages.PackageBackup (
    packageEntries,
    packagesBackup
  ) where

import Distribution.Server.Packages.State (PackagesState)
import Distribution.Server.Backup.Import

packageEntries :: PackagesState -> [BackupEntry]
packageEntries _ = []

packagesBackup :: RestoreBackup
packagesBackup = RestoreBackup
  { restoreEntry    = \_ -> return (Left "Not implemented")
  , restoreComplete = return ()
  }

