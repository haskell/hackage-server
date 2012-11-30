{-# LANGUAGE PatternGuards #-}

module Distribution.Server.Features.Upload.Backup (
    maintainerBackup,
    maintToExport,
    maintToCSV
  ) where

import Data.Acid (AcidState, update)

import Distribution.Server.Features.Upload.State

import Distribution.Server.Users.Group (UserList(..))
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BlobStorage (BlobStorage)

import Distribution.Package
import Distribution.Text
import Data.Version
import Text.CSV (CSV, Record)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet

-------------------------------------------------------------------------------
-- Maintainer groups backup
maintainerBackup :: BlobStorage -> AcidState PackageMaintainers -> RestoreBackup
maintainerBackup store maintainersState =
  fromPureRestoreBackup store
    (update maintainersState . ReplacePackageMaintainers)
    (updateMaintainersPure Map.empty)

updateMaintainersPure :: Map PackageName UserList -> PureRestoreBackup PackageMaintainers
updateMaintainersPure mains = PureRestoreBackup {
    pureRestoreEntry = \entry -> do
      case entry of
        BackupByteString ["maintainers.csv"] bs -> do
          csv    <- importCSV' "maintainers.csv" bs
          mains' <- importMaintainers csv mains
          return (updateMaintainersPure mains')
        _ ->
          return (updateMaintainersPure mains)
  , pureRestoreFinalize =
      return $ PackageMaintainers (mains)
  }

importMaintainers :: CSV -> Map PackageName UserList -> Restore (Map PackageName UserList)
importMaintainers = concatM . map fromRecord . drop 2
  where
    fromRecord :: Record -> Map PackageName UserList -> Restore (Map PackageName UserList)
    fromRecord (packageStr:idStr) mains = do
        pkgname <- parseText "package name" packageStr
        ids <- mapM (parseRead "user id") idStr
        return (Map.insert pkgname (UserList $ IntSet.fromList ids) mains)
    fromRecord x _ = fail $ "Invalid package maintainer record: " ++ show x

maintToExport :: Map PackageName UserList -> BackupEntry
maintToExport pkgmap = csvToBackup ["maintainers.csv"] (maintToCSV assocUsers)
  where assocUsers = map (\(name, UserList ul) -> (name, IntSet.toList ul))
                   $ Map.toList pkgmap

maintToCSV :: [(PackageName, [Int])] -> CSV
maintToCSV users = [showVersion pkgCSVVer]:pkgCSVKey:
    map (\(name, ids) -> display name:map show ids) users
  where
    pkgCSVKey = ["package", "maintainers"]
    pkgCSVVer = Version [0,1] ["unstable"]

