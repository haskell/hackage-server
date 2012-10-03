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

import Distribution.Package
import Distribution.Text
import Data.Version
import Text.CSV

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet
import Control.Monad.State

-------------------------------------------------------------------------------
-- Maintainer groups backup
maintainerBackup :: AcidState PackageMaintainers -> RestoreBackup
maintainerBackup maintainersState =
  updateMaintainers maintainersState Map.empty

updateMaintainers :: AcidState PackageMaintainers
                  -> Map PackageName UserList -> RestoreBackup
updateMaintainers maintainersState mains = fix $ \r -> RestoreBackup
  { restoreEntry = \(entry, bs) -> do
        res <- runImport mains $ case entry of
            ["maintainers.csv"] -> importMaintainers bs
            _ -> return ()
        return $ fmap (updateMaintainers maintainersState) res
  , restoreFinalize = return . Right $ r
  , restoreComplete = update maintainersState $ ReplacePackageMaintainers (PackageMaintainers mains)
  }

importMaintainers :: ByteString -> Import (Map PackageName UserList) ()
importMaintainers contents = importCSV "maintainers.csv" contents $ \csvs -> do
    mapM_ fromRecord (drop 2 csvs)
  where
    fromRecord (packageStr:idStr) = do
        pkgname <- parseText "package name" packageStr
        ids <- mapM (parseRead "user id") idStr
        modify $ Map.insert pkgname (UserList $ IntSet.fromList ids)
    fromRecord x = fail $ "Invalid package maintainer record: " ++ show x

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

