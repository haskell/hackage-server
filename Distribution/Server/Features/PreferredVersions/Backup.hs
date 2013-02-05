{-# LANGUAGE RecordWildCards #-}
module Distribution.Server.Features.PreferredVersions.Backup where

import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Features.PreferredVersions.State
import Data.Version (showVersion, Version(..))
import Distribution.Text (display)
import Distribution.Package (PackageName)
import qualified Data.Map as Map

{-------------------------------------------------------------------------------
  Create backup
-------------------------------------------------------------------------------}

backupPreferredVersions :: PreferredVersions -> [BackupEntry]
backupPreferredVersions (PreferredVersions preferredMap deprecatedMap) =
     map backupPreferredInfo (Map.toList preferredMap)
  ++ map backupDeprecated (Map.toList deprecatedMap)

backupPreferredInfo :: (PackageName, PreferredInfo) -> BackupEntry
backupPreferredInfo (name, PreferredInfo {..}) =
    csvToBackup (pkgPath name "preferred.csv") $ [
        [showVersion versionCSV]
      , "preferredRanges" : map display preferredRanges
      , "deprecatedVersions" : map showVersion deprecatedVersions
      ] ++ case sumRange of
             Nothing           -> []
             Just versionRange -> [["sumRange", display versionRange]]
  where
    versionCSV = Version [0,1] ["unstable"]

backupDeprecated :: (PackageName, [PackageName]) -> BackupEntry
backupDeprecated (name, deprecatedFor) =
    csvToBackup (pkgPath name "deprecated.csv") $ [
        [showVersion versionCSV]
      , "deprecatedFor" : map display deprecatedFor
      ]
  where
    versionCSV = Version [0,1] ["unstable"]

pkgPath :: PackageName -> String -> [String]
pkgPath pkgname file = ["package", display pkgname, file]

