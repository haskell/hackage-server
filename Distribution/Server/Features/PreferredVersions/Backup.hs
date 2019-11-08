{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module Distribution.Server.Features.PreferredVersions.Backup
  ( restorePreferredVersions
  , backupPreferredVersions
  ) where

import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Features.PreferredVersions.State
import Data.Version (Version(..), showVersion)
import Distribution.Text (display, simpleParse)
import Distribution.Package (PackageName)
import Distribution.Parsec (Parsec(..))
import Distribution.Version (VersionRange)
import qualified Data.Map as Map
import Control.Applicative ((<$>))
import Text.CSV (CSV, Record)
import Control.Monad (guard)

{-------------------------------------------------------------------------------
  Restore backup
-------------------------------------------------------------------------------}

restorePreferredVersions :: RestoreBackup PreferredVersions
restorePreferredVersions =
  updatePreferredVersions (initialPreferredVersions True)

updatePreferredVersions :: PreferredVersions -> RestoreBackup PreferredVersions
updatePreferredVersions st = RestoreBackup {
    restoreEntry    = \entry -> updatePreferredVersions <$> importEntry st entry
  , restoreFinalize = return st
  }

importEntry :: PreferredVersions -> BackupEntry -> Restore PreferredVersions
importEntry st (BackupByteString ["package", pkgstr, "preferred.csv"] bs) = do
  pkg <- parsePackageName pkgstr
  csv <- importCSV "preferred.csv" bs
  importPreferredCSV st pkg csv
importEntry st (BackupByteString ["package", pkgstr, "deprecated.csv"] bs) = do
  pkg <- parsePackageName pkgstr
  csv <- importCSV "deprecated.csv" bs
  importDeprecatedCSV st pkg csv
importEntry st _ = return st

importPreferredCSV :: PreferredVersions
                   -> PackageName
                   -> CSV
                   -> Restore PreferredVersions
importPreferredCSV st pkg ( _version
                          : (match "preferredRanges"    -> Just ranges)
                          : (match "deprecatedVersions" -> Just deprecated)
                          : (optionalSumRange           -> Just sumRange)
                          ) = do
  let info = PreferredInfo { preferredRanges    = ranges
                           , deprecatedVersions = deprecated
                           , sumRange           = sumRange
                           }
  return st { preferredMap = Map.insert pkg info (preferredMap st) }
importPreferredCSV _ _ _ = fail "Failed to read preferred.csv"

importDeprecatedCSV :: PreferredVersions
                    -> PackageName
                    -> CSV
                    -> Restore PreferredVersions
importDeprecatedCSV st pkg [ _version
                           , match "deprecatedFor" -> Just deprecatedFor
                           ] =
  return st { deprecatedMap = Map.insert pkg deprecatedFor (deprecatedMap st) }
importDeprecatedCSV _ _ _ = fail "Failed to read deprecated.csv"

match :: Parsec a => String -> Record -> Maybe [a]
match header (header' : xs) = guard (header == header') >> mapM simpleParse xs
match _ _ = Nothing

-- Outer maybe is Nothing on a parsing error; the inner maybe is because
-- the version range is optional
optionalSumRange :: CSV -> Maybe (Maybe VersionRange)
optionalSumRange [] = Just Nothing
optionalSumRange [["sumRange", simpleParse -> Just range]] = Just (Just range)
optionalSumRange _ = Nothing

parsePackageName :: String -> Restore PackageName
parsePackageName (simpleParse -> Just name) = return name
parsePackageName str = fail $ "Could not parse package name '" ++ str ++ "'"

{-------------------------------------------------------------------------------
  Create backup
-------------------------------------------------------------------------------}

backupPreferredVersions :: PreferredVersions -> [BackupEntry]
backupPreferredVersions (PreferredVersions preferredMap deprecatedMap _) =
     map backupPreferredInfo (Map.toList preferredMap)
  ++ map backupDeprecated (Map.toList deprecatedMap)

backupPreferredInfo :: (PackageName, PreferredInfo) -> BackupEntry
backupPreferredInfo (name, PreferredInfo {..}) =
    csvToBackup (pkgPath name "preferred.csv") $ [
        [showVersion versionCSV]
      , "preferredRanges" : map display preferredRanges
      , "deprecatedVersions" : map display deprecatedVersions
      ] ++ case sumRange of
             Nothing           -> []
             Just versionRange -> [["sumRange", display versionRange]]
  where
    versionCSV = Version [0,1] ["unstable"]

backupDeprecated :: (PackageName, [PackageName]) -> BackupEntry
backupDeprecated (name, deprecatedFor) =
    csvToBackup (pkgPath name "deprecated.csv") [
        [showVersion versionCSV]
      , "deprecatedFor" : map display deprecatedFor
      ]
  where
    versionCSV = Version [0,1] ["unstable"]

pkgPath :: PackageName -> String -> [String]
pkgPath pkgname file = ["package", display pkgname, file]
