module Distribution.Server.Features.Tags.Backup (
    tagsBackup,
    aliasBackup,
    tagsToCSV,
    aliasToCSV,
    tagsToRecord,
    aliasToRecord,
  ) where

import Distribution.Server.Features.Tags.State
import Distribution.Server.Framework.BackupRestore

import Distribution.Package
import Distribution.Text (display)

import Text.CSV (CSV, Record)
import qualified Data.Map as Map
-- import Data.Set (Set)
import qualified Data.Set as Set

tagsBackup :: RestoreBackup PackageTags
tagsBackup = updateTags emptyPackageTags

updateTags :: PackageTags -> RestoreBackup PackageTags
updateTags tagsState = RestoreBackup {
    restoreEntry = \(BackupByteString entry bs) ->
      if entry == ["tags.csv"]
        then do csv <- importCSV "tags.csv" bs
                tagsState' <- updateFromCSV csv tagsState
                return (updateTags tagsState')
        else return (updateTags tagsState)
  , restoreFinalize = return tagsState
  }
aliasBackup :: RestoreBackup TagAlias
aliasBackup = updateAlias emptyTagAlias

updateAlias :: TagAlias -> RestoreBackup TagAlias
updateAlias tagAliases = RestoreBackup {
    restoreEntry = \(BackupByteString entry bs) ->
      if entry == ["tagAlias.csv"]
        then do csv <- importCSV "tagAlias.csv" bs
                tagAliases' <- updateFromCSVA csv tagAliases
                return (updateAlias tagAliases')
        else return (updateAlias tagAliases)
  , restoreFinalize = return tagAliases
  }

updateFromCSV :: CSV -> PackageTags -> Restore PackageTags
updateFromCSV = concatM . map fromRecord
  where
    fromRecord :: Record -> PackageTags -> Restore PackageTags
    fromRecord (packageField:tagFields) tagsState | not (null tagFields) = do
      pkgname <- parseText "package name" packageField
      tags <- mapM (parseText "tag") tagFields
      return (setTags pkgname (Set.fromList tags) tagsState)
    fromRecord x _ = fail $ "Invalid tags record: " ++ show x

updateFromCSVA :: CSV -> TagAlias -> Restore TagAlias
updateFromCSVA = concatM . map fromRecord
  where
    fromRecord :: Record -> TagAlias -> Restore TagAlias
    fromRecord (canonical:aliases) tagsAlias | not (null aliases) = do
      tag <- parseText "tag" canonical
      alias <- mapM (parseText "tag") aliases
      return (setAliases tag (Set.fromList alias) tagsAlias)
    fromRecord x _ = fail $ "Invalid tags record: " ++ show x

------------------------------------------------------------------------------
tagsToCSV :: PackageTags -> CSV
tagsToCSV = map (\(p, t) -> tagsToRecord p $ Set.toList t)
          . Map.toList . packageTags

aliasToCSV :: TagAlias -> CSV
aliasToCSV (TagAlias ta) = map (\(t, a) -> aliasToRecord t $ Set.toList a) . Map.toList $ ta

tagsToRecord :: PackageName -> [Tag] -> Record -- [String]
tagsToRecord pkgname tags = display pkgname:map display tags

aliasToRecord :: Tag -> [Tag] -> Record -- [String]
aliasToRecord canonical aliases = display canonical:map display aliases
