module Distribution.Server.Packages.Backup.Tags (
    tagsBackup,
    tagsToCSV,
    tagsToRecord
  ) where

import Data.Acid (AcidState, update)
import Distribution.Server.Packages.Tag
import Distribution.Server.Framework.BackupRestore

import Distribution.Package
import Distribution.Text (display)

import Text.CSV (CSV, Record)
import qualified Data.Map as Map
-- import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State (modify)
import Data.Function (fix)
import Data.ByteString.Lazy.Char8 (ByteString)

tagsBackup :: AcidState PackageTags -> RestoreBackup
tagsBackup tagsState = updateTags tagsState emptyPackageTags

updateTags :: AcidState PackageTags -> PackageTags -> RestoreBackup
updateTags tagsState tags = fix $ \r -> RestoreBackup
  { restoreEntry = \(entry, bs) -> do
        res <- runImport tags $ case entry of
            ["tags.csv"] -> importTags bs
            _ -> return ()
        return $ fmap (updateTags tagsState) res
  , restoreFinalize = return . Right $ r
  , restoreComplete = update tagsState $ ReplacePackageTags tags
  }

importTags :: ByteString -> Import PackageTags ()
importTags contents = importCSV "tags.csv" contents $ \csv ->
    mapM_ fromRecord csv
  where
    fromRecord (packageField:tagFields) | not (null tagFields) = do
        pkgname <- parseText "package name" packageField
        tags <- mapM (parseText "tag") tagFields
        modify $ setTags pkgname (Set.fromList tags)
    fromRecord x = fail $ "Invalid tags record: " ++ show x

------------------------------------------------------------------------------
tagsToCSV :: PackageTags -> CSV
tagsToCSV = map (\(p, t) -> tagsToRecord p $ Set.toList t)
          . Map.toList . packageTags

tagsToRecord :: PackageName -> [Tag] -> Record -- [String]
tagsToRecord pkgname tags = display pkgname:map display tags

