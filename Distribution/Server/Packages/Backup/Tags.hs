module Distribution.Server.Packages.Backup.Tags (
    tagsBackup,
    tagsToCSV,
    tagsToRecord
  ) where

import Distribution.Server.Acid (update)
import Distribution.Server.Packages.Tag
import Distribution.Server.Backup.Import

import Distribution.Package
import Distribution.Text (display)

import Text.CSV (CSV, Record)
import qualified Data.Map as Map
-- import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State (modify)
import Data.Function (fix)
import Data.ByteString.Lazy.Char8 (ByteString)

tagsBackup :: RestoreBackup
tagsBackup = updateTags emptyPackageTags

updateTags :: PackageTags -> RestoreBackup
updateTags tags = fix $ \r -> RestoreBackup
  { restoreEntry = \(entry, bs) -> do
        res <- runImport tags $ case entry of
            ["tags.csv"] -> importTags bs
            _ -> return ()
        return $ fmap updateTags res
  , restoreFinalize = return . Right $ r
  , restoreComplete = update $ ReplacePackageTags tags
  }

importTags :: ByteString -> Import PackageTags ()
importTags contents = importCSV "tags.csv" contents $ \csv ->
    mapM_ fromRecord (drop 2 csv)
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

