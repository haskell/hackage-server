module Distribution.Server.Packages.Backup.Downloads (
    downloadsBackup,
    downloadsToCSV,
    downloadsToRecord
  ) where

import Distribution.Server.Acid (update)
import Distribution.Server.Packages.Downloads
import Distribution.Server.Framework.BackupRestore

import Distribution.Package
import Distribution.Text (display)
import Distribution.Version

import Text.CSV (CSV, Record)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State (modify)
import Data.Function (fix)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Time.Calendar

downloadsBackup :: RestoreBackup
downloadsBackup = updateDownloads emptyDownloadCounts

updateDownloads :: DownloadCounts -> RestoreBackup
updateDownloads dcs = fix $ \r -> RestoreBackup
  { restoreEntry = \(entry, bs) -> do
        res <- runImport dcs $ case entry of
            ["downloads.csv"] -> importDownloads bs
            _ -> return ()
        return $ fmap updateDownloads res
  , restoreFinalize = return . Right $ r
  , restoreComplete = update $ ReplacePackageDownloads dcs
  }

importDownloads :: ByteString -> Import DownloadCounts ()
importDownloads contents = importCSV "downloads.csv" contents $ \csv ->
    mapM_ fromRecord csv
  where
    fromRecord [dayField, packageNameField, packageVerField, countField] = do
        day <- liftM ModifiedJulianDay $ parseRead "day" dayField
        pkgname <- parseText "package name" packageNameField
        pkgver <- parseText "package version" packageVerField
        count <- parseRead "day download count" countField
        modify $ incrementCounts day pkgname pkgver count
    fromRecord x = fail $ "Invalid tags record: " ++ show x

------------------------------------------------------------------------------
downloadsToCSV :: DownloadCounts -> CSV
downloadsToCSV dcs
  = [ downloadsToRecord day pkg_name pkg_ver count
    | (pkg_name, di) <- Map.toList (downloadMap dcs)
    , (day, pds) <- Map.toList (dayDownloads di)
    , (pkg_ver, count) <- Map.toList (versionDownloads pds)
    ]

downloadsToRecord :: Day -> PackageName -> Version -> Int -> Record -- [String]
downloadsToRecord day pkg_name pkg_ver count = [show (toModifiedJulianDay day), display pkg_name, display pkg_ver, show count]
