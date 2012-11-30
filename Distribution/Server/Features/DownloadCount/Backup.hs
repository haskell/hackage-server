module Distribution.Server.Features.DownloadCount.Backup (
    downloadsBackup,
    downloadsToCSV,
    downloadsToRecord
  ) where

import Distribution.Server.Framework.BackupRestore
import Data.Acid (AcidState, update)

import Distribution.Server.Features.DownloadCount.State

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

downloadsBackup :: AcidState DownloadCounts -> RestoreBackup
downloadsBackup downloadState = updateDownloads downloadState emptyDownloadCounts

updateDownloads :: AcidState DownloadCounts -> DownloadCounts -> RestoreBackup
updateDownloads downloadState dcs = fix $ \r -> RestoreBackup
  { restoreEntry = \entry -> do
        res <- runImport dcs $ case entry of
            BackupByteString ["downloads.csv"] bs -> importDownloads bs
            _ -> return ()
        return $ fmap (updateDownloads downloadState) res
  , restoreFinalize = return . Right $ r
  , restoreComplete = update downloadState $ ReplacePackageDownloads dcs
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
