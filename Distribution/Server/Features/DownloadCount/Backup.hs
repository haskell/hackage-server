module Distribution.Server.Features.DownloadCount.Backup (
    downloadsBackup,
    downloadsToCSV,
    downloadsToRecord
  ) where

import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BlobStorage (BlobStorage)
import Data.Acid (AcidState, update)

import Distribution.Server.Features.DownloadCount.State

import Distribution.Package
import Distribution.Text (display)
import Distribution.Version

import Text.CSV (CSV, Record)
import qualified Data.Map as Map
import Control.Monad
import Data.Time.Calendar

downloadsBackup :: BlobStorage -> AcidState DownloadCounts -> RestoreBackup
downloadsBackup store downloadState =
  fromPureRestoreBackup store
    (update downloadState . ReplacePackageDownloads)
    (updateDownloadsPure emptyDownloadCounts)

updateDownloadsPure :: DownloadCounts -> PureRestoreBackup DownloadCounts
updateDownloadsPure dcs = PureRestoreBackup {
    pureRestoreEntry = \(BackupByteString entry bs) ->
      if entry == ["downloads.csv"]
        then do csv  <- importCSV' "downloads.csv" bs
                dcs' <- updateFromCSV csv dcs
                return (updateDownloadsPure dcs')
        else return (updateDownloadsPure dcs)
  , pureRestoreFinalize = return dcs
  }

updateFromCSV :: CSV -> DownloadCounts -> Restore DownloadCounts
updateFromCSV = concatM . map fromRecord
  where
    fromRecord :: Record -> DownloadCounts -> Restore DownloadCounts
    fromRecord [dayField, packageNameField, packageVerField, countField] dcs = do
        day <- liftM ModifiedJulianDay $ parseRead "day" dayField
        pkgname <- parseText "package name" packageNameField
        pkgver <- parseText "package version" packageVerField
        count <- parseRead "day download count" countField
        return (incrementCounts day pkgname pkgver count dcs)
    fromRecord x _ = fail $ "Invalid tags record: " ++ show x

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
