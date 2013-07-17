module Distribution.Server.Features.DownloadCount.Backup {- (
    onDiskBackup
  , onDiskRestore
  , inMemBackup
  , inMemRestore
  ) -} where

import Prelude hiding (init)

import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump

import Distribution.Server.Features.DownloadCount.State

import Distribution.Package
import Distribution.Text (display, simpleParse)
import Distribution.Version

import Text.CSV (CSV, Record)
import qualified Data.Map as Map
import Control.Monad
import Data.Time.Calendar

import Distribution.Server.Util.CountingMap

onDiskBackup :: OnDiskStats -> [BackupEntry]
onDiskBackup = undefined

onDiskRestore :: RestoreBackup OnDiskStats
onDiskRestore = importOne "ondisk.csv" cmFromCSV

inMemBackup :: InMemStats -> [BackupEntry]
inMemBackup (InMemStats day inMemStats) =
  [csvToBackup ["inmem.csv"] $
      [display versionCSV]
    : [display day]
    : cmToCSV inMemStats
  ]
  where
    versionCSV = Version [0,1] ["unstable"]

inMemRestore :: RestoreBackup InMemStats
inMemRestore = importOne "inmem.csv" importInMemStats

importInMemStats :: Monad m => CSV -> m InMemStats
importInMemStats (_version : [dayStr] : inMemStatsCSV) = do
  day <- case simpleParse dayStr of
           Just day -> return day
           Nothing  -> fail "importInMemStats: Invalid day"
  inMemStats <- cmFromCSV inMemStatsCSV
  return (InMemStats day inMemStats)
importInMemStats _ =
  fail "Invalid format for inmem.csv"

{-
onDiskBackup :: OnDiskStats -> [BackupEntry]
onDiskBackup dc = [csvToBackup ["ondisk.csv"] $ csv]
  where
    version = Version [0,1] []
    csv     = [display version] : pvpdToCSV (onDiskToPVPD dc)

inMemBackup :: InMemStats -> [BackupEntry]
inMemBackup dc = [csvToBackup ["inmem.csv"] $ csv]
  where
    version = Version [0,1] []
    csv     = [display version] : pvpdToCSV (inMemToPVPD dc)

onDiskRestore :: RestoreBackup OnDiskStats
onDiskRestore = restorePVPD "ondisk.csv" initOnDiskStats pvpdToOnDisk

inMemRestore :: RestoreBackup InMemStats
inMemRestore = restorePVPD "inmem.csv" (initInMemStats initDay) pvpdToInMem
  where
    initDay = error "Day not set. inmem.csv should have at least one entry"

{------------------------------------------------------------------------------
  We store the CSV files with one line per package version/day pair
------------------------------------------------------------------------------}

type PVPD = [(Day, PackageName, Version, Int)]

pvpdToCSV :: PVPD -> CSV
pvpdToCSV = map toRecord
  where
    toRecord :: (Day, PackageName, Version, Int) -> Record
    toRecord (day, pkg_name, pkg_ver, count) = [
        showDay day
      , display pkg_name
      , display pkg_ver
      , show count
      ]

csvToPVPD:: CSV -> Restore PVPD
csvToPVPD = mapM fromRecord
  where
    fromRecord :: Record -> Restore (Day, PackageName, Version, Int)
    fromRecord [dayField, packageNameField, packageVerField, countField] = do
      day     <- parseDay  "day"                dayField
      pkgname <- parseText "package name"       packageNameField
      pkgver  <- parseText "package version"    packageVerField
      count   <- parseRead "day download count" countField
      return (day, pkgname, pkgver, count)
    fromRecord x = fail $ "csvToPVPD: Invalid record: " ++ show x

onDiskToPVPD :: OnDiskStats -> PVPD
onDiskToPVPD dcs = [
    (day, pkg_name, pkg_ver, count)
  | (pkg_name, di)   <- Map.toList (downloadMap dcs)
  , (day, pds)       <- Map.toList (dayDownloads di)
  , (pkg_ver, count) <- Map.toList (versionDownloads pds)
  ]

inMemToPVPD :: InMemStats -> PVPD
inMemToPVPD (InMemStats day counts) = [
    (day, pkgName pkgId, pkgVersion pkgId, count)
  | (pkgId, count) <- Map.toList counts
  ]

pvpdToOnDisk :: Record -> PVPD -> OnDiskStats -> Restore OnDiskStats
pvpdToOnDisk [_version] pvpd stats =
    return $ foldr (.) id (map aux pvpd) stats
  where
    aux (day, pkgname, pkgver, count) = incrementCounts day pkgname pkgver count
pvpdToOnDisk _ _ _ =
  fail "pvpdToOnDisk: unrecognized header"

pvpdToInMem :: Record -> PVPD -> InMemStats -> Restore InMemStats
pvpdToInMem [_version, dayString] pvpd stats = do
    day <- parseDay "day" dayString
    return $ foldr (.) id (map (aux day) pvpd) stats
  where
    aux (_day, pkgname, pkgver, count) (InMemStats day counts) =
      let pkgId   = PackageIdentifier pkgname pkgver
          counts' = adjustFrom (+ count) pkgId 0 counts
      in InMemStats day counts'
pvpdToInMem _ _ _ =
  fail "pvpdToInMem: unrecognized header"

restorePVPD :: FilePath -> st -> (Record -> PVPD -> st -> Restore st) -> RestoreBackup st
restorePVPD filename init update = go init
  where
    go dcs = RestoreBackup {
         restoreEntry = \(BackupByteString entry bs) ->
           if entry == [filename]
             then do header:csv <- importCSV filename bs
                     pvpd       <- csvToPVPD csv
                     dcs'       <- update header pvpd dcs
                     return (go dcs')
             else return (go dcs)
       , restoreFinalize = return dcs
       }

{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

showDay :: Day -> String
showDay = show . toModifiedJulianDay

parseDay :: String -> String -> Restore Day
parseDay label field = liftM ModifiedJulianDay $ parseRead label field
-}

{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

-- TODO: should probably move this to the RestoreBackup module and use it
-- elsewhere too
importOne :: String -> (CSV -> Restore a) -> RestoreBackup a
importOne name importA = aux Nothing
  where
    aux ma = RestoreBackup {
        restoreEntry = \entry -> case entry of
          BackupByteString name' bs | name' == [name] -> do
            csv <- importCSV name bs
            a   <- importA csv
            return $ aux (Just a)
          _ ->
            return $ aux ma
      , restoreFinalize = case ma of
          Just a  -> return a
          Nothing -> fail $ "Missing " ++ name
      }
