{-# LANGUAGE PatternGuards #-}

module Distribution.Server.Features.Core.Backup (
    packagesBackup,
    indexToAllVersions,
    indexToAllVersions',
    indexToCurrentVersions,
    infoToAllEntries,
    infoToCurrentEntries,
    pkgPath
  ) where

import Distribution.Server.Features.Core.State

import Distribution.Server.Packages.Types
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Distribution.Package
import Distribution.PackageDescription.Parse (parsePackageDescription)
import Distribution.ParseUtils (ParseResult(..), locatedErrorMsg)
import Distribution.Text
import Data.Version
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Text.CSV

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Ord (comparing)
import Control.Monad.State
import qualified Codec.Compression.GZip as GZip

packagesBackup :: RestoreBackup PackagesState
packagesBackup = updatePackages Map.empty

updatePackages :: PartialIndex -> RestoreBackup PackagesState
updatePackages packageMap = RestoreBackup {
    restoreEntry = \entry -> do
      packageMap' <- doPackageImport packageMap entry
      return (updatePackages packageMap')
  , restoreFinalize = do
      results <- mapM partialToFullPkg (Map.toList packageMap)
      return $ PackagesState (PackageIndex.fromList results)
  }

type PartialIndex = Map PackageId PartialPkg

data PartialPkg = PartialPkg {
    partialCabal :: [(Int, CabalFileText)],
    partialCabalUpload :: [(Int, UploadInfo)],
    partialTarball :: [(Int, PkgTarball)],
    partialTarballUpload :: [(Int, UploadInfo)]
}

doPackageImport :: PartialIndex -> BackupEntry -> Restore PartialIndex
doPackageImport packages entry = case entry of
  BackupByteString ("package":pkgStr:rest) bs -> do
    pkgId <- parsePackageId pkgStr
    let partial = Map.findWithDefault emptyPartialPkg pkgId packages
    partial' <- case rest of
      ["uploads.csv"] -> do
        list <- importCSV "uploads.csv" bs >>= importVersionList
        return $ partial { partialCabalUpload = list }
      ["tarball.csv"] -> do
        list <- importCSV "tarball.csv" bs >>= importVersionList
        return $ partial { partialTarballUpload = list }
      [other] | Just version <- extractVersion other (packageName pkgId) ".cabal" ->
        return $ partial { partialCabal = (version, CabalFileText bs):partialCabal partial }
      _ -> return partial
    return (Map.insert pkgId partial' packages)
  BackupBlob ["package",pkgStr,other] blobId -> do
    pkgId <- parsePackageId pkgStr
    let partial = Map.findWithDefault emptyPartialPkg pkgId packages
    partial' <- case extractVersion other pkgId ".tar.gz" of
      Just version -> do
        bs <- restoreGetBlob blobId
        blobIdUncompressed <- restoreAddBlob $ GZip.decompress bs
        let tb = PkgTarball { pkgTarballGz = blobId,
                              pkgTarballNoGz = blobIdUncompressed }
        return $ partial { partialTarball = (version, tb):partialTarball partial }
      _ -> return partial
    return (Map.insert pkgId partial' packages)
  _ ->
    return packages
  where
    extractVersion name text ext = case stripPrefix (display text ++ ext) name of
      Just "" -> Just 0
      Just ('-':num) -> case reads num of
          [(version, "")] -> Just version
          _ -> Nothing
      _ -> Nothing

parsePackageId :: String -> Restore PackageId
parsePackageId pkgStr = case simpleParse pkgStr of
  Nothing    -> fail $ "Package directory " ++ show pkgStr ++ " isn't a valid package id"
  Just pkgId -> return pkgId

importVersionList :: CSV -> Restore [(Int, UploadInfo)]
importVersionList = mapM fromRecord . drop 2
  where
    fromRecord :: Record -> Restore (Int, UploadInfo)
    fromRecord [indexStr, timeStr, idStr] = do
       index <- parseRead "index" indexStr
       utcTime <- parseTime timeStr
       user <- parseText "user id" idStr
       return (index, (utcTime, user))
    fromRecord x = fail $ "Error processing versions list: " ++ show x

emptyPartialPkg :: PartialPkg
emptyPartialPkg = PartialPkg [] [] [] []

partialToFullPkg :: (PackageId, PartialPkg) -> Restore PkgInfo
partialToFullPkg (pkgId, partial) = do
    cabalDex   <- liftM2 (makeRecord $ "cabal file for " ++ display pkgId)
                         partialCabal partialCabalUpload partial
    tarballDex <- liftM2 (makeRecord $ "tarball for " ++ display pkgId)
                         partialTarball partialTarballUpload partial
    case descendUploadTimes cabalDex of
      [] -> fail $ "No cabal files found for " ++ display pkgId
      ((cabal, info):cabalOld) -> case parsePackageDescription (cabalFileString cabal) of
        ParseFailed err -> fail $ show (locatedErrorMsg err)
        ParseOk _ _ -> do
            return $ PkgInfo {
                pkgInfoId = pkgId,
                pkgData = cabal,
                pkgTarball = descendUploadTimes tarballDex,
                pkgDataOld = cabalOld,
                pkgUploadData = info
            }
  where
    makeRecord :: String -> [(Int, a)] -> [(Int, UploadInfo)] -> Restore [(a, UploadInfo)]
    makeRecord item list list' = makeRecord' item 0 (mergeBy (\(i, _) (i', _) -> compare i i')
                                                     (sortBy (comparing fst) list)
                                                     (sortBy (comparing fst) list'))

    -- (OnlyInLeft = no upload entry, OnlyInRight = no file), with checks for indexes
    makeRecord' _ _ [] = return []
    makeRecord' item index (InBoth x y:xs) = if fst x == index then fmap ((snd x, snd y):) (makeRecord' item (index+1) xs)
                                                               else fail $ "Missing index " ++ show (fst x-1) ++ "for " ++ item
    makeRecord' item _ (OnlyInLeft  x:_) = fail $ item ++ " (index "++ show (fst x)
                                               ++ ") found without matching upload log entry"
    makeRecord' item _ (OnlyInRight y:_) = fail $ "Upload log entry for " ++ item ++ " (index "
                                               ++ show (fst y) ++") found, but file itself missing"

--------------------------------------------------------------------------------
-- Every tarball and cabal file ever uploaded for every single package name and version
indexToAllVersions :: PackagesState -> [BackupEntry]
indexToAllVersions st =
    let pkgList = PackageIndex.allPackages . packageList $ st
    in concatMap infoToAllEntries pkgList

-- The most recent tarball and cabal file for every single package name and version
indexToAllVersions' :: PackagesState -> [BackupEntry]
indexToAllVersions' st =
    let pkgList = PackageIndex.allPackages . packageList $ st
    in concatMap infoToCurrentEntries pkgList

-- The most recent tarball and cabal file for the most recent version of every package
indexToCurrentVersions :: PackagesState -> [BackupEntry]
indexToCurrentVersions st =
    let pkgList = PackageIndex.allPackagesByName . packageList $ st
        pkgList' = map (maximumBy (comparing pkgUploadTime)) pkgList
    in concatMap infoToCurrentEntries pkgList'

-- it's also possible to make a cabal-only export

---------- Converting PkgInfo to entries
infoToAllEntries :: PkgInfo -> [BackupEntry]
infoToAllEntries pkg =
    let pkgId = pkgInfoId pkg
        cabals   = cabalListToExport pkgId $ ((pkgData pkg, pkgUploadData pkg):pkgDataOld pkg)
        tarballs = tarballListToExport pkgId (pkgTarball pkg)
    in cabals ++ tarballs

infoToCurrentEntries :: PkgInfo -> [BackupEntry]
infoToCurrentEntries pkg =
    let pkgId = pkgInfoId pkg
        cabals   = cabalListToExport pkgId [(pkgData pkg, pkgUploadData pkg)]
        tarballs = tarballListToExport pkgId (take 1 $ pkgTarball pkg)
    in cabals ++ tarballs

----------- Converting pieces of PkgInfo to entries
cabalListToExport :: PackageId -> [(CabalFileText, UploadInfo)] -> [BackupEntry]
cabalListToExport pkgId cabalInfos = csvToBackup (pkgPath pkgId "uploads.csv") (versionListToCSV infos):
    map cabalToExport (zip [0..] cabals)
  where (cabals, infos) = unzip cabalInfos
        cabalName = display (packageName pkgId) ++ ".cabal"
        cabalToExport :: (Int, CabalFileText) -> BackupEntry
        cabalToExport (0, CabalFileText bs) = BackupByteString (pkgPath pkgId cabalName) bs
        cabalToExport (n, CabalFileText bs) = BackupByteString (pkgPath pkgId (cabalName ++ "-" ++ show n)) bs

tarballListToExport :: PackageId -> [(PkgTarball, UploadInfo)] -> [BackupEntry]
tarballListToExport pkgId tarballInfos = csvToBackup (pkgPath pkgId "tarball.csv") (versionListToCSV infos):
    map tarballToExport (zip [0..] tarballs)
  where (tarballs, infos) = unzip tarballInfos
        tarballName = display pkgId ++ ".tar.gz"
        tarballToExport :: (Int, PkgTarball) -> BackupEntry
        tarballToExport (0, tb) = blobToBackup (pkgPath pkgId tarballName) (pkgTarballGz tb)
        tarballToExport (n, tb) = blobToBackup (pkgPath pkgId (tarballName ++ "-" ++ show n)) (pkgTarballGz tb)

pkgPath :: PackageId -> String -> [String]
pkgPath pkgId file = ["package", display pkgId, file]

versionListToCSV :: [UploadInfo] -> CSV
versionListToCSV infos = [showVersion versionCSVVer]:versionCSVKey:
    map (\(index, (time, user)) -> [ show (index :: Int)
                                   , formatTime defaultTimeLocale timeFormatSpec time
                                   , display user]) (zip [0..] infos)
  where
    versionCSVVer = Version [0,1] ["unstable"]
    versionCSVKey = ["index", "time", "user-id"]

