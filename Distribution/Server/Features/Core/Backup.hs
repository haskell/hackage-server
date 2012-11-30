{-# LANGUAGE PatternGuards #-}

module Distribution.Server.Features.Core.Backup (
    packagesBackup,
    indexToAllVersions,
    indexToAllVersions',
    indexToCurrentVersions,
    infoToAllEntries,
    infoToCurrentEntries,
  ) where

import Data.Acid (AcidState, update)

import Distribution.Server.Features.Core.State

import Distribution.Server.Packages.Types
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Framework.BlobStorage (BlobStorage)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Distribution.Package
import Distribution.PackageDescription.Parse (parsePackageDescription)
import Distribution.ParseUtils (ParseResult(..), locatedErrorMsg)
import Distribution.Text
import Data.Version
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Text.CSV

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Ord (comparing)
import Data.Monoid (mempty)
import Control.Monad.State
import qualified Codec.Compression.GZip as GZip

packagesBackup :: AcidState PackagesState -> BlobStorage -> RestoreBackup
packagesBackup packagesState blobs = updatePackageBackup packagesState blobs Map.empty

type PartialIndex = Map PackageId PartialPkg

updatePackageBackup :: AcidState PackagesState -> BlobStorage -> PartialIndex -> RestoreBackup
updatePackageBackup packagesState storage packageMap = RestoreBackup
  { restoreEntry    = \entry -> do
        res <- doPackageImport storage packageMap entry
        return $ fmap (updatePackageBackup packagesState storage) res
  , restoreFinalize =
        let results = mapM partialToFullPkg (Map.toList packageMap)
        in return $ fmap (finalPackagesBackup packagesState . PackagesState . PackageIndex.fromList) results
  , restoreComplete = return ()
  }

finalPackagesBackup :: AcidState PackagesState -> PackagesState -> RestoreBackup
finalPackagesBackup packagesState packages = mempty {
    restoreComplete = update packagesState $ ReplacePackagesState packages
  }


data PartialPkg = PartialPkg {
    partialCabal :: [(Int, CabalFileText)],
    partialCabalUpload :: [(Int, UploadInfo)],
    partialTarball :: [(Int, PkgTarball)],
    partialTarballUpload :: [(Int, UploadInfo)]
}
emptyPartialPkg :: PartialPkg
emptyPartialPkg = PartialPkg [] [] [] []

partialToFullPkg :: (PackageId, PartialPkg) -> Either String PkgInfo
partialToFullPkg (pkgId, partial) = do
    cabalDex   <- liftM2 (makeRecord $ "cabal file for " ++ display pkgId)
                         partialCabal partialCabalUpload partial
    tarballDex <- liftM2 (makeRecord $ "tarball for " ++ display pkgId)
                         partialTarball partialTarballUpload partial
    case shiftUploadTimes (descendUploadTimes cabalDex) of
      [] -> Left $ "No cabal files found for " ++ display pkgId
      ((cabal, info):cabalOld) -> case parsePackageDescription (cabalFileString cabal) of
        ParseFailed err -> Left $ show (locatedErrorMsg err)
        ParseOk _ _ -> do
            return $ PkgInfo {
                pkgInfoId = pkgId,
                pkgData = cabal,
                pkgTarball = descendUploadTimes tarballDex,
                pkgDataOld = cabalOld,
                pkgUploadData = info
            }
  where
    makeRecord :: String -> [(Int, a)] -> [(Int, UploadInfo)] -> Either String [(a, UploadInfo)]
    makeRecord item list list' = makeRecord' item 0 (mergeBy (\(i, _) (i', _) -> compare i i')
                                                     (sortBy (comparing fst) list)
                                                     (sortBy (comparing fst) list'))
    -- (OnlyInLeft = no upload entry, OnlyInRight = no file), with checks for indexes
    makeRecord' _ _ [] = Right []
    makeRecord' item index (InBoth x y:xs) = if fst x == index then fmap ((snd x, snd y):) (makeRecord' item (index+1) xs)
                                                               else Left $ "Missing index " ++ show (fst x-1) ++ "for " ++ item
    makeRecord' item _ (OnlyInLeft  x:_) = Left $ item ++ " (index "++ show (fst x)
                                               ++ ") found without matching upload log entry"
    makeRecord' item _ (OnlyInRight y:_) = Left $ "Upload log entry for " ++ item ++ " (index "
                                               ++ show (fst y) ++") found, but file itself missing"

--from data/upload-time format to pkgInfo format (on import)
shiftUploadTimes :: [(a, b)] -> [(a, b)]
shiftUploadTimes [] = []
shiftUploadTimes times@((cabal', _):_) = case go times of (shifted, info') -> ((cabal', info'):shifted)
    where go ((_, info):xs@((cabal, _):_)) = case go xs of (xs', info') -> ((cabal, info):xs', info') -- not a tail recursive 'go'
          go [(_, info')] = ([], info')
          go [] = undefined

-- from pkgInfo formats to data/upload-time format (on export, and maybe for displaying to a web interface)
-- if a non-empty list is passed in, a non-empty list /will/ be passed out
unshiftUploadTimes :: [(a, b)] -> [(a, b)]
unshiftUploadTimes [] = []
unshiftUploadTimes times@((_, info'):_) = go times
    where go ((cabal, _):xs@((_, info):_)) = (cabal, info):go xs
          go [(cabal', _)] = [(cabal', info')]
          go [] = undefined
{-
How data is stored in PkgInfo:
[(data1, upload1), (data2, upload2)]
current: (data0, upload0)
How it is correlated in the tarball:
data0 - upload1 (upload time of data0 is stored with the data that replaced it, in this case data1)
data1 - upload2
data2 - upload0 (upload time of earliest version is at top)
-}

-- instead of keeping the PartialPkgs around for a long time, there could be package data
-- tarballs within the backup tarball, which are read one at a time (and so we can see if
-- import fails /during/ import rather than during restoreFinalize)
doPackageImport :: BlobStorage -> PartialIndex -> BackupEntry -> IO (Either String PartialIndex)
doPackageImport storage packages entry = runImport packages $ case entry of
  BackupByteString ("package":pkgStr:rest) bs -> case simpleParse pkgStr of
    Nothing    -> fail $ "Package directory " ++ show pkgStr ++ " isn't a valid package id"
    Just pkgId -> do
        partial  <- gets (Map.findWithDefault emptyPartialPkg pkgId)
        partial' <- case rest of
            ["uploads.csv"] -> importVersionList "uploads.csv" bs >>= \list ->
                        return $ partial { partialCabalUpload = list }
            ["tarball.csv"] -> importVersionList "tarball.csv" bs >>= \list ->
                        return $ partial {  partialTarballUpload = list }
            [other] | Just version <- extractVersion other (packageName pkgId) ".cabal" ->
                        return $ partial { partialCabal = (version, CabalFileText bs):partialCabal partial }
            _ -> return partial
        -- TODO: if we have both uploads.csv and tarball.csv, it should be possible to convert to PkgInfo
        -- before the finalization, reducing total memory usage -- the PkgInfos will still be in memory when
        -- we're done, but the PartialPkgs will need to be garbage collected. Doing it along the way will
        -- make GHC less heap-hungry.
        modify (Map.insert pkgId partial')
  BackupBlob ("package":pkgStr:rest) blobId -> case simpleParse pkgStr of
    Nothing    -> fail $ "Package directory " ++ show pkgStr ++ " isn't a valid package id"
    Just pkgId -> do
        partial  <- gets (Map.findWithDefault emptyPartialPkg pkgId)
        partial' <- case rest of
            [other] | Just version <- extractVersion other pkgId ".tar.gz" -> do
                        bs <- liftIO $ BlobStorage.fetch storage blobId
                        blobIdUncompressed <- liftIO $ BlobStorage.add storage (GZip.decompress bs)
                        let tb = PkgTarball { pkgTarballGz = blobId,
                                              pkgTarballNoGz = blobIdUncompressed }
                        return $ partial { partialTarball = (version, tb):partialTarball partial }
            _ -> return partial
        modify (Map.insert pkgId partial')
  _ ->
    return ()
  where
    extractVersion name text ext = case stripPrefix (display text ++ ext) name of
      Just "" -> Just 0
      Just ('-':num) -> case reads num of
          [(version, "")] -> Just version
          _ -> Nothing
      _ -> Nothing

importVersionList :: FilePath -> ByteString -> Import s [(Int, UploadInfo)]
importVersionList name contents = importCSV name contents $ mapM fromRecord . drop 2
  where
    fromRecord [indexStr, timeStr, idStr] = do
         index <- parseRead "index" indexStr
         utcTime <- parseTime timeStr
         user <- parseText "user id" idStr
         return (index, (utcTime, user))
    fromRecord x = fail $ "Error processing versions list: " ++ show x

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
        cabals   = cabalListToExport pkgId $ unshiftUploadTimes ((pkgData pkg, pkgUploadData pkg):pkgDataOld pkg)
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
cabalListToExport pkgId cabalInfos = csvToBackup (pkgPath ++ ["uploads.csv"]) (versionListToCSV infos):
    map cabalToExport (zip [0..] cabals)
  where (cabals, infos) = unzip cabalInfos
        cabalName = display (packageName pkgId) ++ ".cabal"
        cabalToExport :: (Int, CabalFileText) -> BackupEntry
        cabalToExport (0, CabalFileText bs) = BackupByteString (pkgPath ++ [cabalName]) bs
        cabalToExport (n, CabalFileText bs) = BackupByteString (pkgPath ++ [cabalName ++ "-" ++ show n]) bs
        pkgPath = ["package", display pkgId]

tarballListToExport :: PackageId -> [(PkgTarball, UploadInfo)] -> [BackupEntry]
tarballListToExport pkgId tarballInfos = csvToBackup (pkgPath ++ ["tarball.csv"]) (versionListToCSV infos):
    map tarballToExport (zip [0..] tarballs)
  where (tarballs, infos) = unzip tarballInfos
        tarballName = display pkgId ++ ".tar.gz"
        tarballToExport :: (Int, PkgTarball) -> BackupEntry
        tarballToExport (0, tb) = blobToBackup (pkgPath ++ [tarballName]) (pkgTarballGz tb)
        tarballToExport (n, tb) = blobToBackup (pkgPath ++ [tarballName ++ "-" ++ show n]) (pkgTarballGz tb)
        pkgPath = ["package", display pkgId]

versionListToCSV :: [UploadInfo] -> CSV
versionListToCSV infos = [showVersion versionCSVVer]:versionCSVKey:
    map (\(index, (time, user)) -> [ show (index :: Int)
                                   , formatTime defaultTimeLocale timeFormatSpec time
                                   , display user]) (zip [0..] infos)
  where
    versionCSVVer = Version [0,1] ["unstable"]
    versionCSVKey = ["index", "time", "user-id"]

