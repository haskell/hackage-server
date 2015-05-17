{-# LANGUAGE PatternGuards #-}

module Distribution.Server.Features.Core.Backup (
    packagesBackup,
    indexToAllVersions,
    infoToAllEntries,
    pkgPath,
    PartialIndex(..),
    PartialPkg,
    partialToFullPkg,
    parsePackageId,
    doPackageImport
  ) where

import Distribution.Server.Features.Core.State
import Distribution.Server.Packages.Index

import Distribution.Server.Packages.Types
import Distribution.Server.Framework.BackupRestore
import Distribution.Server.Framework.BackupDump
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Distribution.Package
import Distribution.PackageDescription.Parse (parsePackageDescription)
import Distribution.ParseUtils (ParseResult(..), locatedErrorMsg)
import Distribution.Text
import Data.Version
import Text.CSV

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as Vec
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import Data.List
import Data.Ord (comparing)
import Control.Monad.State
import qualified Distribution.Server.Util.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import System.FilePath ((</>))

packagesBackup :: RestoreBackup PackagesState
packagesBackup = updatePackages (PartialIndex Map.empty Nothing)

updatePackages :: PartialIndex -> RestoreBackup PackagesState
updatePackages accum@(PartialIndex packageMap updatelog) = RestoreBackup {
    restoreEntry = \entry -> do
      accum' <- doPackageImport accum entry
      return (updatePackages accum')
  , restoreFinalize = do
      results <- mapM partialToFullPkg (Map.toList packageMap)
      return $ PackagesState (PackageIndex.fromList results) updatelog
  }

data PartialIndex = PartialIndex !(Map PackageId PartialPkg)
                                 !(Maybe (Seq TarIndexEntry))

data PartialPkg = PartialPkg {
    partialCabal :: [(Int, CabalFileText)],
    partialCabalUpload :: [(Int, UploadInfo)],
    partialTarball :: [(Int, PkgTarball)],
    partialTarballUpload :: [(Int, UploadInfo)]
}

doPackageImport :: PartialIndex -> BackupEntry -> Restore PartialIndex
doPackageImport (PartialIndex packages updatelog) entry = case entry of
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
    return $! PartialIndex (Map.insert pkgId partial' packages) updatelog
  BackupBlob filename@["package",pkgStr,other] blobId -> do
    pkgId <- parsePackageId pkgStr
    let partial = Map.findWithDefault emptyPartialPkg pkgId packages
    partial' <- case extractVersion other pkgId ".tar.gz" of
      Just version -> do
        bs <- restoreGetBlob blobId
        blobIdUncompressed <- restoreAddBlob $ GZip.decompressNamed (foldr1 (</>) filename) (forceLast bs)
        let tb = PkgTarball { pkgTarballGz = blobId,
                              pkgTarballNoGz = blobIdUncompressed }
        return $ partial { partialTarball = (version, tb):partialTarball partial }
      _ -> return partial
    return $! PartialIndex (Map.insert pkgId partial' packages) updatelog
  BackupByteString ["updatelog.csv"] bs -> do
    updatelog' <- importCSV "updatelog.csv" bs >>= importTarIndexEntries
    return $! PartialIndex packages (Just updatelog')
  _ ->
    return (PartialIndex packages updatelog)
  where
    extractVersion name text ext = case stripPrefix (display text ++ ext) name of
      Just "" -> Just 0
      Just ('-':num) -> case reads num of
          [(version, "")] -> Just version
          _ -> Nothing
      _ -> Nothing

    -- Workaround: in zlib prior to 0.5.4.1, GZip.decompress would not fully
    -- consume the input data (because the gzip format means it knows when
    -- it has got to the end of the expected data). As a consequence the bs
    -- we get from restoreGetBlob would not have its file handle closed.
    forceLast = BS.fromChunks . forceLastBlock . BS.toChunks
    forceLastBlock []     = []
    forceLastBlock (c:[]) = c : []
    forceLastBlock (c:cs) = c : forceLastBlock cs

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
       utcTime <- parseUTCTime "time" timeStr
       user <- parseText "user-id" idStr
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
    let cabalRevisions   = sortByUploadTimes cabalDex
        tarballRevisions = sortByUploadTimes tarballDex

    when (null cabalRevisions) $
      fail $ "No cabal files found for " ++ display pkgId

    let (latestCabalFile, _) = last cabalRevisions
    case parsePackageDescription (cabalFileString latestCabalFile) of
      ParseFailed err -> fail $ show (locatedErrorMsg err)
      ParseOk _ _     -> return ()

    return PkgInfo {
      pkgInfoId            = pkgId,
      pkgMetadataRevisions = Vec.fromList cabalRevisions,
      pkgTarballRevisions  = Vec.fromList tarballRevisions
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

    sortByUploadTimes :: [(a, UploadInfo)] -> [(a, UploadInfo)]
    sortByUploadTimes = sortBy (comparing (fst . snd))


--------------------------------------------------------------------------------
-- Every tarball and cabal file ever uploaded for every single package name and version
indexToAllVersions :: PackagesState -> [BackupEntry]
indexToAllVersions st =
    let pkgList = PackageIndex.allPackages . packageIndex $ st
    in maybe id (\l x -> packageUpdateLogToExport l : x) (packageUpdateLog st) $
       concatMap infoToAllEntries pkgList

---------- Converting PkgInfo to entries
infoToAllEntries :: PkgInfo -> [BackupEntry]
infoToAllEntries pkg =
    let pkgId = pkgInfoId pkg
        cabals   = cabalListToExport   pkgId (Vec.toList (pkgMetadataRevisions pkg))
        tarballs = tarballListToExport pkgId (Vec.toList (pkgTarballRevisions pkg))
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
                                   , formatUTCTime time
                                   , display user]) (zip [0..] infos)
  where
    versionCSVVer = Version [0,1] ["unstable"]
    versionCSVKey = ["index", "time", "user-id"]

----------- Converting TarIndexEntry to and from entries

packageUpdateLogToExport :: Seq TarIndexEntry -> BackupEntry
packageUpdateLogToExport = csvToBackup ["updatelog.csv"]
                         . packageUpdateLogToCSV

packageUpdateLogToCSV :: Seq TarIndexEntry -> CSV
packageUpdateLogToCSV updlog =
    [showVersion versionCSVVer] : versionCSVKey : map entryToCSV (Foldable.toList updlog)
  where
    versionCSVVer = Version [0,1] []
    versionCSVKey = ["time", "pkgid", "revno", "user-name", "extrapath", "extracontent"]
    entryToCSV (PackageEntry pkgid revno time username) =
      [ formatUTCTime time
      , display pkgid
      , show revno
      , display username
      , ""
      , ""
      ]
    entryToCSV (ExtraEntry extrapath content time) =
      [ formatUTCTime time
      , ""
      , ""
      , ""
      , extrapath
      , BSC.unpack content
      ]

importTarIndexEntries :: CSV -> Restore (Seq TarIndexEntry)
importTarIndexEntries = fmap Seq.fromList . mapM fromRecord . drop 2
  where
    fromRecord :: Record -> Restore TarIndexEntry
    fromRecord [timeStr, pkgidStr, revnoStr, usernameStr, "", ""] = do
       utcTime  <- parseUTCTime "time" timeStr
       pkgid    <- parseText "pkgid" pkgidStr
       revno    <- parseRead "revno" revnoStr
       username <- parseText "user-name" usernameStr
       return (PackageEntry pkgid revno utcTime username)

    fromRecord [timeStr, "", "", "", extrapath, extracontent] = do
       utcTime <- parseUTCTime "time" timeStr
       return (ExtraEntry extrapath (BSC.pack extracontent) utcTime)

    fromRecord x = fail $ "Error index entries list: " ++ show x

