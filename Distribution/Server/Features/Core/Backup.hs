{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE RecordWildCards #-}
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
import Distribution.Server.Framework.BlobStorage (BlobId, blobMd5)
import Distribution.Server.Users.Types (UserName(..))
import Distribution.Server.Features.Security.SHA256
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Distribution.Package
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec (showPError)
import Distribution.Text
import Data.Version (Version(..), showVersion)
import Text.CSV

import Control.Arrow (first)
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
      return $ PackagesState (PackageIndex.fromList results)
                             (maybe (Left []) Right updatelog)
  }

data PartialIndex = PartialIndex !(Map PackageId PartialPkg)
                                 !(Maybe (Seq TarIndexEntry))

data PartialPkg = PartialPkg {
    partialCabal         :: [(Int, CabalFileText)],
    partialCabalUpload   :: [(Int, UploadInfo)],
    partialTarball       :: [(Int, (FilePath, BlobId))],
    partialTarballUpload :: [(Int, (UploadInfo, Maybe TarballInfo))]
}

data TarballInfo = TarballInfo {
    infoTarGzMD5    :: BlobId
  , infoTarGzLength :: Int
  , infoTarGzSHA256 :: SHA256Digest
  , infoTarMD5      :: BlobId
  }

pkgTarballToInfo :: PkgTarball -> TarballInfo
pkgTarballToInfo tarball = TarballInfo {
    infoTarGzMD5    = blobInfoId         . pkgTarballGz   $ tarball
  , infoTarGzLength = blobInfoLength     . pkgTarballGz   $ tarball
  , infoTarGzSHA256 = blobInfoHashSHA256 . pkgTarballGz   $ tarball
  , infoTarMD5      =                      pkgTarballNoGz $ tarball
  }

doPackageImport :: PartialIndex -> BackupEntry -> Restore PartialIndex
doPackageImport (PartialIndex packages updatelog) entry = case entry of
  BackupByteString fp@("package":pkgStr:rest) bs -> do
    pkgId <- parsePackageId pkgStr
    let partial = Map.findWithDefault emptyPartialPkg pkgId packages
    partial' <- case rest of
      ["uploads.csv"] -> do
        list <- importCSV "uploads.csv" bs >>= importCabalMetadata fp
        return $ partial { partialCabalUpload = list }
      ["tarball.csv"] -> do
        list <- importCSV "tarball.csv" bs >>= importTarballMetadata fp
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
        return $ partial {
            partialTarball = (version, (foldr1 (</>) filename, blobId))
                           : partialTarball partial
         }
      _ ->
       -- Unrecognized file. Ignore
       return partial
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

parsePackageId :: String -> Restore PackageId
parsePackageId pkgStr = case simpleParse pkgStr of
  Nothing    -> fail $ "Package directory " ++ show pkgStr ++ " isn't a valid package id"
  Just pkgId -> return pkgId

importCabalMetadata :: [String] -> CSV -> Restore [(Int, UploadInfo)]
importCabalMetadata _fp (_versionStr:_headers:body) =
    mapM fromRecord body
  where
    fromRecord :: Record -> Restore (Int, UploadInfo)
    fromRecord [strIndex, strTime, strUser] = do
       index   <- parseRead    "index"   strIndex
       utcTime <- parseUTCTime "time"    strTime
       user    <- parseText    "user-id" strUser
       return (index, (utcTime, user))
    fromRecord x = fail $ "Error processing versions list: " ++ show x
importCabalMetadata fp _ =
    fail $ "Invalid cabal metadata in " ++ foldr1 (</>) fp

importTarballMetadata :: [String] -> CSV -> Restore [(Int, (UploadInfo, Maybe TarballInfo))]
importTarballMetadata _fp ([strVersion]:_headers:body) = do
    version <- parseVersion "CSV version header" strVersion
    if version >= Version [0,2] []
      then mapM fromRecord_v2 body
      else mapM fromRecord_v1 body
  where
    fromRecord_v2 :: Record -> Restore (Int, (UploadInfo, Maybe TarballInfo))
    fromRecord_v2 [strIndex, strTime, strUser, strTarGzMD5, strTarGzLen, strTarGzSHA256, strTarMD5] = do
       index   <- parseRead    "index"   strIndex
       utcTime <- parseUTCTime "time"    strTime
       user    <- parseText    "user-id" strUser
       infoTarGzMD5    <- parseBlobId "tar-gz-md5"    strTarGzMD5
       infoTarGzLength <- parseRead   "tar-gz-len"    strTarGzLen
       infoTarGzSHA256 <- parseSHA    "tar-gz-sha256" strTarGzSHA256
       infoTarMD5      <- parseBlobId "tar-md5"       strTarMD5
       return (index, ((utcTime, user), Just TarballInfo{..}))
    fromRecord_v2 x = fail $ "Error processing versions list: " ++ show x

    fromRecord_v1 :: Record -> Restore (Int, (UploadInfo, Maybe TarballInfo))
    fromRecord_v1 [strIndex, strTime, strUser] = do
       index   <- parseRead    "index"   strIndex
       utcTime <- parseUTCTime "time"    strTime
       user    <- parseText    "user-id" strUser
       return (index, ((utcTime, user), Nothing))
    fromRecord_v1 x = fail $ "Error processing versions list: " ++ show x
importTarballMetadata fp _ =
    fail $ "Invalid tarball metadata in " ++ foldr1 (</>) fp

emptyPartialPkg :: PartialPkg
emptyPartialPkg = PartialPkg [] [] [] []

partialToFullPkg :: (PackageId, PartialPkg) -> Restore PkgInfo
partialToFullPkg (pkgId, PartialPkg{..}) = do
    cabalDex   <- makeRecord ("cabal file for " ++ display pkgId)
                             partialCabal
                             partialCabalUpload
                             combineCabal
    tarballDex <- makeRecord ("tarball for " ++ display pkgId)
                             partialTarball
                             partialTarballUpload
                             combineTarball
    let cabalRevisions   = sortByUploadTimes cabalDex
        tarballRevisions = sortByUploadTimes tarballDex

    when (null cabalRevisions) $
      fail $ "No cabal files found for " ++ display pkgId

    let (latestCabalFile, _) = last cabalRevisions
        filename = display pkgId ++ ".cabal"

    case runParseResult $ parseGenericPackageDescription $
         BS.toStrict $ cabalFileByteString latestCabalFile of
      (_, Left (_, errs)) -> fail $ unlines (map (showPError filename) errs)
      (_, Right _)        -> return ()

    return PkgInfo {
      pkgInfoId            = pkgId,
      pkgMetadataRevisions = Vec.fromList cabalRevisions,
      pkgTarballRevisions  = Vec.fromList tarballRevisions
    }
  where
    combineCabal :: CabalFileText -> UploadInfo -> Restore (CabalFileText, UploadInfo)
    combineCabal cabalFile uploadInfo = return (cabalFile, uploadInfo)

    combineTarball :: (FilePath, BlobId) -> (UploadInfo, Maybe TarballInfo) -> Restore (PkgTarball, UploadInfo)
    combineTarball (filename, blobId) (uploadInfo, Just TarballInfo{..}) = do
      pkgTarballGz <-
        -- If the blob ID of the restored file matches the one in the metadata,
        -- we can assume the length and SHA256 are also okay. If not we have to
        -- recompute it.
        if blobId == infoTarGzMD5
          then return BlobInfo {
                   blobInfoId         = infoTarGzMD5
                 , blobInfoLength     = infoTarGzLength
                 , blobInfoHashSHA256 = infoTarGzSHA256
                 }
          else blobInfoFromBS blobId `fmap` restoreGetBlob blobId

      uncompressedExists <- restoreFindBlob infoTarMD5
      pkgTarballNoGz <-
        -- If the blob ID of the compressed tarball matches the reported one,
        -- and the blob ID of the uncompressed tarball exists in the store,
        -- we assume we can just use that blob. Otherwise, we decompress the
        -- restored compressed tarball and recompute its blob ID.
        if uncompressedExists && blobId == infoTarGzMD5
          then return infoTarMD5
          else do bs <- restoreGetBlob blobId
                  let bsDec = GZip.decompressNamed filename (forceLast bs)
                  restoreAddBlob bsDec

      return (PkgTarball{..}, uploadInfo)

    combineTarball (filename, blobId) (uploadInfo, Nothing) = do
      -- We are doing an import from an old backup. We have to recompute hash info
      bs <- restoreGetBlob blobId
      let bsDec = GZip.decompressNamed filename (forceLast bs)
      blobIdDec <- restoreAddBlob bsDec
      -- TODO: This will force the gz blob into memory. Can we avoid that?
      let tb = PkgTarball {
                   pkgTarballGz   = blobInfoFromBS blobId bs
                 , pkgTarballNoGz = blobIdDec
                 }
      return (tb, uploadInfo)

    makeRecord :: String
               -> [(Int, a)]
               -> [(Int, b)]
               -> (a -> b -> Restore c)
               -> Restore [c]
    makeRecord item list list' f =
       makeRecord' item 0 f (mergeBy (\(i, _) (i', _) -> compare i i')
                                     (sortBy (comparing fst) list)
                                     (sortBy (comparing fst) list'))

    -- (OnlyInLeft = no upload entry, OnlyInRight = no file), with checks for indexes
    makeRecord' :: String -- ^ Description of what we are creating (for error messages)
                -> Int    -- ^ Index we are expecting (items should be numbered consecutively)
                -> (a -> b -> Restore c)           -- ^ Combining function
                -> [MergeResult (Int, a) (Int, b)] -- ^ Merged inputs
                -> Restore [c]
    makeRecord' _ _ _ [] = return []
    makeRecord' item index f (InBoth x y:xs) =
       if fst x == index
         then do b  <- f (snd x) (snd y)
                 bs <- makeRecord' item (index+1) f xs
                 return (b:bs)
         else fail $ "Missing index " ++ show (fst x-1) ++ "for " ++ item
    makeRecord' item _ _ (OnlyInLeft  x:_) =
       fail $ item ++ " (index "++ show (fst x)
           ++ ") found without matching upload log entry"
    makeRecord' item _ _ (OnlyInRight y:_) =
       fail $ "Upload log entry for " ++ item ++ " (index "
           ++ show (fst y) ++") found, but file itself missing"

    sortByUploadTimes :: [(a, UploadInfo)] -> [(a, UploadInfo)]
    sortByUploadTimes = sortBy (comparing (fst . snd))

-- Workaround: in zlib prior to 0.5.4.1, GZip.decompress would not fully
-- consume the input data (because the gzip format means it knows when
-- it has got to the end of the expected data). As a consequence the bs
-- we get from restoreGetBlob would not have its file handle closed.
forceLast :: BS.ByteString -> BS.ByteString
forceLast = BS.fromChunks . forceLastBlock . BS.toChunks
  where
    forceLastBlock []     = []
    forceLastBlock (c:[]) = c : []
    forceLastBlock (c:cs) = c : forceLastBlock cs

--------------------------------------------------------------------------------
-- Every tarball and cabal file ever uploaded for every single package name and version
indexToAllVersions :: PackagesState -> [BackupEntry]
indexToAllVersions st =
    let pkgList = PackageIndex.allPackages . packageIndex $ st
    in maybe id (\l x -> packageUpdateLogToExport l : x)
             (either (const Nothing) Just $ packageUpdateLog st) $
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
cabalListToExport pkgId cabalInfos =
      csvToBackup (pkgPath pkgId "uploads.csv") cabalMetadata
    : map blobEntry (zip [0..] cabals)
  where
    (cabals, infos) = unzip cabalInfos
    cabalName = display (packageName pkgId) ++ ".cabal"

    blobEntry :: (Int, CabalFileText) -> BackupEntry
    blobEntry (0, CabalFileText bs) = BackupByteString (pkgPath pkgId cabalName) bs
    blobEntry (n, CabalFileText bs) = BackupByteString (pkgPath pkgId (cabalName ++ "-" ++ show n)) bs

    cabalMetadata :: CSV
    cabalMetadata =
          [showVersion versionCSVVer]
        : versionCSVKey
        : map metadataEntry (zip [0..] infos)
      where
        versionCSVVer = Version [0,1] ["unstable"]
        versionCSVKey = ["index", "time", "user-id"]

    metadataEntry :: (Int, UploadInfo) -> Record
    metadataEntry (index, (time, user)) = [
        show (index :: Int)
      , formatUTCTime time
      , display user
      ]

tarballListToExport :: PackageId -> [(PkgTarball, UploadInfo)] -> [BackupEntry]
tarballListToExport pkgId tarballInfos =
      csvToBackup (pkgPath pkgId "tarball.csv") tarballMetadata
    : map blobEntry (zip [0..] (map fst tarballInfos))
  where
    tarballName :: String
    tarballName = display pkgId ++ ".tar.gz"

    blobEntry :: (Int, PkgTarball) -> BackupEntry
    blobEntry (0, tb) = blobToBackup (pkgPath pkgId tarballName) (blobInfoId (pkgTarballGz tb))
    blobEntry (n, tb) = blobToBackup (pkgPath pkgId (tarballName ++ "-" ++ show n)) (blobInfoId (pkgTarballGz tb))

    tarballMetadata :: CSV
    tarballMetadata =
          [showVersion versionCSVVer]
        : versionCSVKey
        : map metadataEntry (zip [0..] (map (first pkgTarballToInfo) tarballInfos))
      where
        -- VERSION HISTORY
        -- version 0.1: index, time, user-id
        -- version 0.2: index, time, user-id, tar-gz-md5, tar-gz-len, tar-gz-sha256, tar-md5
        versionCSVVer = Version [0,2] ["unstable"]
        versionCSVKey = [ "index"
                        , "time"
                        , "user-id"
                        , "tar-gz-md5"
                        , "tar-gz-len"
                        , "tar-gz-sha256"
                        , "tar-md5"
                        ]

    metadataEntry :: (Int, (TarballInfo, UploadInfo)) -> Record
    metadataEntry (index, (TarballInfo{..}, (time, user))) = [
        show (index :: Int)
      , formatUTCTime time
      , display user
      , blobMd5 infoTarGzMD5
      , show    infoTarGzLength
      , show    infoTarGzSHA256
      , blobMd5 infoTarMD5
      ]

pkgPath :: PackageId -> String -> [String]
pkgPath pkgId file = ["package", display pkgId, file]


----------- Converting TarIndexEntry to and from entries

packageUpdateLogToExport :: Seq TarIndexEntry -> BackupEntry
packageUpdateLogToExport = csvToBackup ["updatelog.csv"]
                         . packageUpdateLogToCSV

packageUpdateLogToCSV :: Seq TarIndexEntry -> CSV
packageUpdateLogToCSV updlog =
    [showVersion versionCSVVer] : map entryToCSV (Foldable.toList updlog)
  where
    versionCSVVer = Version [0,1] []
    entryToCSV (CabalFileEntry pkgid revno time uid username) =
      [ "cabal"
      , display pkgid
      , show revno
      , formatUTCTime time
      , display uid
      , display username
      ]
    -- TODO: Currently ExtraEntry is used only for preferred-versions, so this
    -- way to back them up is probably okay. If these files get more complicated
    -- we should probably store them as separate files instead.
    entryToCSV (ExtraEntry extrapath content time) =
      [ "extra"
      , extrapath
      , formatUTCTime time
      , BSC.unpack content
      ]
    entryToCSV (MetadataEntry pkgid revno time) =
      [ "metadata"
      , display pkgid
      , show revno
      , formatUTCTime time
      ]

importTarIndexEntries :: CSV -> Restore (Seq TarIndexEntry)
importTarIndexEntries = fmap Seq.fromList . mapM fromRecord . drop 1
  where
    fromRecord :: Record -> Restore TarIndexEntry
    fromRecord ["cabal", strPkgid, strRevno, strTime, strUid, username] = do
       pkgid    <- parseText    "pkgid"     strPkgid
       revno    <- parseRead    "revno"     strRevno
       utcTime  <- parseUTCTime "time"      strTime
       uid      <- parseText    "uid"       strUid
       -- We don't use parseText for the username because this would throw
       -- an error if the username is empty
       return $ CabalFileEntry pkgid revno utcTime uid (UserName username)

    fromRecord ["extra", extrapath, strTime, extracontent] = do
       utcTime <- parseUTCTime "time" strTime
       return $ ExtraEntry extrapath (BSC.pack extracontent) utcTime

    fromRecord ["metadata", strPkgid, strRevno, strTime] = do
       pkgid    <- parseText    "pkgid"     strPkgid
       revno    <- parseRead    "revno"     strRevno
       utcTime  <- parseUTCTime "time"      strTime
       return $ MetadataEntry pkgid revno utcTime

    fromRecord x = fail $ "Error index entries list: " ++ show x
