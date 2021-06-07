{-# LANGUAGE BangPatterns, TemplateHaskell #-}
module Distribution.Server.Packages.Index (
    writeIncremental,
    TarIndexEntry(..),
    legacyExtras,
    writeLegacy,
  ) where

import qualified Codec.Archive.Tar       as Tar
         ( write )
import qualified Codec.Archive.Tar.Entry as Tar
         ( Entry(..), fileEntry, toTarPath, Ownership(..) )
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Framework.MemSize

import Distribution.Server.Packages.Types
         ( CabalFileText(..), PkgInfo(..)
         , pkgLatestCabalFileText, pkgLatestUploadInfo
         )
import Distribution.Server.Packages.Metadata
import Distribution.Server.Users.Users
         ( Users, userIdToName )
import Distribution.Server.Users.Types
         ( UserId(..), UserName(..) )
import Distribution.Server.Util.ParseSpecVer

import Distribution.Text
         ( display )
import Distribution.Types.PackageName
import Distribution.Package
         ( Package, PackageId, packageName, packageVersion )
import Distribution.Version (mkVersion)
import Data.Time.Clock
         ( UTCTime )
import Data.Time.Clock.POSIX
         ( utcTimeToPOSIXSeconds )
import Data.Int (Int64)
import Data.SafeCopy (base, deriveSafeCopy)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as Vec
import Data.ByteString.Lazy (ByteString)
import System.FilePath.Posix
import Data.Maybe (catMaybes, mapMaybe)


-- | Entries used to construct the contents of the hackage index tarball
--
data TarIndexEntry =
    -- | Package cabal files
    --
    -- We keep a copy of the UserName because usernames can change (and if they
    -- do, old entries in the index should remain the same); similarly, we
    -- keep a copy of the upload time because the upload time of a package
    -- can also be changed (this is used during mirroring, for instance).
    --
    -- The UTCTime and userName are used as file metadata in the tarball.
    CabalFileEntry !PackageId !RevisionNo !UTCTime !UserId !UserName

    -- | Package metadata
    --
    -- We add these whenever a new package tarball is uploaded.
    --
    -- This metadata entry can be for any revision of the packages
    -- (not necessarily the latest) so we need to know the revision number.
    --
    -- Although we do not currently allow to change the upload time for package
    -- tarballs, but I'm not sure why not (TODO) and it's conceivable we may
    -- change this, so we record the original upload time.
  | MetadataEntry !PackageId !RevisionNo !UTCTime

    -- | Additional entries that we add to the tarball
    --
    -- This is currently used for @preferred-versions@.
  | ExtraEntry !FilePath !ByteString !UTCTime
  deriving (Eq, Show)

type RevisionNo = Int

instance MemSize TarIndexEntry where
  memSize (CabalFileEntry a b c d e) = memSize5 a b c d e
  memSize (MetadataEntry  a b c)   = memSize3 a b c
  memSize (ExtraEntry     a b c)   = memSize3 a b c

deriveSafeCopy 0 'base ''TarIndexEntry

-- Construct, with the specified user database, extra top-level files, and
-- a package index, an index tarball. This tarball has the modification times
-- and uploading users built-in.

writeIncremental :: PackageIndex PkgInfo -> [TarIndexEntry] -> ByteString
writeIncremental pkgs =
    Tar.write . catMaybes . map mkTarEntry
  where
    -- This should never return Nothing, it'd be an internal error but just
    -- in case we'll skip them
    mkTarEntry :: TarIndexEntry -> Maybe Tar.Entry

    mkTarEntry (CabalFileEntry pkgid revno timestamp userid username) = do
        pkginfo   <- PackageIndex.lookupPackageId pkgs pkgid
        cabalfile <- fmap (cabalFileByteString . fst) $
                     pkgMetadataRevisions pkginfo Vec.!? revno
        tarPath   <- either (const Nothing) Just $
                     Tar.toTarPath False fileName
        let !tarEntry = addTimestampAndOwner timestamp userid username $
                          Tar.fileEntry tarPath cabalfile
        return tarEntry
      where
        pkgname = unPackageName (packageName pkgid)
        fileName = pkgname </> display (packageVersion pkgid)
                           </> pkgname <.> "cabal"

    mkTarEntry (MetadataEntry pkgid revno timestamp) = do
        pkginfo <- PackageIndex.lookupPackageId pkgs pkgid
        let (filePath, content) = computePkgMetadata pkginfo revno
        tarPath <- either (const Nothing) Just $ Tar.toTarPath False filePath
        let !tarEntry = addTimestampAndOwner timestamp (UserId 0) (UserName "Hackage") $
                          Tar.fileEntry tarPath content
        return tarEntry

    mkTarEntry (ExtraEntry fileName content timestamp) = do
      tarPath <- either (const Nothing) Just $
                  Tar.toTarPath False fileName
      let !tarEntry = addTimestampAndOwner timestamp (UserId 0) (UserName "Hackage") $
                        Tar.fileEntry tarPath content
      return tarEntry

    addTimestampAndOwner timestamp (UserId uid) (UserName username) entry =
      entry {
        Tar.entryTime      = utcToUnixTime timestamp,
        Tar.entryOwnership = Tar.Ownership {
          Tar.ownerName = username,
          Tar.groupName = "Hackage",
          Tar.ownerId = uid,
          Tar.groupId = 0
        }
      }

utcToUnixTime :: UTCTime -> Int64
utcToUnixTime = truncate . utcTimeToPOSIXSeconds

-- | Extract legacy entries
legacyExtras :: [TarIndexEntry] -> Map String (ByteString, UTCTime)
legacyExtras = go Map.empty
  where
    -- Later entries in the update log will override earlier ones. This is
    -- intentional.
    go :: Map String (ByteString, UTCTime)
       -> [TarIndexEntry]
       -> Map String (ByteString, UTCTime)
    go acc [] = acc
    go acc (ExtraEntry fp bs time : es) =
       let acc' = Map.insert fp (bs, time) acc
       in go acc' es

    -- CabalFileEntry simply points into the package DB. It is here only to
    -- record the order in which things were added, as well as the original
    -- username; it's not important for the legacy index, which will just
    -- extract the latest cabal files directly from the package DB.
    go acc (CabalFileEntry{} : es) = go acc es

    -- The legacy index does not contain the TUF metadata
    go acc (MetadataEntry{} : es) = go acc es

-- | Write tarball in legacy format
--
-- "Legacy format" here refers to prior to the intrudction of the incremental
-- index, and contains the packages in order of packages/versions (for better
-- compression), contains at most one preferred-version per package (important
-- because of a bug in cabal which would otherwise merge all perferred-versions
-- files for a package), and does not contain the TUF files.
writeLegacy :: Users -> Map String (ByteString, UTCTime) -> PackageIndex PkgInfo -> ByteString
writeLegacy users =
    writeLegacyAux (cabalFileByteString . pkgLatestCabalFileText) setModTime
  . extraEntries
  where
    setModTime pkgInfo entry =
      let (utime, uuser) = pkgLatestUploadInfo pkgInfo in
      entry {
        Tar.entryTime      = utcToUnixTime utime,
        Tar.entryOwnership = Tar.Ownership {
          Tar.ownerName = userName uuser,
          Tar.groupName = "Hackage",
          Tar.ownerId = 0,
          Tar.groupId = 0
        }
      }

    userName = display . userIdToName users

    extraEntries :: Map FilePath (ByteString, UTCTime) -> [Tar.Entry]
    extraEntries emap = do
        (path, (entry, mtime)) <- Map.toList emap
        Right tarPath <- return $ Tar.toTarPath False path
        return $ (Tar.fileEntry tarPath entry) { Tar.entryTime = utcToUnixTime mtime }

-- | Create an uncompressed tar repository index file as a 'ByteString'.
--
-- Takes a couple functions to turn a package into a tar entry. Extra
-- entries are also accepted.
--
-- This used to live in Distribution.Server.Util.Index.
--
-- NOTE: In order to mitigate the effects of
--       https://github.com/haskell/cabal/issues/4624
--       as a hack, this operation filters out .cabal files
--       with cabal-version >= 2.
writeLegacyAux :: Package pkg
               => (pkg -> ByteString)
               -> (pkg -> Tar.Entry -> Tar.Entry)
               -> [Tar.Entry]
               -> PackageIndex pkg
               -> ByteString
writeLegacyAux externalPackageRep updateEntry extras =
  Tar.write . (extras++) . mapMaybe entry . PackageIndex.allPackages
  where
    -- entry :: pkg -> Maybe Tar.Entry
    entry pkg
      | specVer >= mkVersion [2] = Nothing
      | otherwise                = Just
                                 . updateEntry pkg
                                 . Tar.fileEntry tarPath
                                 $ cabalText
      where
        Right tarPath = Tar.toTarPath False fileName
        name = unPackageName $ packageName pkg
        fileName = name </> display (packageVersion pkg)
                        </> name <.> "cabal"

        -- TODO: Hack-alert! We want to do this in a more elegant way.
        specVer = parseSpecVerLazy cabalText
        cabalText = externalPackageRep pkg
