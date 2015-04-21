{-# LANGUAGE BangPatterns, TemplateHaskell #-}
module Distribution.Server.Packages.Index (
    write,
    TarIndexEntry(..),
  ) where

import qualified Codec.Archive.Tar       as Tar
         ( write )
import qualified Codec.Archive.Tar.Entry as Tar
         ( Entry(..), fileEntry, toTarPath, Ownership(..) )
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Framework.MemSize

import Distribution.Server.Packages.Types
         ( CabalFileText(..), PkgInfo(..) )
import Distribution.Server.Users.Types
         ( UserName(..) )

import Distribution.Text
         ( display )
import Distribution.Package
         ( PackageId, PackageName(..), packageName, packageVersion )
import Data.Time.Clock
         ( UTCTime )
import Data.Time.Clock.POSIX
         ( utcTimeToPOSIXSeconds )
import Data.Int (Int64)
import Data.SafeCopy (base, deriveSafeCopy)

import qualified Data.Vector as Vec
import Data.ByteString.Lazy (ByteString)
import System.FilePath.Posix
import Data.Maybe (catMaybes)


-- | Entries used to construct the contents of the hackage index tarball
--
data TarIndexEntry = PackageEntry !PackageId !RevisionNo !UTCTime !UserName
                   | ExtraEntry !FilePath !ByteString !UTCTime
  deriving (Eq, Show)

type RevisionNo = Int

instance MemSize TarIndexEntry where
  memSize (PackageEntry a b c d) = memSize4 a b c d
  memSize (ExtraEntry   a b c)   = memSize3 a b c

deriveSafeCopy 0 'base ''TarIndexEntry

-- Construct, with the specified user database, extra top-level files, and
-- a package index, an index tarball. This tarball has the modification times
-- and uploading users built-in.

write :: PackageIndex PkgInfo -> [TarIndexEntry] -> ByteString
write pkgs =
    Tar.write . catMaybes . map mkTarEntry
  where
    -- This should never return Nothing, it'd be an internal error but just
    -- in case we'll skip them
    mkTarEntry :: TarIndexEntry -> Maybe Tar.Entry

    mkTarEntry (PackageEntry pkgid revno timestamp username) = do
        pkginfo   <- PackageIndex.lookupPackageId pkgs pkgid
        cabalfile <- fmap (cabalFileByteString . fst) $
                     pkgMetadataRevisions pkginfo Vec.!? revno
        tarPath   <- either (const Nothing) Just $
                     Tar.toTarPath False fileName
        let !tarEntry = addTimestampAndOwner timestamp username $
                          Tar.fileEntry tarPath cabalfile
        return tarEntry
      where
        PackageName pkgname = packageName pkgid
        fileName = pkgname </> display (packageVersion pkgid)
                           </> pkgname <.> "cabal"

    mkTarEntry (ExtraEntry fileName content timestamp) = do
      tarPath <- either (const Nothing) Just $
                  Tar.toTarPath False fileName
      let !tarEntry = addTimestampAndOwner timestamp (UserName "Hackage") $
                        Tar.fileEntry tarPath content
      return tarEntry

    addTimestampAndOwner timestamp (UserName username) entry =
      entry {
        Tar.entryTime      = utcToUnixTime timestamp,
        Tar.entryOwnership = Tar.Ownership {
          Tar.ownerName = username,
          Tar.groupName = "Hackage",
          Tar.ownerId = 0,
          Tar.groupId = 0
        }
      }

utcToUnixTime :: UTCTime -> Int64
utcToUnixTime = truncate . utcTimeToPOSIXSeconds
{-
write :: Users -> Map String (ByteString, UTCTime) -> PackageIndex PkgInfo -> ByteString
write users =
    PackageIndex.write (cabalFileByteString . pkgLatestCabalFileText) setModTime
  . extraEntries
  where
    setModTime pkgInfo entry =
      let (utime, uuser) = pkgLatestUploadInfo pkgInfo in 
      entry {
        Tar.entryTime      = utcToUnixTime timestamp,
        Tar.entryOwnership = Tar.Ownership {
          Tar.ownerName = username,
          Tar.groupName = "Hackage",
          Tar.ownerId = 0,
          Tar.groupId = 0
        }
      }
    utcToUnixTime :: UTCTime -> Int64
    utcToUnixTime = truncate . utcTimeToPOSIXSeconds
    userName = display . userIdToName users
    extraEntries emap = do
        (path, (entry, mtime)) <- Map.toList emap
        Right tarPath <- return $ Tar.toTarPath False path
        return $ (Tar.fileEntry tarPath entry) { Tar.entryTime = utcToUnixTime mtime }

-- | Create an uncompressed tar repository index file as a 'ByteString'.
--
-- Takes a couple functions to turn a package into a tar entry. Extra
-- entries are also accepted.
--
write :: Package pkg
      => (pkg -> ByteString)
      -> (pkg -> Tar.Entry -> Tar.Entry)
      -> [Tar.Entry]
      -> PackageIndex pkg
      -> ByteString
write externalPackageRep updateEntry extras =
  Tar.write . (extras++) . map entry . PackageIndex.allPackages
  where
    entry pkg = updateEntry pkg
              . Tar.fileEntry tarPath
              $ externalPackageRep pkg
      where
        Right tarPath = Tar.toTarPath False fileName
        PackageName name = packageName pkg
        fileName = name </> display (packageVersion pkg)
                        </> name <.> "cabal"
-}
