-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.Packages.Index
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Create the package index.
--
-----------------------------------------------------------------------------
module Distribution.Server.Packages.Index (
    write,
  ) where

import qualified Codec.Archive.Tar.Entry as Tar
import qualified Distribution.Server.Util.Index as PackageIndex

import Distribution.Server.Packages.Types
         ( CabalFileText(..), PkgInfo(..)
         , pkgLatestCabalFileText, pkgLatestUploadInfo )
import Distribution.Server.Users.Users
         ( Users, userIdToName )

import Distribution.Text
         ( display )
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import Data.Time.Clock
         ( UTCTime )
import Data.Time.Clock.POSIX
         ( utcTimeToPOSIXSeconds )
import Data.Int (Int64)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Lazy (ByteString)
import Prelude hiding (read)

-- Construct, with the specified user database, extra top-level files, and
-- a package index, an index tarball. This tarball has the modification times
-- and uploading users built-in.
write :: Users -> Map String (ByteString, UTCTime) -> PackageIndex PkgInfo -> ByteString
write users =
    PackageIndex.write (cabalFileByteString . pkgLatestCabalFileText) setModTime
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
    utcToUnixTime :: UTCTime -> Int64
    utcToUnixTime = truncate . utcTimeToPOSIXSeconds
    userName = display . userIdToName users
    extraEntries emap = do
        (path, (entry, mtime)) <- Map.toList emap
        Right tarPath <- return $ Tar.toTarPath False path
        return $ (Tar.fileEntry tarPath entry) { Tar.entryTime = utcToUnixTime mtime }

