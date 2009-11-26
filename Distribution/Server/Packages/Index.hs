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
         ( Entry(..), Ownership(..) )
import qualified Distribution.Server.Util.Index as PackageIndex

import Distribution.Server.Packages.Types
         ( PkgInfo(..) )
import qualified Distribution.Server.Users.Users as Users
         ( Users, idToName )

import Distribution.Text
         ( display )
import Distribution.Server.PackageIndex (PackageIndex)
import qualified Distribution.Server.PackageIndex as PackageIndex
import Data.Time.Clock
         ( UTCTime )
import Data.Time.Clock.POSIX
         ( utcTimeToPOSIXSeconds )
import Data.Int (Int64)

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Prelude hiding (read)

write :: Users.Users -> PackageIndex PkgInfo -> ByteString
write users = PackageIndex.write pkgData setModTime
  where
    setModTime pkgInfo entry = entry {
      Tar.entryTime      = utcToUnixTime (pkgUploadTime pkgInfo),
      Tar.entryOwnership = Tar.Ownership {
        Tar.ownerName = userName (pkgUploadUser pkgInfo),
        Tar.groupName = "HackageDB",
        Tar.ownerId = 0,
        Tar.groupId = 0
      }
    }
    utcToUnixTime :: UTCTime -> Int64
    utcToUnixTime = truncate . utcTimeToPOSIXSeconds
    userName = display . Users.idToName users
