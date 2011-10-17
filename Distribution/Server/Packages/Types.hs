{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, StandaloneDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.Packages.Types
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- All data types for the entire cabal-install system gathered here to avoid some .hs-boot files.
-----------------------------------------------------------------------------
module Distribution.Server.Packages.Types where

import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Framework.BlobStorage (BlobId)
import Distribution.Server.Framework.Instances ()

import Distribution.Package
         ( PackageIdentifier(..), Package(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(..))
import Distribution.PackageDescription.Parse
         ( parsePackageDescription, ParseResult(..) )
import Distribution.Simple.Utils (fromUTF8)

import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import qualified Data.ByteString.Lazy.Char8 as BS (unpack)
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import Data.List (sortBy)
import Data.Ord (comparing)

-- | The information we keep about a particular version of a package.
-- 
-- Previous versions of this package name and version may exist as well.
-- We normally disallow re-uploading but may make occasional exceptions.
data PkgInfo = PkgInfo {
    pkgInfoId :: !PackageIdentifier,
    -- | The information held in a parsed .cabal file (used by cabal-install)
    -- If this takes up too much space, it might be possible to wrap it in
    -- Maybe and let the server parse the data on the fly.
    pkgDesc   :: !GenericPackageDescription,
    -- | The .cabal file text.
    pkgData   :: !ByteString,
    -- | The actual package .tar.gz file. It is optional for making an incomplete
    -- mirror, e.g. using archives of just the latest packages, or perhaps for a
    -- multipart upload process.
    --
    -- The canonical tarball URL points to the most recently uploaded package.
    pkgTarball :: ![(PkgTarball, UploadInfo)],
    -- | Previous data. The UploadInfo does *not* indicate when the ByteString was
    -- uploaded, but rather when it was replaced. This way, pkgUploadData won't change
    -- even if a cabal file is changed.
    -- Should be updated whenever a tarball is uploaded (see mergePkg state function)
    pkgDataOld :: ![(ByteString, UploadInfo)],
    -- | When the package was created. Imports will override this with time in their logs.
    pkgUploadData :: !UploadInfo
} deriving (Eq, Typeable, Show)

data PkgTarball = PkgTarball {
   pkgTarballGz   :: !BlobId,
   pkgTarballNoGz :: !BlobId
} deriving (Eq, Typeable, Show)

type UploadInfo = (UTCTime, UserId)

pkgUploadTime :: PkgInfo -> UTCTime
pkgUploadTime = fst . pkgUploadData

pkgUploadUser :: PkgInfo -> UserId
pkgUploadUser = snd . pkgUploadData

-- a small utility
descendUploadTimes :: [(a, UploadInfo)] -> [(a, UploadInfo)]
descendUploadTimes = sortBy (flip $ comparing (fst . snd))

instance Package PkgInfo where packageId = pkgInfoId

instance Serialize PkgInfo where
  put pkgInfo = do
    Serialize.put (pkgInfoId pkgInfo)
    Serialize.put (pkgData pkgInfo)
    Serialize.put (pkgTarball pkgInfo)
    Serialize.put (pkgDataOld pkgInfo)
    Serialize.put (pkgUploadData pkgInfo)

  get = do
    infoId  <- Serialize.get
    bstring <- Serialize.get
    desc <- case parsePackageDescription . fromUTF8 . BS.unpack $ bstring of
        ParseFailed e -> fail $ "Internal error: " ++ show e
        ParseOk _ x   -> return x
    tarball <- Serialize.get
    old     <- Serialize.get
    updata  <- Serialize.get
    return PkgInfo {
        pkgInfoId = infoId,
        pkgDesc   = desc,
        pkgUploadData = updata,
        pkgDataOld    = old,
        pkgTarball    = tarball,
        pkgData       = bstring
    }

instance Serialize PkgTarball where
    put tb = do
      Serialize.put (pkgTarballGz tb)
      Serialize.put (pkgTarballNoGz tb)
    get = do
      gz <- Serialize.get
      noGz <- Serialize.get
      return PkgTarball {
          pkgTarballGz = gz,
          pkgTarballNoGz = noGz
      }

------------------------------------------------------
-- | The information we keep about a candidate package.
-- 
-- It's currently possible to have candidates for packages which don't exist yet.
--
data CandPkgInfo = CandPkgInfo {
    candInfoId  :: !PackageIdentifier,
    -- there should be one ByteString and one BlobId per candidate.
    -- this was enforced in the types.. but it's easier to just
    -- reuse PkgInfo for the task.
    candPkgInfo :: !PkgInfo,
    -- | Warnings to display at the top of the package page.
    candWarnings   :: ![String],
    -- | Whether to allow non-maintainers to view the page or not.
    candPublic :: !Bool
} deriving (Show, Typeable)

instance Package CandPkgInfo where packageId = candInfoId

instance Serialize CandPkgInfo where
  put pkgInfo = do
    Serialize.put (candInfoId pkgInfo)
    Serialize.put (candPkgInfo pkgInfo)
    Serialize.put (candWarnings pkgInfo)
    Serialize.put (candPublic pkgInfo)

  get = do
    infoId  <- Serialize.get
    pkgInfo <- Serialize.get
    warning <- Serialize.get
    public  <- Serialize.get
    return CandPkgInfo {
        candInfoId  = infoId,
        candPkgInfo = pkgInfo,
        candWarnings = warning,
        candPublic = public
    }


