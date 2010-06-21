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
import Distribution.Server.Util.BlobStorage (BlobId)
import Distribution.Server.Instances ()

import Distribution.Package
         ( PackageIdentifier(..), Package(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(..))
import Distribution.PackageDescription.Parse
         ( parsePackageDescription, ParseResult(..) )
import Distribution.Simple.Utils (fromUTF8)

import qualified Data.Binary as Binary
import Data.Binary (Binary)
import qualified Data.ByteString.Lazy.Char8 as BS (unpack)
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import Data.List (sortBy)
import Data.Ord (comparing)

-- | The information we keep about a particular version of a package.
-- 
-- Previous versions of this package name and version may exist as well.
-- We normally disallow re-uploading but may make occasional exceptions,
-- such as , and there are some such old packages.
data PkgInfo = PkgInfo {
    pkgInfoId :: !PackageIdentifier,
    pkgDesc   :: !GenericPackageDescription,
    -- | The .cabal file text.
    pkgData   :: !ByteString,
    -- | The actual package .tar.gz file. It is optional for making an incomplete
    -- mirror, e.g. using archives of just the latest packages, or perhaps for a
    -- multipart upload process.
    --
    -- The canonical tarball URL points to the most recently uploaded package.
    pkgTarball :: ![(BlobId, UploadInfo)],
    -- | Previous data. The UploadInfo does *not* indicate when the ByteString was
    -- uploaded, but rather when it was replaced. This way, pkgUploadData won't change
    -- even if a cabal file is changed.
    pkgDataOld :: ![(ByteString, UploadInfo)],
    -- | When the package was created. Imports will override this with time in their logs.
    pkgUploadData :: !UploadInfo
} deriving (Typeable, Show)

type UploadInfo = (UTCTime, UserId)

pkgUploadTime :: PkgInfo -> UTCTime
pkgUploadTime = fst . pkgUploadData

pkgUploadUser :: PkgInfo -> UserId
pkgUploadUser = snd . pkgUploadData

-- a small utility
descendUploadTimes :: [(a, UploadInfo)] -> [(a, UploadInfo)]
descendUploadTimes = sortBy (flip $ comparing (fst . snd))

instance Package PkgInfo where packageId = pkgInfoId

instance Binary PkgInfo where
  put pkgInfo = do
    Binary.put (pkgInfoId pkgInfo)
    Binary.put (pkgUploadData pkgInfo)
    Binary.put (pkgDataOld pkgInfo)
    Binary.put (pkgTarball pkgInfo)
    Binary.put (pkgData pkgInfo)

  get = do
    infoId  <- Binary.get
    updata  <- Binary.get
    old     <- Binary.get
    tarball <- Binary.get
    bstring <- Binary.get
    return PkgInfo {
      pkgInfoId = infoId,
      pkgDesc   = case parse bstring of
                    -- XXX: Better error message?
                    ParseFailed e -> error $ "Internal error: " ++ show e
                    ParseOk _ x   -> x,
      pkgUploadData = updata,
      pkgDataOld    = old,
      pkgTarball    = tarball,
      pkgData       = bstring
    }
    where parse = parsePackageDescription . fromUTF8 . BS.unpack
