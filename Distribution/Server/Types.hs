{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.Types
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- All data types for the entire cabal-install system gathered here to avoid some .hs-boot files.
-----------------------------------------------------------------------------
module Distribution.Server.Types where

import Distribution.Server.Users.Types
         ( UserId )
import Distribution.Server.Util.BlobStorage
         ( BlobId )
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

-- | The information we keep about a particular version of a package.
data PkgInfo = PkgInfo {
    pkgInfoId :: PackageIdentifier,
    pkgDesc   :: GenericPackageDescription,

    -- | The .cabal file text.
    pkgData   :: ByteString,

    -- | The actual package .tar.gz file. It is optional for the moment
    -- to make testing easier, eg using archives of just the latest packages.
    pkgTarball :: Maybe BlobId,

    -- | When the .tar.gz file was uploaded.
    pkgUploadTime :: UTCTime,

    -- | Who uploaded the .tar.gz file.
    pkgUploadUser :: UserId,

    -- | Previous upload times and users. We normally disallow re-uploading but
    -- we may make occasional exceptions, and there are some such old packages.
    pkgUploadOld  :: [(UTCTime, UserId)]
  }
  deriving Typeable

instance Package PkgInfo where packageId = pkgInfoId

instance Binary PkgInfo where
  put pkgInfo = do
    Binary.put (pkgInfoId pkgInfo)
    Binary.put (pkgUploadTime pkgInfo)
    Binary.put (pkgUploadUser pkgInfo)
    Binary.put (pkgUploadOld pkgInfo)
    Binary.put (pkgTarball pkgInfo)
    Binary.put (pkgData pkgInfo)

  get = do
    infoId  <- Binary.get
    mtime   <- Binary.get
    user    <- Binary.get
    old     <- Binary.get
    tarball <- Binary.get
    bstring <- Binary.get
    return PkgInfo {
      pkgInfoId = infoId,
      pkgDesc   = case parse bstring of
                    -- XXX: Better error message?
                    ParseFailed e -> error $ "Internal error: " ++ show e
                    ParseOk _ x   -> x,
      pkgUploadTime = mtime,
      pkgUploadUser = user,
      pkgUploadOld  = old,
      pkgData   = bstring,
      pkgTarball= tarball
    }
    where parse = parsePackageDescription . fromUTF8 . BS.unpack
