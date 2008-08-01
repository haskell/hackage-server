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

import Distribution.Server.BlobStorage
         ( BlobId )

import Distribution.Package
         ( PackageIdentifier(..), Package(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(..) )

import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)

deriving instance Typeable GenericPackageDescription
deriving instance Typeable PackageIdentifier

type UserName = String --FIXME: use proper username type

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
    pkgUploadUser :: String,

    -- | Previous upload times and users. We normally disallow re-uploading but
    -- we may make occasional exceptions, and there are some such old packages.
    pkgUploadOld  :: [(UTCTime, String)]
  }
  deriving Typeable

instance Package PkgInfo where packageId = pkgInfoId

