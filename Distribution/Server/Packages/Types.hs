{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, StandaloneDeriving, TemplateHaskell #-}
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
import Distribution.Server.Util.Parse (unpackUTF8)

import Distribution.Package
         ( PackageIdentifier(..), Package(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(..))
import Distribution.PackageDescription.Parse
         ( parsePackageDescription, ParseResult(..) )

import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.SafeCopy

newtype CabalFileText = CabalFileText { cabalFileByteString :: ByteString }
  deriving (Eq, Serialize)

cabalFileString :: CabalFileText -> String
cabalFileString = unpackUTF8 . cabalFileByteString

instance Show CabalFileText where
    show cft = "CabalFileText (Data.ByteString.Lazy.Char8.pack (Distribution.Simple.Utils.toUTF8 " ++ show (cabalFileString cft) ++ "))"

-- | The information we keep about a particular version of a package.
-- 
-- Previous versions of this package name and version may exist as well.
-- We normally disallow re-uploading but may make occasional exceptions.
data PkgInfo = PkgInfo {
    pkgInfoId :: !PackageIdentifier,
    -- | The .cabal file text.
    pkgData   :: !CabalFileText,
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
    pkgDataOld :: ![(CabalFileText, UploadInfo)],
    -- | When the package was created. Imports will override this with time in their logs.
    pkgUploadData :: !UploadInfo
} deriving (Eq, Typeable, Show)

instance SafeCopy PkgInfo where
  putCopy = contain . Serialize.put
  getCopy = contain Serialize.get

-- | The information held in a parsed .cabal file (used by cabal-install)
pkgDesc :: PkgInfo -> GenericPackageDescription
pkgDesc pkgInfo
     = case parsePackageDescription $ cabalFileString $ pkgData pkgInfo of
       -- We only make PkgInfos with parsable pkgDatas, so if it
       -- doesn't parse then something has gone wrong.
       ParseFailed e -> error ("Internal error: " ++ show e)
       ParseOk _ x   -> x

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
    cabal <- Serialize.get
    case parsePackageDescription . cabalFileString $ cabal of
        ParseFailed e -> fail $ "Internal error: " ++ show e
        ParseOk _ _   -> return ()
    tarball <- Serialize.get
    old     <- Serialize.get
    updata  <- Serialize.get
    return PkgInfo {
        pkgInfoId = infoId,
        pkgUploadData = updata,
        pkgDataOld    = old,
        pkgTarball    = tarball,
        pkgData       = cabal
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

