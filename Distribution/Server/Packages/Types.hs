{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable,
             StandaloneDeriving, TemplateHaskell, TypeFamilies #-}
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

import Distribution.Server.Users.Types (UserId(..))
import Distribution.Server.Framework.BlobStorage (BlobId, BlobId_v0)
import Distribution.Server.Framework.Instances (PackageIdentifier_v0)
import Distribution.Server.Framework.MemSize
import Distribution.Server.Util.Parse (unpackUTF8)

import Distribution.Package
         ( PackageIdentifier(..), Package(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(..))
import Distribution.PackageDescription.Parse
         ( parsePackageDescription, ParseResult(..) )

import Control.Applicative
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (Day(..))
import Data.Typeable (Typeable)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.SafeCopy


newtype CabalFileText = CabalFileText { cabalFileByteString :: ByteString }
  deriving (Eq, MemSize)

cabalFileString :: CabalFileText -> String
cabalFileString = unpackUTF8 . cabalFileByteString

instance SafeCopy CabalFileText where
  putCopy (CabalFileText bs) = contain $ Serialize.put bs
  getCopy = contain $ CabalFileText <$> Serialize.get

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

deriveSafeCopy 2 'extension ''PkgInfo
deriveSafeCopy 2 'extension ''PkgTarball

instance MemSize PkgInfo where
    memSize (PkgInfo a b c d e) = memSize5 a b c d e

instance MemSize PkgTarball where
    memSize (PkgTarball a b) = memSize2 a b

--------------------------
-- Old SafeCopy versions
--

data PkgInfo_v0 = PkgInfo_v0  !PackageIdentifier_v0 !CabalFileText
                              ![(PkgTarball_v0, UploadInfo_v0)]
                              ![(CabalFileText, UploadInfo_v0)]
                              !UploadInfo_v0

instance SafeCopy  PkgInfo_v0
instance Serialize PkgInfo_v0 where
    put (PkgInfo_v0 a (CabalFileText b) c d e) =
         Serialize.put a
      >> Serialize.put b
      >> Serialize.put c
      >> Serialize.put [ (bs, uinf) | (CabalFileText bs, uinf) <- d ]
      >> Serialize.put e
    get = PkgInfo_v0 <$> 
         Serialize.get
     <*> (CabalFileText <$> Serialize.get)
     <*> Serialize.get
     <*> (map (\(bs,uinf) -> (CabalFileText bs, uinf)) <$> Serialize.get)
     <*> Serialize.get

instance Migrate PkgInfo where
    type MigrateFrom PkgInfo = PkgInfo_v0
    migrate (PkgInfo_v0 a b c d e) =
      PkgInfo (migrate a) b
              [ (migrate pt, migrateUploadInfo ui) | (pt, ui) <- c ]
              [ (cf, (migrateUploadInfo ui)) | (cf, ui) <- d ]
              (migrateUploadInfo e)
      where
        migrateUploadInfo (UTCTime_v0 ts, UserId_v0 uid) = (ts, UserId uid)


type UploadInfo_v0 = (UTCTime_v0, UserId_v0)

newtype UTCTime_v0 = UTCTime_v0 UTCTime
instance Serialize UTCTime_v0 where
  put (UTCTime_v0 time) = do
    Serialize.put (toModifiedJulianDay $ utctDay time)
    Serialize.put (toRational $ utctDayTime time)
  get = do
    day  <- Serialize.get
    secs <- Serialize.get
    return (UTCTime_v0 (UTCTime (ModifiedJulianDay day) (fromRational secs)))

newtype UserId_v0 = UserId_v0 Int
instance Serialize UserId_v0 where
  put (UserId_v0 x) = Serialize.put x
  get = UserId_v0 <$> Serialize.get


data PkgTarball_v0 = PkgTarball_v0 !BlobId_v0 !BlobId_v0
instance SafeCopy PkgTarball_v0
instance Serialize PkgTarball_v0 where
    put (PkgTarball_v0 a b) = Serialize.put a >> Serialize.put b
    get = PkgTarball_v0 <$> Serialize.get <*> Serialize.get

instance Migrate PkgTarball where
    type MigrateFrom PkgTarball = PkgTarball_v0
    migrate (PkgTarball_v0 a b) = PkgTarball (migrate a) (migrate b)
