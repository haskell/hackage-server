{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable,
             StandaloneDeriving, TemplateHaskell, TypeFamilies,
             RecordWildCards #-}
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

import qualified Data.Vector as Vec
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

    -- | The .cabal file text. This includes all revisions, indexed from the
    -- original vision (revision 0). This is always non-empty.
    --
    pkgMetadataRevisions :: !(Vec.Vector (CabalFileText, UploadInfo)),

    -- | The package .tar.gz file. This includes all revisions but is typically
    -- of length 1. It can be empty (to allow a multi-stage upload process, or
    -- perhaps in future for making an incomplete mirror, e.g. using archives
    -- of just the latest packages). The representation allows multiple versions
    -- but the normal policy is not to allow replacing the tarball.
    --
    pkgTarballRevisions :: !(Vec.Vector (PkgTarball, UploadInfo))

} deriving (Eq, Typeable, Show)

instance Package PkgInfo where packageId = pkgInfoId

data PkgTarball = PkgTarball {
   pkgTarballGz   :: !BlobId,
   pkgTarballNoGz :: !BlobId
} deriving (Eq, Typeable, Show)

type UploadInfo = (UTCTime, UserId)

pkgOriginalRevision :: PkgInfo -> (CabalFileText, UploadInfo)
pkgOriginalRevision = Vec.head . pkgMetadataRevisions

pkgOriginalUploadInfo :: PkgInfo -> UploadInfo
pkgOriginalUploadInfo = snd . pkgOriginalRevision

pkgOriginalUploadTime :: PkgInfo -> UTCTime
pkgOriginalUploadTime = fst . pkgOriginalUploadInfo

pkgOriginalUploadUser :: PkgInfo -> UserId
pkgOriginalUploadUser = snd . pkgOriginalUploadInfo

pkgLatestRevision :: PkgInfo -> (CabalFileText, UploadInfo)
pkgLatestRevision = Vec.last . pkgMetadataRevisions

pkgLatestCabalFileText :: PkgInfo -> CabalFileText
pkgLatestCabalFileText = fst . pkgLatestRevision

pkgLatestUploadInfo :: PkgInfo -> UploadInfo
pkgLatestUploadInfo = snd . pkgLatestRevision

pkgLatestUploadTime :: PkgInfo -> UTCTime
pkgLatestUploadTime = fst . pkgLatestUploadInfo

pkgLatestUploadUser :: PkgInfo -> UserId
pkgLatestUploadUser = snd . pkgLatestUploadInfo

pkgNumRevisions :: PkgInfo -> Int
pkgNumRevisions = Vec.length . pkgMetadataRevisions

pkgLatestTarball :: PkgInfo -> Maybe (PkgTarball, UploadInfo)
pkgLatestTarball pkginfo
  | Vec.null (pkgTarballRevisions pkginfo) = Nothing
  | otherwise = Just (Vec.last (pkgTarballRevisions pkginfo))

-- | The information held in a parsed .cabal file (used by cabal-install)
pkgDesc :: PkgInfo -> GenericPackageDescription
pkgDesc pkgInfo =
    case parsePackageDescription $ cabalFileString $ fst $ pkgLatestRevision pkgInfo of
      -- We only make PkgInfos with parsable pkgDatas, so if it
      -- doesn't parse then something has gone wrong.
      ParseFailed e -> error ("Internal error: " ++ show e)
      ParseOk _ x   -> x

deriveSafeCopy 4 'extension ''PkgInfo
deriveSafeCopy 2 'extension ''PkgTarball

instance MemSize PkgInfo where
    memSize (PkgInfo a b c) = memSize3 a b c

instance MemSize PkgTarball where
    memSize (PkgTarball a b) = memSize2 a b

--------------------------
-- Old SafeCopy versions
--

data PkgInfo_v2 = PkgInfo_v2 {
    v2_pkgInfoId            :: !PackageIdentifier,
    v2_pkgMetadataRevisions :: !(Vec.Vector (CabalFileText, UploadInfo)),
    v2_pkgTarballRevisions  :: !(Vec.Vector (PkgTarball, UploadInfo))
}

deriveSafeCopy 3 'extension ''PkgInfo_v2

instance Migrate PkgInfo where
    type MigrateFrom PkgInfo = PkgInfo_v2
    migrate (PkgInfo_v2 {..}) =
      PkgInfo {
        pkgInfoId            = v2_pkgInfoId,
        -- Fix the previous migration that put all the revisions in reverse.
        -- We'll sort by upload time, in case there have been any new
        -- revisions added in the meantime.
        pkgMetadataRevisions = Vec.fromList
                             $ sortBy (comparing (fst.snd))
                             $ Vec.toList v2_pkgMetadataRevisions,
        pkgTarballRevisions  = v2_pkgTarballRevisions
      }

data PkgInfo_v1 = PkgInfo_v1 {
    v1_pkgInfoId     :: !PackageIdentifier,
    v1_pkgData       :: !CabalFileText,
    v1_pkgTarball    :: ![(PkgTarball, UploadInfo)],
    v1_pkgDataOld    :: ![(CabalFileText, UploadInfo)],
    v1_pkgUploadData :: !UploadInfo
}

deriveSafeCopy 2 'extension ''PkgInfo_v1

instance Migrate PkgInfo_v2 where
    type MigrateFrom PkgInfo_v2 = PkgInfo_v1
    migrate (PkgInfo_v1 {..}) =
      PkgInfo_v2 {
        v2_pkgInfoId            = v1_pkgInfoId,
        -- This migration was wrong. It put the revisions in the wrong order.
        -- This mistake is corrected in the subsequent migration.
        v2_pkgMetadataRevisions = Vec.fromList ((v1_pkgData, v1_pkgUploadData)
                                                :v1_pkgDataOld),
        v2_pkgTarballRevisions  = Vec.fromList v1_pkgTarball
      }

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

instance Migrate PkgInfo_v1 where
    type MigrateFrom PkgInfo_v1 = PkgInfo_v0
    migrate (PkgInfo_v0 a b c d e) =
      PkgInfo_v1 (migrate a) b
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
