-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.Packages.Utils.Old
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
-----------------------------------------------------------------------------
module Distribution.Server.Packages.Utils.Acid where

import Distribution.Server.Packages.Types

import Distribution.Server.Users.Types (UserId(..))
import Distribution.Server.Util.Parse (unpackUTF8Strict)

import Distribution.PackageDescription
         ( GenericPackageDescription(..))
import Distribution.PackageDescription.Parsec
         ( parseGenericPackageDescription, runParseResult )

import Data.Time.Clock (UTCTime(..))
import qualified Data.Vector          as Vec

cabalFileString :: CabalFileText -> String
cabalFileString = unpackUTF8Strict . cabalFileByteString

pkgOriginalRevision :: PkgInfo -> (CabalFileText, OldUploadInfo)
pkgOriginalRevision = Vec.head . pkgMetadataRevisions

pkgOriginalUploadInfo :: PkgInfo -> OldUploadInfo
pkgOriginalUploadInfo = snd . pkgOriginalRevision

pkgOriginalUploadTime :: PkgInfo -> UTCTime
pkgOriginalUploadTime = fst . pkgOriginalUploadInfo

pkgOriginalUploadUser :: PkgInfo -> UserId
pkgOriginalUploadUser = snd . pkgOriginalUploadInfo

pkgLatestRevision :: PkgInfo -> (CabalFileText, OldUploadInfo)
pkgLatestRevision = Vec.last . pkgMetadataRevisions

pkgSpecificRevision :: PkgInfo -> MetadataRevIx -> Maybe (CabalFileText, OldUploadInfo)
pkgSpecificRevision pkg (MetadataRevIx revno) = pkgMetadataRevisions pkg Vec.!? revno

pkgAllRevisionsCabalFiles :: PkgInfo -> [CabalFileText]
pkgAllRevisionsCabalFiles = fmap fst . Vec.toList . pkgMetadataRevisions

pkgSpecificTarball :: PkgInfo -> TarballRevIx -> Maybe (PkgTarball, OldUploadInfo)
pkgSpecificTarball pkg (TarballRevIx revno) = pkgTarballRevisions pkg Vec.!? revno

pkgAllTarballs :: PkgInfo -> [(PkgTarball, OldUploadInfo)]
pkgAllTarballs = Vec.toList . pkgTarballRevisions

pkgAllRevisionsUploadInfos :: PkgInfo -> [OldUploadInfo]
pkgAllRevisionsUploadInfos = fmap snd . Vec.toList . pkgMetadataRevisions

pkgLatestCabalFileText :: PkgInfo -> CabalFileText
pkgLatestCabalFileText = fst . pkgLatestRevision

pkgLatestUploadInfo :: PkgInfo -> OldUploadInfo
pkgLatestUploadInfo = snd . pkgLatestRevision

pkgLatestUploadTime :: PkgInfo -> UTCTime
pkgLatestUploadTime = fst . pkgLatestUploadInfo

pkgLatestUploadUser :: PkgInfo -> UserId
pkgLatestUploadUser = snd . pkgLatestUploadInfo

pkgNumRevisions :: PkgInfo -> Int
pkgNumRevisions = Vec.length . pkgMetadataRevisions

pkgMaxRevision :: PkgInfo -> MetadataRevIx
pkgMaxRevision = MetadataRevIx . subtract 1 . pkgNumRevisions

-- | The latest tarball for a package (if any)
--
-- For packages with a @.cabal@ file but no tarball we return 'Nothing'.
-- For other package we return the latest tarball, corresponding upload info
-- and revision number. The revision number will normally be 1, but may be
-- higher if more tarballs were uploaded for this package (on the central
-- Hackage server we disallow this).
pkgLatestTarball :: PkgInfo -> Maybe (PkgTarball, OldUploadInfo, Int)
pkgLatestTarball pkginfo =
   if Vec.null tarballs
     then Nothing
     else let (tarball, uploadInfo) = Vec.last tarballs
          in Just (tarball, uploadInfo, Vec.length tarballs - 1)
  where
    tarballs = pkgTarballRevisions pkginfo

-- | The information held in a parsed .cabal file (used by cabal-install)
pkgDesc :: PkgInfo -> GenericPackageDescription
pkgDesc pkgInfo =
    case runParseResult $ parseGenericPackageDescription $
         cabalFileByteString $ fst $
         pkgLatestRevision pkgInfo of
      -- We only make PkgInfos with parsable pkgDatas, so if it
      -- doesn't parse then something has gone wrong.
      (_, Left (_,es)) -> error ("Internal error: " ++ show es)
      (_, Right x)     -> x

-- | The information held in a parsed .cabal file, with nicer failure
pkgDescMaybe :: PkgInfo -> Maybe GenericPackageDescription
pkgDescMaybe pkgInfo =
    case runParseResult $ parseGenericPackageDescription $
         cabalFileByteString $ fst $
         pkgLatestRevision pkgInfo of
      -- We only make PkgInfos with parsable pkgDatas, so if it
      -- doesn't parse then something has gone wrong.
      (_, Left (_, _es)) -> Nothing
      (_, Right x)     -> Just x

