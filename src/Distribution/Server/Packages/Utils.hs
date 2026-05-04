module Distribution.Server.Packages.Utils where

import Distribution.Server.Packages.Types

import Distribution.Server.Users.Types (UserId(..))

import Distribution.PackageDescription
         ( GenericPackageDescription(..))
import Distribution.PackageDescription.Parsec
         ( parseGenericPackageDescription, runParseResult )

import Data.Time.Clock (UTCTime(..))
import qualified Data.Vector as Vec

data MetadataRevision = MetadataRevision
  { metaRevCabalFile :: CabalFileText
  , metaRevInfo :: UploadInfo
  }

data UploadInfo = UploadInfo
  { uploadInfoTime :: UTCTime
  , uploadInfoUser :: UserId
  }

pkgOriginalRevision :: PkgInfo -> MetadataRevision
pkgOriginalRevision = fromOldMetadataRev . Vec.head . pkgMetadataRevisions

pkgOriginalUploadInfo :: PkgInfo -> UploadInfo
pkgOriginalUploadInfo = metaRevInfo . pkgOriginalRevision

pkgOriginalUploadTime :: PkgInfo -> UTCTime
pkgOriginalUploadTime = uploadInfoTime . pkgOriginalUploadInfo

pkgOriginalUploadUser :: PkgInfo -> UserId
pkgOriginalUploadUser = uploadInfoUser . pkgOriginalUploadInfo

pkgLatestRevision :: PkgInfo -> MetadataRevision
pkgLatestRevision = fromOldMetadataRev . Vec.last . pkgMetadataRevisions

pkgSpecificRevision :: PkgInfo -> MetadataRevIx -> Maybe MetadataRevision
pkgSpecificRevision pkg (MetadataRevIx revno)
  = fmap fromOldMetadataRev
  $ pkgMetadataRevisions pkg Vec.!? revno

pkgAllRevisionsCabalFiles :: PkgInfo -> [CabalFileText]
pkgAllRevisionsCabalFiles = fmap fst . Vec.toList . pkgMetadataRevisions

pkgSpecificTarball :: PkgInfo -> TarballRevIx -> Maybe (PkgTarball, UploadInfo)
pkgSpecificTarball pkg (TarballRevIx revno) = fmap (fmap fromOldUploadInfo) $ pkgTarballRevisions pkg Vec.!? revno

pkgAllTarballs :: PkgInfo -> [(PkgTarball, UploadInfo)]
pkgAllTarballs = fmap (fmap fromOldUploadInfo) . Vec.toList . pkgTarballRevisions

pkgAllRevisionsUploadInfos :: PkgInfo -> [UploadInfo]
pkgAllRevisionsUploadInfos = fmap (fromOldUploadInfo . snd) . Vec.toList . pkgMetadataRevisions

pkgLatestCabalFileText :: PkgInfo -> CabalFileText
pkgLatestCabalFileText = metaRevCabalFile . pkgLatestRevision

pkgLatestUploadInfo :: PkgInfo -> UploadInfo
pkgLatestUploadInfo = metaRevInfo . pkgLatestRevision

pkgLatestUploadTime :: PkgInfo -> UTCTime
pkgLatestUploadTime = uploadInfoTime . pkgLatestUploadInfo

pkgLatestUploadUser :: PkgInfo -> UserId
pkgLatestUploadUser = uploadInfoUser . pkgLatestUploadInfo

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
pkgLatestTarball :: PkgInfo -> Maybe (PkgTarball, UploadInfo, Int)
pkgLatestTarball pkginfo =
   if Vec.null tarballs
     then Nothing
     else let (tarball, (when, who)) = Vec.last tarballs
          in Just (tarball, UploadInfo when who, Vec.length tarballs - 1)
  where
    tarballs = pkgTarballRevisions pkginfo

-- | The information held in a parsed .cabal file (used by cabal-install)
pkgDesc :: MetadataRevision -> GenericPackageDescription
pkgDesc = either (error . mappend "Internal error: ") id . pkgDescImpl

-- | The information held in a parsed .cabal file, with nicer failure
pkgDescMaybe :: MetadataRevision -> Maybe GenericPackageDescription
pkgDescMaybe = either (const Nothing) Just . pkgDescImpl


-- | The information held in a parsed .cabal file, with nicer failure
pkgDescImpl :: MetadataRevision -> Either String GenericPackageDescription
pkgDescImpl rev =
    case runParseResult $ parseGenericPackageDescription $
         cabalFileByteString $ metaRevCabalFile rev of
      -- We only make PkgInfos with parsable pkgDatas, so if it
      -- doesn't parse then something has gone wrong.
      (_, Left (_, es)) -> Left $ show es
      (_, Right x)     -> Right x

fromOldUploadInfo :: OldUploadInfo -> UploadInfo
fromOldUploadInfo = uncurry UploadInfo

fromOldMetadataRev :: (CabalFileText, OldUploadInfo) -> MetadataRevision
fromOldMetadataRev (cabal, oui) = MetadataRevision cabal $ fromOldUploadInfo oui
