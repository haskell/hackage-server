{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}
-- | Constructing TUF package metadata
module Distribution.Server.Packages.Metadata (
    computePkgMetadata
  ) where

-- Standard libraries
import qualified Data.Vector          as Vec
import qualified Data.ByteString.Lazy as BS.Lazy

-- Hackage
import Distribution.Server.Features.Security.FileInfo
import Distribution.Server.Features.Security.Layout
import Distribution.Server.Packages.Types

-- Cabal
import Distribution.Package

-- Hackage security
import qualified Hackage.Security.Server      as Sec
import qualified Hackage.Security.TUF.FileMap as Sec.FileMap

-- | Index entry for the TUF metadata for a package
--
-- Revisions numbers count from 0; we use the revision number as is for the
-- TUF file version.
computePkgMetadata :: PkgInfo   -- ^ Package
                   -> Int       -- ^ Tarball revision
                   -> (FilePath, BS.Lazy.ByteString)
computePkgMetadata pkg revNo = (inIndexPkgMetadata pkgId, raw)
  where
    tarballs     = pkgTarballRevisions pkg
    (tarball, _) = tarballs Vec.! revNo
    pkgId        = pkgInfoId pkg
    targets      = pkgTarballTargets revNo pkgId tarball
    signed       = Sec.withSignatures' [] targets
    raw          = Sec.renderJSON_NoLayout signed

pkgTarballTargets :: Int -> PackageIdentifier -> PkgTarball -> Sec.Targets
pkgTarballTargets revNo pkgId pkgTarball = Sec.Targets {
      targetsVersion     = Sec.FileVersion (fromIntegral revNo)
    , targetsExpires     = Sec.expiresNever
    , targetsTargets     = Sec.FileMap.fromList [
                               (inRepoPkgTarGz pkgId, secFileInfo pkgTarballGz)
                             ]
    , targetsDelegations = Nothing
    }
  where
   PkgTarball{..} = pkgTarball
