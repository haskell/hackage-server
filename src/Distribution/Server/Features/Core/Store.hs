{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.Core.Store
  ( Backend(..)
  , Store(..)
  , RecentPackageVersions(..)
  ) where

import Distribution.Server.Framework (AbstractStateComponent, MemSize(..), memSize2)
import Distribution.Server.Features.Core.State (PackagesState)
import Distribution.Server.Packages.Index (TarIndexEntry)
import Distribution.Server.Packages.Types
import Distribution.Server.Users.Types (UserId, UserName)
import Distribution.Server.Users.Users (Users)

import Distribution.Package (PackageId, PackageName)

import Control.Monad.Trans (MonadIO)
import Data.Map (Map)
import Data.Time.Clock (UTCTime)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    getPackagesState       :: forall m. MonadIO m => m PackagesState
  , lookupPackageName      :: forall m. MonadIO m => PackageName -> m [PkgInfo]
  , lookupPackageId        :: forall m. MonadIO m => PackageId -> m (Maybe PkgInfo)
    -- | The given packages that exist, with all their versions.
  , lookupPackageNames     :: forall m. MonadIO m => [PackageName] -> m (Map PackageName [PkgInfo])
  , latestPackages         :: forall m. MonadIO m => m [PkgInfo]
    -- | Every package name, in ascending order.
  , allPackageNames        :: forall m. MonadIO m => m [PackageName]
  , allPackageIds          :: forall m. MonadIO m => m [PackageId]
    -- | The number of distinct package names, not package versions.
  , countPackages          :: forall m. MonadIO m => m Int
    -- | Package versions whose latest upload or revision is after the first
    -- time and no later than the second.
  , latestUploadsBetween   :: forall m. MonadIO m => UTCTime -> UTCTime -> m [PkgInfo]
  , recentPackageVersions  :: forall m. MonadIO m => m RecentPackageVersions
  , addPackage             :: forall m. MonadIO m => PkgInfo -> UploadInfo -> UserName -> [TarIndexEntry] -> m Bool
  , deletePackage          :: forall m. MonadIO m => PackageId -> m (Maybe PkgInfo)
  , addPackageRevision     :: forall m. MonadIO m => PackageId -> CabalFileText -> UploadInfo -> UserName -> m (Maybe PkgInfo, PkgInfo)
  , addPackageTarball      :: forall m. MonadIO m => PackageId -> PkgTarball -> UploadInfo -> m (Maybe (PkgInfo, PkgInfo))
  , setPackageUploader     :: forall m. MonadIO m => PackageId -> UserId -> m (Maybe (PkgInfo, PkgInfo))
  , setPackageUploadTime   :: forall m. MonadIO m => PackageId -> UTCTime -> m (Maybe (PkgInfo, PkgInfo))
  , addOtherIndexEntry     :: forall m. MonadIO m => TarIndexEntry -> m ()
  , migrateAddUpdateLog    :: forall m. MonadIO m => Users -> m ()
  , migratePackageTarballs :: IO ()
  , createStoreCheckpoint  :: IO ()
  }

-- | Package versions ordered for the recent uploads and recent revisions
-- lists, both read from the same state of the package database.
data RecentPackageVersions = RecentPackageVersions {
    -- | Every package version, by original upload time, newest first.
    recentPackageVersionsUploads   :: [PkgInfo]
    -- | Package versions with more than one revision, by latest revision
    -- time, newest first.
  , recentPackageVersionsRevisions :: [PkgInfo]
  }

instance MemSize RecentPackageVersions where
  memSize (RecentPackageVersions uploads revisions) = memSize2 uploads revisions
