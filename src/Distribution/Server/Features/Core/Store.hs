{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.Core.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Framework (AbstractStateComponent)
import Distribution.Server.Features.Core.State (PackagesState)
import Distribution.Server.Packages.Index (TarIndexEntry)
import Distribution.Server.Packages.Types
import Distribution.Server.Users.Types (UserId, UserName)
import Distribution.Server.Users.Users (Users)

import Distribution.Package (PackageId, PackageName)

import Control.Monad.Trans (MonadIO)
import Data.Time.Clock (UTCTime)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    getPackagesState       :: forall m. MonadIO m => m PackagesState
  , lookupPackageName      :: forall m. MonadIO m => PackageName -> m [PkgInfo]
  , lookupPackageId        :: forall m. MonadIO m => PackageId -> m (Maybe PkgInfo)
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
