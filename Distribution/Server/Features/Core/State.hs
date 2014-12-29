{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, BangPatterns #-}

module Distribution.Server.Features.Core.State where

import Distribution.Package
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Framework.MemSize

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Monoid
import Data.Time (UTCTime)
import qualified Data.Vector as Vec
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (maybeToList)


---------------------------------- Index of metadata and tarballs
data PackagesState = PackagesState {
    packageList  :: !(PackageIndex PkgInfo)
  }
  deriving (Eq, Typeable, Show)

deriveSafeCopy 0 'base ''PackagesState

instance MemSize PackagesState where
    memSize (PackagesState a) = 2 + memSize a

initialPackagesState :: PackagesState
initialPackagesState = PackagesState {
    packageList = mempty
  }

addPackage :: PackageId -> CabalFileText -> UploadInfo -> Maybe PkgTarball
           -> Update PackagesState (Maybe PkgInfo)
addPackage pkgid cabalfile uploadinfo mtarball = do 
    PackagesState pkgindex <- State.get
    case PackageIndex.lookupPackageId pkgindex pkgid of
      Just _  -> return Nothing
      Nothing -> do
        let !pkginfo = PkgInfo {
              pkgInfoId            = pkgid,
              pkgMetadataRevisions = Vec.singleton (cabalfile, uploadinfo),
              pkgTarballRevisions  = case mtarball of
                                       Nothing      -> Vec.empty
                                       Just tarball -> Vec.singleton
                                                         (tarball, uploadinfo)
            }
            pkgindex' = PackageIndex.insert pkginfo pkgindex
        State.put $! PackagesState pkgindex'
        return (Just pkginfo)

deletePackage :: PackageId -> Update PackagesState (Maybe PkgInfo)
deletePackage pkgid = do
    PackagesState pkgindex <- State.get
    case PackageIndex.lookupPackageId pkgindex pkgid of
      Nothing      -> return Nothing
      Just pkginfo -> do
        let pkgindex' = PackageIndex.deletePackageId pkgid pkgindex
        State.put $! PackagesState pkgindex'
        return (Just pkginfo)

addPackageRevision :: PackageId -> CabalFileText -> UploadInfo
                   -> Update PackagesState (Maybe PkgInfo, PkgInfo)
addPackageRevision pkgid cabalfile uploadinfo = do
    PackagesState pkgindex <- State.get
    case PackageIndex.lookupPackageId pkgindex pkgid of
      Just pkginfo -> do
        let !pkginfo' = pkginfo {
              pkgMetadataRevisions = pkgMetadataRevisions pkginfo
                                     `Vec.snoc` (cabalfile, uploadinfo)
            }
            pkgindex' = PackageIndex.insert pkginfo' pkgindex
        State.put $! PackagesState pkgindex'
        return (Just pkginfo, pkginfo')
      Nothing -> do
        let !pkginfo = PkgInfo {
              pkgInfoId            = pkgid,
              pkgMetadataRevisions = Vec.singleton (cabalfile, uploadinfo),
              pkgTarballRevisions  = Vec.empty
            }
            pkgindex' = PackageIndex.insert pkginfo pkgindex
        State.put $! PackagesState pkgindex'
        return (Nothing, pkginfo)

addPackageTarball :: PackageId -> PkgTarball -> UploadInfo
                  -> Update PackagesState (Maybe (PkgInfo, PkgInfo))
addPackageTarball pkgid tarball uploadinfo =
    alterPackage pkgid $ \pkginfo ->
      pkginfo {
        pkgTarballRevisions = pkgTarballRevisions pkginfo
                              `Vec.snoc` (tarball, uploadinfo)
      }

setPackageUploader :: PackageId -> UserId
                   -> Update PackagesState (Maybe (PkgInfo, PkgInfo))
setPackageUploader pkgid uid =
    alterPackage pkgid $ \pkginfo ->
      let (cabalfile, (time, _uid)) = pkgLatestRevision pkginfo in
      pkginfo {
        pkgMetadataRevisions = Vec.init (pkgMetadataRevisions pkginfo)
                               `Vec.snoc` (cabalfile, (time, uid))
      }

setPackageUploadTime :: PackageId -> UTCTime
                     -> Update PackagesState (Maybe (PkgInfo, PkgInfo))
setPackageUploadTime pkgid time =
    alterPackage pkgid $ \pkginfo ->
      let (cabalfile, (_time, uid)) = pkgLatestRevision pkginfo in
      pkginfo {
        pkgMetadataRevisions = Vec.init (pkgMetadataRevisions pkginfo)
                               `Vec.snoc` (cabalfile, (time, uid))
      }

alterPackage :: PackageId -> (PkgInfo -> PkgInfo)
             -> Update PackagesState (Maybe (PkgInfo, PkgInfo))
alterPackage pkgid alter = do
    PackagesState pkgindex <- State.get
    case PackageIndex.lookupPackageId pkgindex pkgid of
      Nothing      -> return Nothing
      Just pkginfo -> do
        let !pkginfo' = alter pkginfo
            pkgindex' = PackageIndex.insert pkginfo' pkgindex
        State.put $! PackagesState pkgindex'
        return (Just (pkginfo, pkginfo'))

-- |Replace all existing packages and reports
replacePackagesState :: PackagesState -> Update PackagesState ()
replacePackagesState = State.put

getPackagesState :: Query PackagesState PackagesState
getPackagesState = ask

makeAcidic ''PackagesState ['getPackagesState
                           ,'replacePackagesState
                           ,'addPackage
                           ,'deletePackage
                           ,'addPackageRevision
                           ,'addPackageTarball
                           ,'setPackageUploader
                           ,'setPackageUploadTime
                           ]

