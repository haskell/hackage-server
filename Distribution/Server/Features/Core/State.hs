{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, BangPatterns #-}

module Distribution.Server.Features.Core.State where

import Distribution.Package
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Index
import Distribution.Server.Users.Types (UserId, UserName(..), UserInfo(..))
import Distribution.Server.Users.Users (Users, lookupUserId)
import Distribution.Server.Framework.MemSize

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (Migrate(..), base, extension, deriveSafeCopy)
import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Monoid
import Data.Time (UTCTime)
import qualified Data.Vector as Vec
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)


---------------------------------- Index of metadata and tarballs
data PackagesState = PackagesState {
    packageIndex      :: !(PackageIndex PkgInfo),
    packageUpdateLog  :: !(Maybe (Seq TarIndexEntry))
    -- for the moment the update log is a Maybe, to help with the transition
    -- we can change that later
  }
  deriving (Eq, Typeable, Show)

deriveSafeCopy 1 'extension ''PackagesState

--TODO: 

instance MemSize PackagesState where
    memSize (PackagesState a b) = 2 + memSize2 a b

initialPackagesState :: PackagesState
initialPackagesState = PackagesState {
    packageIndex     = mempty,
    packageUpdateLog = Just mempty
  }

-- old v0 transaction
addPackage :: PackageId -> CabalFileText -> UploadInfo
           -> Maybe PkgTarball
           -> Update PackagesState (Maybe PkgInfo)
addPackage pkgid cabalfile uploadinfo mtarball =
    addPackage2 pkgid cabalfile uploadinfo (UserName "") mtarball

-- current transaction (adds username)
addPackage2 :: PackageId -> CabalFileText -> UploadInfo -> UserName
            -> Maybe PkgTarball
            -> Update PackagesState (Maybe PkgInfo)
addPackage2 pkgid cabalfile uploadinfo@(timestamp, _uid) username mtarball = do

    PackagesState pkgindex updatelog <- State.get
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
            pkgindex'   = PackageIndex.insert pkginfo pkgindex
            !pkgentry   = PackageEntry pkgid 0 timestamp username
            updatelog'  = fmap (Seq.|> pkgentry) updatelog
        State.put $! PackagesState pkgindex' updatelog'
        return (Just pkginfo)

deletePackage :: PackageId -> Update PackagesState (Maybe PkgInfo)
deletePackage pkgid = do
    PackagesState pkgindex updatelog <- State.get
    case PackageIndex.lookupPackageId pkgindex pkgid of
      Nothing      -> return Nothing
      Just pkginfo -> do
        let pkgindex' = PackageIndex.deletePackageId pkgid pkgindex
        --TODO: reset and rebuild the update log, or at least note that
        -- it has changed, since it'll need to be recompressed
        State.put $! PackagesState pkgindex' updatelog
        return (Just pkginfo)

addPackageRevision :: PackageId -> CabalFileText -> UploadInfo
                   -> Update PackagesState (Maybe PkgInfo, PkgInfo)
addPackageRevision pkgid cabalfile uploadinfo =
    addPackageRevision2 pkgid cabalfile uploadinfo (UserName "")

addPackageRevision2 :: PackageId -> CabalFileText -> UploadInfo -> UserName
                    -> Update PackagesState (Maybe PkgInfo, PkgInfo)
addPackageRevision2 pkgid cabalfile uploadinfo@(timestamp, _uid) username = do
    PackagesState pkgindex updatelog <- State.get
    case PackageIndex.lookupPackageId pkgindex pkgid of
      Just pkginfo -> do
        let !pkginfo' = pkginfo {
              pkgMetadataRevisions = pkgMetadataRevisions pkginfo
                                     `Vec.snoc` (cabalfile, uploadinfo)
            }
            pkgindex'   = PackageIndex.insert pkginfo' pkgindex
            newrevision = Vec.length (pkgMetadataRevisions pkginfo)
            !pkgentry   = PackageEntry pkgid newrevision timestamp username
            updatelog'  = fmap (Seq.|> pkgentry) updatelog
        State.put $! PackagesState pkgindex' updatelog'
        return (Just pkginfo, pkginfo')
      Nothing -> do
        let !pkginfo = PkgInfo {
              pkgInfoId            = pkgid,
              pkgMetadataRevisions = Vec.singleton (cabalfile, uploadinfo),
              pkgTarballRevisions  = Vec.empty
            }
            pkgindex'   = PackageIndex.insert pkginfo pkgindex
            !pkgentry   = PackageEntry pkgid 0 timestamp username
            updatelog'  = fmap (Seq.|> pkgentry) updatelog
        State.put $! PackagesState pkgindex' updatelog'
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
    PackagesState pkgindex updatelog <- State.get
    case PackageIndex.lookupPackageId pkgindex pkgid of
      Nothing      -> return Nothing
      Just pkginfo -> do
        let !pkginfo' = alter pkginfo
            pkgindex' = PackageIndex.insert pkginfo' pkgindex
        State.put $! PackagesState pkgindex' updatelog
        return (Just (pkginfo, pkginfo'))

-- |Replace all existing packages and reports
replacePackagesState :: PackagesState -> Update PackagesState ()
replacePackagesState = State.put

getPackagesState :: Query PackagesState PackagesState
getPackagesState = ask

migrateAddUpdateLog :: Users -> Update PackagesState ()
migrateAddUpdateLog users = do
    PackagesState pkgindex _ <- State.get
    let !updatelog = initialUpdateLog users pkgindex
    State.put $! PackagesState pkgindex (Just updatelog)

initialUpdateLog :: Users -> PackageIndex PkgInfo -> Seq TarIndexEntry
initialUpdateLog users pkgs =
    Seq.fromList
      [ PackageEntry (packageId pkginfo) revno timestamp username
      | pkginfo <- PackageIndex.allPackages pkgs
      , (revno, pkgrev) <- zip [0..] (Vec.toList (pkgMetadataRevisions pkginfo))
        -- Note including all revisions for now, could include just last
      , let (_, (timestamp, uid)) = pkgrev
            username = maybe (UserName "") userName (lookupUserId uid users)
      ]

makeAcidic ''PackagesState ['getPackagesState
                           ,'replacePackagesState
                           ,'addPackage
                           ,'addPackage2
                           ,'deletePackage
                           ,'addPackageRevision
                           ,'addPackageRevision2
                           ,'addPackageTarball
                           ,'setPackageUploader
                           ,'setPackageUploadTime
                           ,'migrateAddUpdateLog
                           ]

------------------------------------------------------------------------------

data PackagesState_v0 = PackagesState_v0 !(PackageIndex PkgInfo)

deriveSafeCopy 0 'base ''PackagesState_v0

instance Migrate PackagesState where
    type MigrateFrom PackagesState = PackagesState_v0
    migrate (PackagesState_v0 pkgs) =
      PackagesState {
        packageIndex     = pkgs,
        packageUpdateLog = Nothing -- filled in by a more complex migration
      }

