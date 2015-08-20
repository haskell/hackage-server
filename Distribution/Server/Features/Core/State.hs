{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, BangPatterns #-}

module Distribution.Server.Features.Core.State (
    -- * DB state
    PackagesState(..)
  , initialPackagesState
    -- * DB transactions
    --
    -- NOTE: Explictly not exported: legacy transactions 'AddPackage' and
    -- 'AddPackageRevision' (see 'AddPackage2' and 'AddPackageRevision2').
  , AddOtherIndexEntry(..)
  , AddPackage2(..)
  , AddPackageRevision2(..)
  , AddPackageTarball(..)
  , DeletePackage(..)
  , GetPackagesState(..)
  , MigrateAddUpdateLog(..)
  , ReplacePackagesState(..)
  , SetPackageUploadTime(..)
  , SetPackageUploader(..)
  , UpdatePackageInfo(..)
  ) where

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
import Data.Ord (comparing)

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

-- | Initial packages state
--
-- NOTE: If we are starting from a fresh DB, obviously migration is not needed.
-- However, if we are not, but we _are_ starting from an initial DB value, this
-- must mean we are starting a server with an existing DB but no checkpoint. In
-- this case we might have old transactions to replay, so we might have to
-- migrate. The need for migration is indicated by having a 'Nothing' value for
-- the 'packageUpdateLog'.
--
-- If we failed to migrate these old transactions, two things would go wrong:
--
-- * We would add the 'CabalFileEntry's to the package log, but we would be
--   missing the corresponding TUF entries.
-- * Since the transaction has a 'PkgTarball' as argument, we would end up with
--   migrated 'PkgTarball's in the package DB (that is, 'PkgTarball_v2_v1's),
--   BUT with a non-'Nothing' update log, so we would fail to notice on start-up
--   that we need to migrate.
initialPackagesState :: Bool -> PackagesState
initialPackagesState freshDB = PackagesState {
    packageIndex     = mempty,
    packageUpdateLog = if freshDB then Just mempty else Nothing
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
            !pkgentry   = CabalFileEntry pkgid 0 timestamp username
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
            !pkgentry   = CabalFileEntry pkgid newrevision timestamp username
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
            !pkgentry   = CabalFileEntry pkgid 0 timestamp username
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

updatePackageInfo :: PackageId -> PkgInfo -> Update PackagesState ()
updatePackageInfo pkgid pkginfo = void $ alterPackage pkgid (const pkginfo)

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

-- | Add entries into the index (other than cabal files)
addOtherIndexEntry :: TarIndexEntry -> Update PackagesState ()
addOtherIndexEntry !extraentry = do
    PackagesState pkgindex updatelog <- State.get
    let updatelog' = fmap (Seq.|> extraentry) updatelog
    State.put $! PackagesState pkgindex updatelog'

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

-- | Construct the initial update log (migration)
--
-- NOTES:
--
-- * This creates only the cabal file entries and TUF entries; the extra entries
--   for preferred-versions are created by 'ephemeralPrefsMigration' in the
--   'PreferredVersions' feature.
-- * We do this here rather than in the security feature so that this happens
--   before we set up the hook to update the hackage on package changes
--   (otherwise the index would continuously be updated during this process,
--   which would be far too expensive).
-- * Moreover, this allows us to order the index such that the TUF files are
--   interleaved with the cabal files.
-- * We use a stable sort to make sure that when timestamps are equal,
--   we keep .cabal files together with their TUF .json counterparts.
initialUpdateLog :: Users -> PackageIndex PkgInfo -> Seq TarIndexEntry
initialUpdateLog users pkgs =
      Seq.sortBy (comparing entryTimestamp) -- stable sort; see above
    $ Seq.fromList
    $ concatMap entriesForPackage
    $ PackageIndex.allPackages pkgs
  where
    entriesForPackage :: PkgInfo -> [TarIndexEntry]
    entriesForPackage pkgInfo = concat [
          map (entryCabal pkgId) $ vecToList (pkgMetadataRevisions pkgInfo)
        , map (entryTUF   pkgId) $ vecToList (pkgTarballRevisions  pkgInfo)
        ]
      where
        pkgId = pkgInfoId pkgInfo

    entryCabal :: PackageId -> (Int, (a, UploadInfo)) -> TarIndexEntry
    entryCabal pkgId (revNo, (_cabalFile, (timestamp, uid))) =
        CabalFileEntry pkgId revNo timestamp (uidToName uid)

    entryTUF :: PackageId -> (Int, (a, UploadInfo)) -> TarIndexEntry
    entryTUF pkgId (revNo, (_tarball, (timestamp, _uid))) =
        MetadataEntry pkgId revNo timestamp

    uidToName :: UserId -> UserName
    uidToName uid = maybe (UserName "") userName (lookupUserId uid users)

    entryTimestamp :: TarIndexEntry -> UTCTime
    entryTimestamp (CabalFileEntry _ _ timestamp _) = timestamp
    entryTimestamp (MetadataEntry  _ _ timestamp  ) = timestamp
    entryTimestamp (ExtraEntry     _ _ timestamp  ) = timestamp

    vecToList :: Vec.Vector a -> [(Int, a)]
    vecToList = zip [0..] . Vec.toList

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
                           ,'updatePackageInfo
                           ,'addOtherIndexEntry
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
