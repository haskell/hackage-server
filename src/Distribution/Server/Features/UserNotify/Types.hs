{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell,
             RankNTypes, NamedFieldPuns, RecordWildCards, BangPatterns,
             DefaultSignatures, OverloadedStrings #-}
module Distribution.Server.Features.UserNotify.Types where


import Prelude hiding (lookup)
import Distribution.Pretty

import Distribution.Server.Features.Tags.Types
import Distribution.Server.Framework
import Distribution.Server.Packages.Types (UploadInfo, PkgInfo)
import Distribution.Server.Users.Types (UserId)

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Hashable (Hashable(..))
import Data.SafeCopy (Migrate(..), base, deriveSafeCopy, extension)
import Text.PrettyPrint hiding ((<>))
import Data.Set (Set)
import Data.Time (UTCTime)
import Data.Text (Text)
import Distribution.Package


data NotifyRevisionRange = NotifyAllVersions | NotifyNewestVersion | NoNotifyRevisions deriving (Bounded, Enum, Eq, Read, Show)
instance MemSize NotifyRevisionRange where
  memSize _ = 1

instance Pretty NotifyRevisionRange where
  pretty NoNotifyRevisions = text "No"
  pretty NotifyAllVersions = text "All Versions"
  pretty NotifyNewestVersion = text "Newest Version"

instance Hashable NotifyRevisionRange where
  hash = fromEnum
  hashWithSalt s x = s `hashWithSalt` hash x

data NotifyTriggerBounds
  = Always
  | BoundsOutOfRange
  | NewIncompatibility
  deriving (Bounded, Enum, Eq, Read, Show)

instance MemSize NotifyTriggerBounds where
  memSize _ = 1

instance Hashable NotifyTriggerBounds where
  hash = fromEnum
  hashWithSalt s x = s `hashWithSalt` hash x

$(deriveSafeCopy 0 'base ''NotifyTriggerBounds)
$(deriveSafeCopy 0 'base ''NotifyRevisionRange)

$(deriveJSON defaultOptions ''NotifyRevisionRange)
$(deriveJSON defaultOptions ''NotifyTriggerBounds)


data Notification
  = NotifyNewVersion
      { notifyPackageInfo :: PkgInfo
      }
  | NotifyNewRevision
      { notifyPackageId :: PackageId
      , notifyRevisions :: [UploadInfo]
      }
  | NotifyMaintainerUpdate
      { notifyMaintainerUpdateType :: NotifyMaintainerUpdateType
      , notifyUserActor :: UserId
      , notifyUserSubject :: UserId
      , notifyPackageName :: PackageName
      , notifyReason :: Text
      , notifyUpdatedAt :: UTCTime
      }
  | NotifyDocsBuild
      { notifyPackageId :: PackageId
      , notifyBuildSuccess :: Bool
      }
  | NotifyUpdateTags
      { notifyPackageName :: PackageName
      , notifyAddedTags :: Set Tag
      , notifyDeletedTags :: Set Tag
      }
  | NotifyDependencyUpdate
      { notifyPackageId :: PackageId
        -- ^ Dependency that was updated
      , notifyWatchedPackages :: [PackageId]
        -- ^ Packages maintained by user that depend on updated dep
      , notifyTriggerBounds :: NotifyTriggerBounds
      }
  | NotifyVouchingCompleted
  deriving (Show)

data NotifyMaintainerUpdateType = MaintainerAdded | MaintainerRemoved
  deriving (Show)


-- | The previous version of 'NotifyPref', kept to migrate old state.
data NotifyPref_v0 = NotifyPref_v0
                  {
                    v0notifyOptOut :: Bool,
                    v0notifyRevisionRange :: NotifyRevisionRange,
                    v0notifyUpload :: Bool,
                    v0notifyMaintainerGroup :: Bool,
                    v0notifyDocBuilderReport :: Bool,
                    v0notifyPendingTags :: Bool
                  }
                  deriving (Eq, Read, Show)
-- | Per-user notification preferences.
data NotifyPref = NotifyPref
                  {
                    notifyOptOut :: Bool,
                    notifyRevisionRange :: NotifyRevisionRange,
                    notifyUpload :: Bool,
                    notifyMaintainerGroup :: Bool,
                    notifyDocBuilderReport :: Bool,
                    notifyPendingTags :: Bool,
                    notifyDependencyForMaintained :: Bool,
                    notifyDependencyTriggerBounds :: NotifyTriggerBounds
                  }
                  deriving (Eq, Read, Show)

defaultNotifyPrefs :: NotifyPref
defaultNotifyPrefs = NotifyPref {
                       notifyOptOut = True, -- TODO when we're comfortable with this we can change to False.
                       notifyRevisionRange = NotifyAllVersions,
                       notifyUpload = True,
                       notifyMaintainerGroup = True,
                       notifyDocBuilderReport = True,
                       notifyPendingTags = True,
                       notifyDependencyForMaintained = True,
                       notifyDependencyTriggerBounds = NewIncompatibility
                     }

instance MemSize NotifyPref_v0 where memSize _ = memSize ((True,True,True),(True,True, True))
instance MemSize NotifyPref    where memSize NotifyPref{..} = memSize8 notifyOptOut notifyRevisionRange notifyUpload notifyMaintainerGroup
                                                                       notifyDocBuilderReport notifyPendingTags notifyDependencyForMaintained
                                                                       notifyDependencyTriggerBounds

$(deriveSafeCopy 0 'base ''NotifyPref_v0)

instance Migrate NotifyPref where
  type MigrateFrom NotifyPref = NotifyPref_v0
  migrate (NotifyPref_v0 f0 f1 f2 f3 f4 f5) =
    NotifyPref f0 f1 f2 f3 f4 f5
      False -- Users that already have opted in to notifications
            -- did so at at a time when it did not include
            -- reverse dependency emails.
            -- So let's assume they don't want these.
            -- Note that this differs from defaultNotifyPrefs.
      NewIncompatibility

$(deriveSafeCopy 1 'extension ''NotifyPref)
