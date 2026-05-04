{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell,
             RankNTypes, NamedFieldPuns, RecordWildCards, BangPatterns,
             DefaultSignatures, OverloadedStrings #-}
module Distribution.Server.Features.UserNotify.Types where


import Prelude hiding (lookup)
import Distribution.Pretty

import Distribution.Server.Features.Tags.Types
import Distribution.Server.Framework
import Distribution.Server.Packages.Types (OldUploadInfo, PkgInfo)
import Distribution.Server.Users.Types (UserId)

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Hashable (Hashable(..))
import Data.SafeCopy (base, deriveSafeCopy)
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
      , notifyRevisions :: [OldUploadInfo]
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

