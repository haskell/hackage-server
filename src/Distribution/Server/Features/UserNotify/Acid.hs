{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell,
             RankNTypes, NamedFieldPuns, RecordWildCards, BangPatterns,
             DefaultSignatures, OverloadedStrings #-}
module Distribution.Server.Features.UserNotify.Acid where

import Distribution.Server.Features.UserNotify.Types
import Prelude hiding (lookup)

import Distribution.Server.Users.Types (UserId)

import Distribution.Server.Framework

import qualified Data.Map as Map

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.SafeCopy (Migrate(migrate), MigrateFrom, base, deriveSafeCopy, extension)
import Data.Time (UTCTime(..), getCurrentTime)


-------------------------
-- Types of stored data
--
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

data NotifyData = NotifyData {unNotifyData :: (Map.Map UserId NotifyPref, UTCTime)} deriving (Eq, Show)

instance MemSize NotifyData where memSize (NotifyData x) = memSize x

emptyNotifyData :: IO NotifyData
emptyNotifyData = getCurrentTime >>= \x-> return (NotifyData (Map.empty, x))

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
$(deriveSafeCopy 0 'base ''NotifyData)

------------------------------
-- State queries and updates
--

getNotifyData :: Query NotifyData NotifyData
getNotifyData = ask

replaceNotifyData :: NotifyData -> Update NotifyData ()
replaceNotifyData = put

getNotifyTime :: Query NotifyData UTCTime
getNotifyTime = fmap (snd . unNotifyData) ask

setNotifyTime :: UTCTime -> Update NotifyData ()
setNotifyTime t = do
    NotifyData (m,_) <- get
    put $! NotifyData (m,t)

lookupNotifyPref :: UserId -> Query NotifyData (Maybe NotifyPref)
lookupNotifyPref uid = do
    NotifyData (m,_) <- ask
    return $! Map.lookup uid m

addNotifyPref :: UserId -> NotifyPref -> Update NotifyData ()
addNotifyPref uid info = do
    NotifyData (m,t) <- get
    put $! NotifyData (Map.insert uid info m,t)

makeAcidic ''NotifyData [
    --queries
    'getNotifyData,
    'lookupNotifyPref,
    'getNotifyTime,
    --updates
    'replaceNotifyData,
    'addNotifyPref,
    'setNotifyTime
  ]

