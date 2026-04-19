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
import Data.Time (UTCTime(..), getCurrentTime)
import Data.Acid.Compat


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

------------------------------
-- IsAcidic machinery
--
-- See Note [Acid Migration] in "Data.Acid.Compat"
-- original module name was Distribution.Server.Features.UserNotify"

--makeAcidic ''NotifyData [
--    --queries
--    'getNotifyData,
--    'lookupNotifyPref,
--    'getNotifyTime,
--    --updates
--    'replaceNotifyData,
--    'addNotifyPref,
--    'setNotifyTime
--  ]

instance IsAcidic NotifyData where
  acidEvents
    = [QueryEvent
         (\ GetNotifyData -> getNotifyData) safeCopyMethodSerialiser,
       QueryEvent
         (\ (LookupNotifyPref arg_aDhJ) -> lookupNotifyPref arg_aDhJ)
         safeCopyMethodSerialiser,
       QueryEvent
         (\ GetNotifyTime -> getNotifyTime) safeCopyMethodSerialiser,
       UpdateEvent
         (\ (ReplaceNotifyData arg_aDhK) -> replaceNotifyData arg_aDhK)
         safeCopyMethodSerialiser,
       UpdateEvent
         (\ (AddNotifyPref arg_aDhL arg_aDhM)
            -> addNotifyPref arg_aDhL arg_aDhM)
         safeCopyMethodSerialiser,
       UpdateEvent
         (\ (SetNotifyTime arg_aDhN) -> setNotifyTime arg_aDhN)
         safeCopyMethodSerialiser]
data GetNotifyData = GetNotifyData
instance SafeCopy GetNotifyData where
  putCopy GetNotifyData = contain (do return ())
  getCopy = contain (return GetNotifyData)
  errorTypeName _ = "Data.SafeCopy.SafeCopy.SafeCopy GetNotifyData"
instance Data.Acid.Compat.Method GetNotifyData where
  type MethodResult GetNotifyData = NotifyData
  type MethodState GetNotifyData = NotifyData
  methodTag = movedMethodTag "Distribution.Server.Features.UserNotify"
instance QueryEvent GetNotifyData
newtype LookupNotifyPref = LookupNotifyPref UserId
instance SafeCopy LookupNotifyPref where
  putCopy (LookupNotifyPref arg_aDgW)
    = contain
        (do safePut arg_aDgW
            return ())
  getCopy = contain (return LookupNotifyPref <*> safeGet)
  errorTypeName _
    = "Data.SafeCopy.SafeCopy.SafeCopy LookupNotifyPref"
instance Data.Acid.Compat.Method LookupNotifyPref where
  type MethodResult LookupNotifyPref = Maybe NotifyPref
  type MethodState LookupNotifyPref = NotifyData
  methodTag = movedMethodTag "Distribution.Server.Features.UserNotify"
instance QueryEvent LookupNotifyPref
data GetNotifyTime = GetNotifyTime
instance SafeCopy GetNotifyTime where
  putCopy GetNotifyTime = contain (do return ())
  getCopy = contain (return GetNotifyTime)
  errorTypeName _ = "Data.SafeCopy.SafeCopy.SafeCopy GetNotifyTime"
instance Data.Acid.Compat.Method GetNotifyTime where
  type MethodResult GetNotifyTime = UTCTime
  type MethodState GetNotifyTime = NotifyData
  methodTag = movedMethodTag "Distribution.Server.Features.UserNotify"
instance QueryEvent GetNotifyTime
newtype ReplaceNotifyData = ReplaceNotifyData NotifyData
instance SafeCopy ReplaceNotifyData where
  putCopy (ReplaceNotifyData arg_aDh5)
    = contain
        (do safePut arg_aDh5
            return ())
  getCopy = contain (return ReplaceNotifyData <*> safeGet)
  errorTypeName _
    = "Data.SafeCopy.SafeCopy.SafeCopy ReplaceNotifyData"
instance Data.Acid.Compat.Method ReplaceNotifyData where
  type MethodResult ReplaceNotifyData = ()
  type MethodState ReplaceNotifyData = NotifyData
  methodTag = movedMethodTag "Distribution.Server.Features.UserNotify"
instance UpdateEvent ReplaceNotifyData
data AddNotifyPref = AddNotifyPref UserId NotifyPref
instance SafeCopy AddNotifyPref where
  putCopy (AddNotifyPref arg_aDha arg_aDhb)
    = contain
        (do safePut arg_aDha
            safePut arg_aDhb
            return ())
  getCopy = contain ((return AddNotifyPref <*> safeGet) <*> safeGet)
  errorTypeName _ = "Data.SafeCopy.SafeCopy.SafeCopy AddNotifyPref"
instance Data.Acid.Compat.Method AddNotifyPref where
  type MethodResult AddNotifyPref = ()
  type MethodState AddNotifyPref = NotifyData
  methodTag = movedMethodTag "Distribution.Server.Features.UserNotify"
instance UpdateEvent AddNotifyPref
newtype SetNotifyTime = SetNotifyTime UTCTime
instance SafeCopy SetNotifyTime where
  putCopy (SetNotifyTime arg_aDhg)
    = contain
        (do safePut arg_aDhg
            return ())
  getCopy = contain (return SetNotifyTime <*> safeGet)
  errorTypeName _ = "Data.SafeCopy.SafeCopy.SafeCopy SetNotifyTime"
instance Data.Acid.Compat.Method SetNotifyTime where
  type MethodResult SetNotifyTime = ()
  type MethodState SetNotifyTime = NotifyData
  methodTag = movedMethodTag "Distribution.Server.Features.UserNotify"
instance UpdateEvent SetNotifyTime

