{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Server.Features.Vouch.State where

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime(..))

import Data.Acid.Compat
import Distribution.Server.Framework (MemSize(..), memSize2)
import Distribution.Server.Users.Types (UserId(..))

data VouchData =
  VouchData
    { vouches :: Map.Map UserId [(UserId, UTCTime)]
    , notNotified :: Set.Set UserId
    }
  deriving (Show, Eq)

instance MemSize VouchData where
  memSize (VouchData vouches notified) = memSize2 vouches notified

putVouch :: UserId -> (UserId, UTCTime) -> Update VouchData ()
putVouch vouchee (voucher, now) = do
  VouchData tbl notNotified <- get
  let oldMap = fromMaybe [] (Map.lookup vouchee tbl)
      newMap = (voucher, now) : oldMap
  put $ VouchData (Map.insert vouchee newMap tbl) notNotified

getVouchesFor :: UserId -> Query VouchData [(UserId, UTCTime)]
getVouchesFor needle = do
  VouchData tbl _notNotified <- ask
  pure . fromMaybe [] $ Map.lookup needle tbl

getVouchesData :: Query VouchData VouchData
getVouchesData = ask

replaceVouchesData :: VouchData -> Update VouchData ()
replaceVouchesData = put

$(deriveSafeCopy 0 'base ''VouchData)

------------------------------
-- IsAcidic machinery
--
-- See Note [Acid Migration] in "Data.Acid.Compat"
-- original module name was Distribution.Server.Features.Vouch"

-- makeAcidic ''VouchData
--   [ 'putVouch
--   , 'getVouchesFor
--   -- Stock
--   , 'getVouchesData
--   , 'replaceVouchesData
--   ]

instance IsAcidic VouchData where
  acidEvents
    = [UpdateEvent
         (\ (PutVouch arg_aumY arg_aumZ) -> putVouch arg_aumY arg_aumZ)
         safeCopyMethodSerialiser,
       QueryEvent
         (\ (GetVouchesFor arg_aun0) -> getVouchesFor arg_aun0)
         safeCopyMethodSerialiser,
       QueryEvent
         (\ GetVouchesData -> getVouchesData) safeCopyMethodSerialiser,
       UpdateEvent
         (\ (ReplaceVouchesData arg_aun1) -> replaceVouchesData arg_aun1)
         safeCopyMethodSerialiser]
data PutVouch = PutVouch UserId (UserId, UTCTime)
instance SafeCopy PutVouch where
  putCopy (PutVouch arg_aumn arg_aumo)
    = contain
        (do safePut arg_aumn
            safePut arg_aumo
            return ())
  getCopy = contain ((return PutVouch <*> safeGet) <*> safeGet)
  errorTypeName _ = "Data.SafeCopy.SafeCopy.SafeCopy PutVouch"
instance Method PutVouch where
  type MethodResult PutVouch = ()
  type MethodState PutVouch = VouchData
  methodTag = movedMethodTag "Distribution.Server.Features.Vouch"
instance UpdateEvent PutVouch
newtype GetVouchesFor = GetVouchesFor UserId
instance SafeCopy GetVouchesFor where
  putCopy (GetVouchesFor arg_aumy)
    = contain
        (do safePut arg_aumy
            return ())
  getCopy = contain (return GetVouchesFor <*> safeGet)
  errorTypeName _ = "Data.SafeCopy.SafeCopy.SafeCopy GetVouchesFor"
instance Method GetVouchesFor where
  type MethodResult GetVouchesFor = [(UserId, UTCTime)]
  type MethodState GetVouchesFor = VouchData
  methodTag = movedMethodTag "Distribution.Server.Features.Vouch"
instance QueryEvent GetVouchesFor
data GetVouchesData = GetVouchesData
instance SafeCopy GetVouchesData where
  putCopy GetVouchesData = contain (do return ())
  getCopy = contain (return GetVouchesData)
  errorTypeName _ = "Data.SafeCopy.SafeCopy.SafeCopy GetVouchesData"
instance Method GetVouchesData where
  type MethodResult GetVouchesData = VouchData
  type MethodState GetVouchesData = VouchData
  methodTag = movedMethodTag "Distribution.Server.Features.Vouch"
instance QueryEvent GetVouchesData
newtype ReplaceVouchesData = ReplaceVouchesData VouchData
instance SafeCopy ReplaceVouchesData where
  putCopy (ReplaceVouchesData arg_aumH)
    = contain
        (do safePut arg_aumH
            return ())
  getCopy = contain (return ReplaceVouchesData <*> safeGet)
  errorTypeName _
    = "Data.SafeCopy.SafeCopy.SafeCopy ReplaceVouchesData"
instance Method ReplaceVouchesData where
  type MethodResult ReplaceVouchesData = ()
  type MethodState ReplaceVouchesData = VouchData
  methodTag = movedMethodTag "Distribution.Server.Features.Vouch"
instance UpdateEvent ReplaceVouchesData
