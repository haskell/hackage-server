{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Distribution.Server.Features.UserSignup.Acid where

import Distribution.Server.Features.UserSignup.Types

import Distribution.Server.Framework hiding (Method)

import Distribution.Server.Util.Nonce

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put, modify)
import Data.Acid.Compat
import Data.SafeCopy

import Data.Time

-------------------------
-- Types of stored data
--

newtype SignupResetTable = SignupResetTable (Map Nonce SignupResetInfo)
  deriving (Eq, Show, MemSize)

emptySignupResetTable :: SignupResetTable
emptySignupResetTable = SignupResetTable Map.empty

$(deriveSafeCopy 0 'base ''SignupResetTable)

------------------------------
-- State queries and updates
--

getSignupResetTable :: Query SignupResetTable SignupResetTable
getSignupResetTable = ask

replaceSignupResetTable :: SignupResetTable -> Update SignupResetTable ()
replaceSignupResetTable = put

lookupSignupResetInfo :: Nonce -> Query SignupResetTable (Maybe SignupResetInfo)
lookupSignupResetInfo nonce = do
    SignupResetTable tbl <- ask
    return $! Map.lookup nonce tbl

addSignupResetInfo :: Nonce -> SignupResetInfo -> Update SignupResetTable Bool
addSignupResetInfo nonce info = do
    SignupResetTable tbl <- get
    if not (Map.member nonce tbl)
      then do put $! SignupResetTable (Map.insert nonce info tbl)
              return True
      else return False

deleteSignupResetInfo :: Nonce -> Update SignupResetTable ()
deleteSignupResetInfo nonce = do
    SignupResetTable tbl <- get
    put $! SignupResetTable (Map.delete nonce tbl)

deleteAllExpired :: UTCTime -> Update SignupResetTable ()
deleteAllExpired expireTime =
    modify $ \(SignupResetTable tbl) ->
      SignupResetTable $
        Map.filter (\entry -> nonceTimestamp entry > expireTime) tbl

------------------------------
-- IsAcidic machinery
--
-- See Note [Acid Migration] in "Data.Acid.Compat"
-- original module name was Distribution.Server.Features.UserSignup"

-- makeAcidic ''SignupResetTable [
--    --queries
--    'getSignupResetTable,
--    'lookupSignupResetInfo,
--    --updates
--    'replaceSignupResetTable,
--    'addSignupResetInfo,
--    'deleteSignupResetInfo,
--    'deleteAllExpired
--  ]

instance IsAcidic SignupResetTable where
  acidEvents
    = [QueryEvent
         (\ GetSignupResetTable -> getSignupResetTable)
         safeCopyMethodSerialiser,
       QueryEvent
         (\ (LookupSignupResetInfo arg_a30zB)
            -> lookupSignupResetInfo arg_a30zB)
         safeCopyMethodSerialiser,
       UpdateEvent
         (\ (ReplaceSignupResetTable arg_a30zC)
            -> replaceSignupResetTable arg_a30zC)
         safeCopyMethodSerialiser,
       UpdateEvent
         (\ (AddSignupResetInfo arg_a30zD arg_a30zE)
            -> addSignupResetInfo arg_a30zD arg_a30zE)
         safeCopyMethodSerialiser,
       UpdateEvent
         (\ (DeleteSignupResetInfo arg_a30zF)
            -> deleteSignupResetInfo arg_a30zF)
         safeCopyMethodSerialiser,
       UpdateEvent
         (\ (DeleteAllExpired arg_a30zG) -> deleteAllExpired arg_a30zG)
         safeCopyMethodSerialiser]
data GetSignupResetTable = GetSignupResetTable
instance SafeCopy GetSignupResetTable where
  putCopy GetSignupResetTable = contain (do return ())
  getCopy = contain (return GetSignupResetTable)
  errorTypeName _
    = "Data.SafeCopy.SafeCopy.SafeCopy GetSignupResetTable"
instance Method GetSignupResetTable where
  type MethodResult GetSignupResetTable = SignupResetTable
  type MethodState GetSignupResetTable = SignupResetTable
  methodTag = movedMethodTag "Distribution.Server.Features.UserSignup"
instance QueryEvent GetSignupResetTable
newtype LookupSignupResetInfo = LookupSignupResetInfo Nonce
instance SafeCopy LookupSignupResetInfo where
  putCopy (LookupSignupResetInfo arg_a30yN)
    = contain
        (do safePut arg_a30yN
            return ())
  getCopy = contain (return LookupSignupResetInfo <*> safeGet)
  errorTypeName _
    = "Data.SafeCopy.SafeCopy.SafeCopy LookupSignupResetInfo"
instance Method LookupSignupResetInfo where
  type MethodResult LookupSignupResetInfo = Maybe SignupResetInfo
  type MethodState LookupSignupResetInfo = SignupResetTable
  methodTag = movedMethodTag "Distribution.Server.Features.UserSignup"
instance QueryEvent LookupSignupResetInfo
newtype ReplaceSignupResetTable
  = ReplaceSignupResetTable SignupResetTable
instance SafeCopy ReplaceSignupResetTable where
  putCopy (ReplaceSignupResetTable arg_a30yS)
    = contain
        (do safePut arg_a30yS
            return ())
  getCopy = contain (return ReplaceSignupResetTable <*> safeGet)
  errorTypeName _
    = "Data.SafeCopy.SafeCopy.SafeCopy ReplaceSignupResetTable"
instance Method ReplaceSignupResetTable where
  type MethodResult ReplaceSignupResetTable = ()
  type MethodState ReplaceSignupResetTable = SignupResetTable
  methodTag = movedMethodTag "Distribution.Server.Features.UserSignup"
instance UpdateEvent ReplaceSignupResetTable
data AddSignupResetInfo = AddSignupResetInfo Nonce SignupResetInfo
instance SafeCopy AddSignupResetInfo where
  putCopy (AddSignupResetInfo arg_a30yX arg_a30yY)
    = contain
        (do safePut arg_a30yX
            safePut arg_a30yY
            return ())
  getCopy
    = contain ((return AddSignupResetInfo <*> safeGet) <*> safeGet)
  errorTypeName _
    = "Data.SafeCopy.SafeCopy.SafeCopy AddSignupResetInfo"
instance Method AddSignupResetInfo where
  type MethodResult AddSignupResetInfo = Bool
  type MethodState AddSignupResetInfo = SignupResetTable
  methodTag = movedMethodTag "Distribution.Server.Features.UserSignup"
instance UpdateEvent AddSignupResetInfo
newtype DeleteSignupResetInfo = DeleteSignupResetInfo Nonce
instance SafeCopy DeleteSignupResetInfo where
  putCopy (DeleteSignupResetInfo arg_a30z3)
    = contain
        (do safePut arg_a30z3
            return ())
  getCopy = contain (return DeleteSignupResetInfo <*> safeGet)
  errorTypeName _
    = "Data.SafeCopy.SafeCopy.SafeCopy DeleteSignupResetInfo"
instance Method DeleteSignupResetInfo where
  type MethodResult DeleteSignupResetInfo = ()
  type MethodState DeleteSignupResetInfo = SignupResetTable
  methodTag = movedMethodTag "Distribution.Server.Features.UserSignup"
instance UpdateEvent DeleteSignupResetInfo
newtype DeleteAllExpired = DeleteAllExpired UTCTime
instance SafeCopy DeleteAllExpired where
  putCopy (DeleteAllExpired arg_a30z8)
    = contain
        (do safePut arg_a30z8
            return ())
  getCopy = contain (return DeleteAllExpired <*> safeGet)
  errorTypeName _
    = "Data.SafeCopy.SafeCopy.SafeCopy DeleteAllExpired"
instance Method DeleteAllExpired where
  type MethodResult DeleteAllExpired = ()
  type MethodState DeleteAllExpired = SignupResetTable
  methodTag = movedMethodTag "Distribution.Server.Features.UserSignup"
instance UpdateEvent DeleteAllExpired
