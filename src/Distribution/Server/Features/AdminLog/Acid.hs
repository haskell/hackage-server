{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, BangPatterns,
             GeneralizedNewtypeDeriving, NamedFieldPuns, RecordWildCards,
             PatternGuards, RankNTypes #-}

module Distribution.Server.Features.AdminLog.Acid where

import Distribution.Server.Features.AdminLog.Types
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Framework

import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Time (UTCTime)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Acid.Compat

newtype AdminLog = AdminLog {
      adminLog :: [(UTCTime,UserId,AdminAction,BS.ByteString)]
} deriving (Show, MemSize)

deriveSafeCopy 0 'base ''AdminLog

initialAdminLog :: AdminLog
initialAdminLog = AdminLog []

getAdminLog :: Query AdminLog AdminLog
getAdminLog = ask

addAdminLog :: (UTCTime, UserId, AdminAction, BS.ByteString) -> Update AdminLog ()
addAdminLog x = State.modify (\(AdminLog xs) -> AdminLog (x : xs))

instance Eq AdminLog where
    (AdminLog (x:_)) == (AdminLog (y:_)) = x == y
    (AdminLog []) == (AdminLog []) = True
    _ == _ = False

replaceAdminLog :: AdminLog -> Update AdminLog ()
replaceAdminLog = State.put

------------------------------
-- IsAcidic machinery
--
-- See Note [Acid Migration] in "Data.Acid.Compat"
-- original module name was Distribution.Server.Features.AdminLog"

-- makeAcidic ''AdminLog ['getAdminLog
--                       ,'replaceAdminLog
--                       ,'addAdminLog]

instance IsAcidic AdminLog where
  acidEvents
    = [QueryEvent
         (\ GetAdminLog -> getAdminLog) safeCopyMethodSerialiser,
       UpdateEvent
         (\ (ReplaceAdminLog arg_aDn6) -> replaceAdminLog arg_aDn6)
         safeCopyMethodSerialiser,
       UpdateEvent
         (\ (AddAdminLog arg_aDn7) -> addAdminLog arg_aDn7)
         safeCopyMethodSerialiser]
data GetAdminLog = GetAdminLog
instance SafeCopy GetAdminLog where
  putCopy GetAdminLog = contain (do return ())
  getCopy = contain (return GetAdminLog)
  errorTypeName _ = "Data.SafeCopy.SafeCopy.SafeCopy GetAdminLog"
instance Data.Acid.Compat.Method GetAdminLog where
  type MethodResult GetAdminLog = AdminLog
  type MethodState GetAdminLog = AdminLog
  methodTag = movedMethodTag "Distribution.Server.Features.AdminLog"
instance QueryEvent GetAdminLog
newtype ReplaceAdminLog = ReplaceAdminLog AdminLog
instance SafeCopy ReplaceAdminLog where
  putCopy (ReplaceAdminLog arg_aDmO)
    = contain
        (do safePut arg_aDmO
            return ())
  getCopy = contain (return ReplaceAdminLog <*> safeGet)
  errorTypeName _ = "Data.SafeCopy.SafeCopy.SafeCopy ReplaceAdminLog"
instance Data.Acid.Compat.Method ReplaceAdminLog where
  type MethodResult ReplaceAdminLog = ()
  type MethodState ReplaceAdminLog = AdminLog
  methodTag = movedMethodTag "Distribution.Server.Features.AdminLog"
instance UpdateEvent ReplaceAdminLog
newtype AddAdminLog
  = AddAdminLog (UTCTime, UserId, AdminAction, BS.ByteString)
instance SafeCopy AddAdminLog where
  putCopy (AddAdminLog arg_aDmT)
    = contain
        (do safePut arg_aDmT
            return ())
  getCopy = contain (return AddAdminLog <*> safeGet)
  errorTypeName _ = "Data.SafeCopy.SafeCopy.SafeCopy AddAdminLog"
instance Data.Acid.Compat.Method AddAdminLog where
  type MethodResult AddAdminLog = ()
  type MethodState AddAdminLog = AdminLog
  methodTag = movedMethodTag "Distribution.Server.Features.AdminLog"
instance UpdateEvent AddAdminLog
