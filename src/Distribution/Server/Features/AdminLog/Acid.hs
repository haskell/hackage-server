{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, BangPatterns,
             GeneralizedNewtypeDeriving, NamedFieldPuns, RecordWildCards,
             PatternGuards, RankNTypes #-}

module Distribution.Server.Features.AdminLog.Acid where

import Distribution.Server.Features.AdminLog.Types
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Framework

import Data.SafeCopy (base, deriveSafeCopy)
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Time (UTCTime)
import qualified Data.ByteString.Lazy.Char8 as BS

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

makeAcidic ''AdminLog ['getAdminLog
                      ,'replaceAdminLog
                      ,'addAdminLog]
