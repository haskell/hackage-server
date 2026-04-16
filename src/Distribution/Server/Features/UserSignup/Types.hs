{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell,
             RankNTypes, NamedFieldPuns, RecordWildCards, BangPatterns #-}
module Distribution.Server.Features.UserSignup.Types where

import Distribution.Server.Framework

import Distribution.Server.Users.Types

import Data.Text (Text)
import Data.SafeCopy (base, deriveSafeCopy)

import Data.Time

-------------------------
-- Types of stored data
--

data SignupResetInfo = SignupInfo {
                         signupUserName     :: !Text,
                         signupRealName     :: !Text,
                         signupContactEmail :: !Text,
                         nonceTimestamp     :: !UTCTime
                       }
                     | ResetInfo {
                         resetUserId        :: !UserId,
                         nonceTimestamp     :: !UTCTime
                     }
  deriving (Eq, Show)

instance MemSize SignupResetInfo where
    memSize (SignupInfo a b c d) = memSize4 a b c d
    memSize (ResetInfo  a b)     = memSize2 a b

$(deriveSafeCopy 0 'base ''SignupResetInfo)
