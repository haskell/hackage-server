{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Distribution.Server.Features.UserSignup.State where

import Distribution.Server.Features.UserSignup.Types
import Distribution.Server.Framework (MemSize)
import Distribution.Server.Util.Nonce (Nonce)

import qualified Data.Map as Map
import Data.Map (Map)
import Data.SafeCopy

-------------------------
-- Types of stored data
--

newtype SignupResetTable = SignupResetTable (Map Nonce SignupResetInfo)
  deriving (Eq, Show, MemSize)

emptySignupResetTable :: SignupResetTable
emptySignupResetTable = SignupResetTable Map.empty

$(deriveSafeCopy 0 'base ''SignupResetTable)
