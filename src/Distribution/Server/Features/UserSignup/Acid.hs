{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
             TypeFamilies, TemplateHaskell,
             RankNTypes, NamedFieldPuns, RecordWildCards, BangPatterns #-}
module Distribution.Server.Features.UserSignup.Acid where

import Distribution.Server.Features.UserSignup.Types

import Distribution.Server.Framework

import Distribution.Server.Util.Nonce

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put, modify)
import Data.SafeCopy (base, deriveSafeCopy)

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

makeAcidic ''SignupResetTable [
    --queries
    'getSignupResetTable,
    'lookupSignupResetInfo,
    --updates
    'replaceSignupResetTable,
    'addSignupResetInfo,
    'deleteSignupResetInfo,
    'deleteAllExpired
  ]

