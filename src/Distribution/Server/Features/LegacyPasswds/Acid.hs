{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             RankNTypes, NamedFieldPuns, RecordWildCards,
             RecursiveDo, BangPatterns #-}
module Distribution.Server.Features.LegacyPasswds.Acid where

import Prelude

import Distribution.Server.Framework

import qualified Distribution.Server.Features.LegacyPasswds.Auth as LegacyAuth

import Distribution.Server.Users.Types

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.SafeCopy (base, deriveSafeCopy)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)

-------------------------
-- Types of stored data
--

newtype LegacyPasswdsTable = LegacyPasswdsTable (IntMap LegacyAuth.HtPasswdHash)
  deriving (Eq, Show)

emptyLegacyPasswdsTable :: LegacyPasswdsTable
emptyLegacyPasswdsTable = LegacyPasswdsTable IntMap.empty

lookupUserLegacyPasswd :: UserId -> LegacyPasswdsTable -> Maybe LegacyAuth.HtPasswdHash
lookupUserLegacyPasswd (UserId uid) (LegacyPasswdsTable tbl) =
    IntMap.lookup uid tbl

enumerateAllUserLegacyPasswd :: LegacyPasswdsTable -> [UserId]
enumerateAllUserLegacyPasswd (LegacyPasswdsTable tbl) =
    map UserId (IntMap.keys tbl)

$(deriveSafeCopy 0 'base ''LegacyPasswdsTable)

instance MemSize LegacyPasswdsTable where
    memSize (LegacyPasswdsTable a) = memSize1 a

------------------------------
-- State queries and updates
--

getLegacyPasswdsTable :: Query LegacyPasswdsTable LegacyPasswdsTable
getLegacyPasswdsTable = ask

replaceLegacyPasswdsTable :: LegacyPasswdsTable -> Update LegacyPasswdsTable ()
replaceLegacyPasswdsTable = put

setUserLegacyPasswd :: UserId -> LegacyAuth.HtPasswdHash -> Update LegacyPasswdsTable ()
setUserLegacyPasswd (UserId uid) udetails = do
    LegacyPasswdsTable tbl <- get
    put $! LegacyPasswdsTable (IntMap.insert uid udetails tbl)

deleteUserLegacyPasswd :: UserId -> Update LegacyPasswdsTable Bool
deleteUserLegacyPasswd (UserId uid) = do
    LegacyPasswdsTable tbl <- get
    if IntMap.member uid tbl
      then do put $! LegacyPasswdsTable (IntMap.delete uid tbl)
              return True
      else return False


makeAcidic ''LegacyPasswdsTable [
    --queries
    'getLegacyPasswdsTable,
    --updates
    'replaceLegacyPasswdsTable,
    'setUserLegacyPasswd,
    'deleteUserLegacyPasswd
  ]

