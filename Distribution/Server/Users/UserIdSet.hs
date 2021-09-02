{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Distribution.Server.Users.UserIdSet (
    UserIdSet(..),
    empty,
    insert,
    delete,
    member,
    size,
    null,
    toList,
    fromList,
    unions,
  ) where

import Distribution.Server.Prelude hiding (null, empty)
import Prelude ()

import Distribution.Server.Users.Types
import Distribution.Server.Framework.MemSize

import qualified Data.IntSet as IntSet
import Data.SafeCopy (SafeCopy(..), contain)
import qualified Data.Serialize as Serialize
import Control.DeepSeq
import Data.Aeson (ToJSON)


-- | A simple set of 'UserId's. Used to implement user groups, but can be used
-- anywhere a set of users identified by 'UserId' is needed.
--
newtype UserIdSet = UserIdSet IntSet.IntSet
  deriving (Eq, Semigroup, Monoid, Typeable, Show, NFData, MemSize, ToJSON)

empty :: UserIdSet
empty = UserIdSet IntSet.empty

insert :: UserId -> UserIdSet -> UserIdSet
insert (UserId uid) (UserIdSet uidset) = UserIdSet (IntSet.insert uid uidset)

delete :: UserId -> UserIdSet -> UserIdSet
delete (UserId uid) (UserIdSet uidset) = UserIdSet (IntSet.delete uid uidset)

member :: UserId -> UserIdSet -> Bool
member (UserId uid) (UserIdSet uidset) = IntSet.member uid uidset

size :: UserIdSet -> Int
size (UserIdSet uidset) = IntSet.size uidset

null :: UserIdSet -> Bool
null (UserIdSet uidset) = IntSet.null uidset

toList :: UserIdSet -> [UserId]
toList (UserIdSet uidset) = map UserId (IntSet.toList uidset)

fromList :: [UserId] -> UserIdSet
fromList ids = UserIdSet $ IntSet.fromList (map (\(UserId uid) -> uid) ids)

unions :: [UserIdSet] -> UserIdSet
unions uidsets = UserIdSet (IntSet.unions [ uidset | UserIdSet uidset <- uidsets ])

instance SafeCopy UserIdSet where
  putCopy (UserIdSet x) = contain $ Serialize.put x
  getCopy = contain $ UserIdSet <$> Serialize.get

