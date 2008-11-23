{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Distribution.Server.Users.Group (
    UserGroup,
    empty,
    add,
    remove,
    member,
    enumerate,
    unions,
  ) where

import Distribution.Server.Users.Types

import qualified Data.IntSet as IntSet
import Data.Monoid (Monoid)
import Data.Binary (Binary)
import Data.Typeable (Typeable)

import Prelude hiding (id)

-- | Some subset of users, eg those allowed to perform some action.
--
newtype UserGroup = UserGroup IntSet.IntSet
  deriving (Eq, Monoid, Binary, Typeable)

empty :: UserGroup
empty = UserGroup IntSet.empty

add :: UserId -> UserGroup -> UserGroup
add (UserId id) (UserGroup group) = UserGroup (IntSet.insert id group)

remove :: UserId -> UserGroup -> UserGroup
remove (UserId id) (UserGroup group) = UserGroup (IntSet.delete id group)

member :: UserId -> UserGroup -> Bool
member (UserId id) (UserGroup group) = IntSet.member id group

enumerate :: UserGroup -> [UserId]
enumerate (UserGroup group) = map UserId (IntSet.toList group)

unions :: [UserGroup] -> UserGroup
unions groups = UserGroup (IntSet.unions [ group | UserGroup group <- groups ])
