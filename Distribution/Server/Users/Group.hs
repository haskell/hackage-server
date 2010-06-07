{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ExistentialQuantification #-}
module Distribution.Server.Users.Group (
    UserList(..),
    UserGroup(..),
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
newtype UserList = UserList IntSet.IntSet
  deriving (Eq, Monoid, Binary, Typeable, Show)

--forall a b. QueryEvent a (Maybe UserList), UpdateEvent b (), UpdateEvent c ()
data UserGroup a b c = UserGroup {
    groupName :: String,
    queryUserList :: a,
    addUserList :: UserId -> b,
    removeUserList :: UserId -> c
}

empty :: UserList
empty = UserList IntSet.empty

add :: UserId -> UserList -> UserList
add (UserId id) (UserList group) = UserList (IntSet.insert id group)

remove :: UserId -> UserList -> UserList
remove (UserId id) (UserList group) = UserList (IntSet.delete id group)

member :: UserId -> UserList -> Bool
member (UserId id) (UserList group) = IntSet.member id group

enumerate :: UserList -> [UserId]
enumerate (UserList group) = map UserId (IntSet.toList group)

unions :: [UserList] -> UserList
unions groups = UserList (IntSet.unions [ group | UserList group <- groups ])
