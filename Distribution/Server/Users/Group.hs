{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ExistentialQuantification #-}
module Distribution.Server.Users.Group (
    UserList(..),
    UserGroup(..),
    GroupDescription(..),
    nullDescription,
    groupName,
    empty,
    add,
    remove,
    member,
    enumerate,
    fromList,
    unions,
    queryGroups
  ) where

import Distribution.Server.Users.Types

import qualified Data.IntSet as IntSet
import Data.Monoid (Monoid)
import Data.SafeCopy (SafeCopy(..), contain)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import Data.Typeable (Typeable)
import Control.DeepSeq

import Prelude hiding (id)

-- | Some subset of users, eg those allowed to perform some action.
--
newtype UserList = UserList IntSet.IntSet
  deriving (Eq, Monoid, Serialize, Typeable, Show)

instance SafeCopy UserList where
  putCopy = contain . Serialize.put
  getCopy = contain Serialize.get

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

fromList :: [UserId] -> UserList
fromList ids = UserList $ IntSet.fromList (map (\(UserId uid) -> uid) ids)

unions :: [UserList] -> UserList
unions groups = UserList (IntSet.unions [ group | UserList group <- groups ])

-- | An abstraction over a UserList for dynamically querying and modifying
-- a user group.
--
-- This structure is not only meant for singleton user groups, but also collections
-- of groups. Some features may provide a UserGroup parametrized by an argument.
--
data UserGroup = UserGroup {
    -- a description of the group for display
    groupDesc :: GroupDescription,
    -- dynamic querying for its members
    queryUserList :: IO UserList,
    -- dynamically add a member (does nothing if already exists)
    -- creates the group if it didn't exist previously
    addUserList :: UserId -> IO (),
    -- dynamically remove a member (does nothing if not present)
    -- creates the group if it didn't exist previously
    removeUserList :: UserId -> IO (),
    -- is the user group actually stored in server data? it's possible for
    -- a group to exist even if it's empty, and likewise to get a UserGroup
    -- that does't exisit *yet*
    groupExists :: IO Bool,
    -- user groups which can remove from one
    canRemoveGroup :: [UserGroup],
    -- user groups which can add to this one  (use 'fix' to add to self)
    canAddGroup :: [UserGroup]
}

-- | A displayable description for a user group.
--
-- Given a groupTitle of A and a group entity of Nothing, the group will be
-- called "A"; given a groupTitle  of "A" and a groupEntity of Just ("B",
-- Just "C"), the title will be displayed as "A for <a href=C>B</a>".
data GroupDescription = GroupDescription {
    groupTitle :: String,
    groupEntity :: Maybe (String, Maybe String),
    groupPrologue  :: String
}
nullDescription :: GroupDescription
nullDescription = GroupDescription { groupTitle = "", groupEntity = Nothing, groupPrologue = "" }

groupName :: GroupDescription -> String
groupName desc = groupTitle desc ++ maybe "" (\(for, _) -> " for " ++ for) (groupEntity desc)

queryGroups :: [UserGroup] -> IO UserList
queryGroups = fmap unions . mapM queryUserList

-- for use in Caches, really...
instance NFData GroupDescription where
    rnf (GroupDescription a b c) = rnf a `seq` rnf b `seq` rnf c

