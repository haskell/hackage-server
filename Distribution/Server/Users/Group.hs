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

import Data.Maybe (maybe)
import qualified Data.IntSet as IntSet
import Data.Monoid (Monoid)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Happstack.Data
import Control.Parallel.Strategies

import Prelude hiding (id)

-- | Some subset of users, eg those allowed to perform some action.
--
newtype UserList = UserList IntSet.IntSet
  deriving (Eq, Monoid, Binary, Typeable, Show)

instance Version UserList where
  mode = Versioned 0 Nothing
instance Serialize UserList where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get

-- Given a groupTitle of A, the group will be called "A"; given a groupTitle 
-- of "A" and a groupEntity of Just ("B", Just "C"), the title will be displayed
-- as "A for <a href=C>B</a>"; e.g., one can have "Maintainers for Parsec".
data GroupDescription = GroupDescription {
    groupTitle :: String,
    groupEntity :: Maybe (String, Maybe String),
    groupPrologue  :: String
}
nullDescription :: GroupDescription
nullDescription = GroupDescription { groupTitle = "", groupEntity = Nothing, groupPrologue = "" }

groupName :: GroupDescription -> String
groupName desc = groupTitle desc ++ maybe "" (\(for, _) -> " for " ++ for) (groupEntity desc)

data UserGroup = UserGroup {
    groupDesc :: GroupDescription,
    queryUserList :: IO UserList,
    addUserList :: UserId -> IO (),
    removeUserList :: UserId -> IO (),
    canRemoveGroup :: [UserGroup],
    canAddGroup :: [UserGroup]
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

fromList :: [UserId] -> UserList
fromList ids = UserList $ IntSet.fromList (map (\(UserId uid) -> uid) ids)

unions :: [UserList] -> UserList
unions groups = UserList (IntSet.unions [ group | UserList group <- groups ])

queryGroups :: [UserGroup] -> IO UserList
queryGroups = fmap unions . mapM queryUserList

-- for use in Caches, really...
instance NFData GroupDescription where
    rnf (GroupDescription a b c) = rnf a `seq` rnf b `seq` rnf c
