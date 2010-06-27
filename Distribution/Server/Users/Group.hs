{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, ExistentialQuantification #-}
module Distribution.Server.Users.Group (
    UserList(..),
    UserGroup(..),
    GroupDescription(..),
    nullDescription,
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
import qualified Data.Binary as Binary
import Happstack.Data

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

data GroupDescription = GroupDescription {
    groupTitle :: String,
    groupShort :: String,
    groupEntityURL :: String,
    groupPrologue  :: String
}
nullDescription :: GroupDescription
nullDescription = GroupDescription { groupTitle = "", groupShort = "",
                                     groupEntityURL = "", groupPrologue = "" }

--used to require: forall a b c. QueryEvent a (Maybe UserList), UpdateEvent b (), UpdateEvent c ()
data UserGroup = UserGroup {
    groupDesc :: GroupDescription,
    queryUserList :: IO UserList,
    addUserList :: UserId -> IO (),
    removeUserList :: UserId -> IO ()
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
