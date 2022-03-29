module Distribution.Server.Users.Group (
    module UserIdSet,
    UserGroup(..),
    GroupDescription(..),
    nullDescription,
    groupName,
    queryUserGroups,
  ) where

import Distribution.Server.Users.Types
import Distribution.Server.Users.UserIdSet as UserIdSet
import Distribution.Server.Framework.MemSize

import Control.DeepSeq


-- | A stateful interface to a group of users. It provides actions for
-- querying and modifying a user group.
--
-- This interface is meant not just for singleton user groups, but also
-- collections of groups. Some features may provide a UserGroup parametrized
-- by an argument (e.g. there's a maintainer group per-package).
--
-- It includes a list of which other users groups are supposed to be allowed
-- to edit this one: the 'canRemoveGroup' and 'canAddGroup' lists. One often
-- wants a user group to be in its own 'canRemoveGroup' or 'canAddGroup'
-- lists. This is easy using a recursive definition such as:
--
-- > let fooGroup = UserGroup { ... , canAddGroup = [adminGroup, fooGroup] }
--
data UserGroup = UserGroup {
    -- | A description of the group for display purposes
    groupDesc :: GroupDescription,

    -- | Query the current set of group members
    queryUserGroup :: IO UserIdSet,

    -- | Add a user to this group. This does nothing if it already exists.
    -- It initialises the group state if it wasn't previously initialised.
    addUserToGroup :: UserId -> IO (),

    -- | Remove a user from this group. It does nothing if not present.
    -- It initialises the group state if it wasn't previously initialised.
    removeUserFromGroup :: UserId -> IO (),

    -- | Other user groups which are authorised to remove users from this one
    -- (which may include this one recursively).
    --
    groupsAllowedToDelete :: [UserGroup],

    -- | User groups which are authorised to add members to this one (which
    -- may include this one recursively).
    --
    groupsAllowedToAdd :: [UserGroup]
}

-- | A displayable description for a user group.
--
-- Given a groupTitle of A and a group entity of Nothing, the group will be
-- called "A"; given a groupTitle  of "A" and a groupEntity of Just ("B",
-- Just "C"), the title will be displayed as "A for <a href=C>B</a>".
data GroupDescription = GroupDescription {
    groupTitle    :: String,
    groupEntity   :: Maybe (String, Maybe String),
    groupPrologue :: String
}

nullDescription :: GroupDescription
nullDescription =
    GroupDescription {
      groupTitle    = "",
      groupEntity   = Nothing,
      groupPrologue = ""
    }

groupName :: GroupDescription -> String
groupName desc =
    groupTitle desc ++ maybe "" (\(for, _) -> " for " ++ for) (groupEntity desc)

queryUserGroups :: [UserGroup] -> IO UserIdSet
queryUserGroups = fmap UserIdSet.unions . mapM queryUserGroup

-- for use in Caches, really...
instance NFData GroupDescription where
    rnf (GroupDescription a b c) = rnf a `seq` rnf b `seq` rnf c

instance MemSize GroupDescription where
    memSize (GroupDescription a b c) = memSize3 a b c
