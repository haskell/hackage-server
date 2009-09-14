{-# LANGUAGE DeriveDataTypeable #-}

module Distribution.Server.Users.Permissions
    ( GroupName(..)
    , Permissions(..)
      -- construction
    , empty
    , addToGroup
    , removeFromGroup
      -- querying
    , lookupUserGroup
    , lookupUserGroups
    , enumerate
    ) where

import Distribution.Package
         ( PackageName )
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserGroup)
import Distribution.Server.Users.Types (UserId)

import Distribution.Compat.ReadP
import Distribution.Text
import Text.PrettyPrint.HughesPJ hiding (empty)

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Typeable



data GroupName = Administrator | Trustee | PackageMaintainer PackageName
                 deriving (Read,Show,Ord,Typeable,Eq)

instance Text GroupName where
    disp Administrator = text "Administrator"
    disp Trustee = text "Trustee"
    disp (PackageMaintainer pkgName)
        = text "PackageMaintainer" <+> disp pkgName

    parse
        = choice
          [ string "Administrator" >> return Administrator
          , string "Trustee" >> return Trustee
          , string "PackageMaintainer" >>
            skipSpaces >>
            PackageMaintainer `fmap` parse
          ]


data Permissions = Permissions
       { permissions :: Map.Map GroupName UserGroup
       } deriving Typeable

empty :: Permissions
empty = Permissions Map.empty

enumerate :: Permissions -> [(GroupName, UserGroup)]
enumerate = Map.toList . permissions

lookupUserGroup :: GroupName -> Permissions -> UserGroup
lookupUserGroup group = lookupUserGroups [group]

lookupUserGroups :: [GroupName] -> Permissions -> UserGroup
lookupUserGroups groups perms
    = Group.unions
       [ Map.findWithDefault Group.empty groupName (permissions perms)
             | groupName <- groups
       ]

addToGroup :: GroupName -> UserId -> Permissions -> Permissions
addToGroup groupName userId perms
    = perms{permissions = Map.alter fn groupName (permissions perms)}
    where fn mbGroup = Just $ Group.add userId (fromMaybe Group.empty mbGroup)

removeFromGroup :: GroupName -> UserId -> Permissions -> Permissions
removeFromGroup groupName userId perms
    =  perms{permissions = Map.alter fn groupName (permissions perms)}
    where fn Nothing = Nothing
          fn (Just group) = Just $ Group.remove userId group
