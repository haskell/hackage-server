{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}
module Distribution.Server.Users.State where

import Distribution.Server.Instances ()

import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserGroup)
import qualified Distribution.Server.Users.Permissions as Permissions
import Distribution.Server.Users.Permissions (Permissions(..),GroupName)
import Distribution.Server.Users.Types (UserId,UserName,UserAuth)
import Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Users (Users)

import qualified Data.Map as Map

import Happstack.State
import Happstack.Data.Serialize
import qualified Data.Binary as Binary

import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State


instance Component Permissions where
    type Dependencies Permissions = End
    initialValue = Permissions Map.empty

instance Version Permissions where
    mode = Versioned 0 Nothing

instance Serialize Permissions where
   putCopy (Permissions p) = contain $ safePut p
   getCopy = contain $ liftM Permissions safeGet

instance Version Users where
  mode = Versioned 0 Nothing

instance Serialize Users where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get

instance Version UserGroup where
  mode = Versioned 0 Nothing
instance Serialize UserGroup where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get

instance Version GroupName where
  mode = Versioned 0 Nothing
instance Serialize GroupName where
  putCopy = contain . Binary.put . show
  getCopy = contain (liftM read Binary.get)

instance Version UserId where
  mode = Versioned 0 Nothing
instance Serialize UserId where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get

instance Version UserName where
  mode = Versioned 0 Nothing
instance Serialize UserName where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get

instance Version UserAuth where
  mode = Versioned 0 Nothing
instance Serialize UserAuth where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get


lookupUserGroup :: GroupName -> Query Permissions UserGroup
lookupUserGroup group
    = Permissions.lookupUserGroup group `fmap` ask

lookupUserGroups :: [GroupName] -> Query Permissions UserGroup
lookupUserGroups groups =
    Permissions.lookupUserGroups groups `fmap` ask

addToGroup :: GroupName -> UserId -> Update Permissions ()
addToGroup groupName userId
    = State.modify $ Permissions.addToGroup groupName userId

removeFromGroup :: GroupName -> UserId -> Update Permissions ()
removeFromGroup groupName userId
    = State.modify $ Permissions.removeFromGroup groupName userId

removeGroup :: GroupName -> Update Permissions ()
removeGroup groupName
    = State.modify $ Permissions.removeGroup groupName

getPermissions :: Query Permissions Permissions
getPermissions = ask

-- |overwrites existing permissions
bulkImportPermissions :: [(UserId, GroupName)] -> Update Permissions ()
bulkImportPermissions perms = do

  State.put Permissions.empty
  mapM_ (\(user, group) -> addToGroup group user) perms

-- |overwrites existing permissions
replacePermissions :: Permissions -> Update Permissions ()
replacePermissions = State.put
         
$(mkMethods ''Permissions ['lookupUserGroup
                          ,'lookupUserGroups
                          ,'addToGroup
                          ,'removeFromGroup
                          ,'removeGroup
                          ,'getPermissions

                          -- Import
                          ,'bulkImportPermissions
                          ,'replacePermissions
                          ])
