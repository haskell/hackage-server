{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}
module Distribution.Server.Users.State where

import Distribution.Server.Instances ()

import Distribution.Package (PackageIdentifier,Package(packageId),PackageName)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types (PkgInfo(..))
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserGroup)
import Distribution.Server.Users.Types (UserId,UserName,UserAuth)
import Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Users (Users)
import Distribution.Server.Util.BlobStorage (BlobId)
import qualified Distribution.Server.BuildReport.BuildReports as BuildReports
import Distribution.Server.BuildReport.BuildReports (BuildReports,BuildReportId,BuildLog)
import Distribution.Server.BuildReport.BuildReport (BuildReport)

import Happstack.State
import Happstack.Data.Serialize
import qualified Data.Binary as Binary

import Data.Typeable
import Data.Maybe (fromMaybe)
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Monoid
import Data.Time.Clock (UTCTime(..))

import qualified Data.Map as Map


--type GroupName = String
data GroupName = Administrator | Trustee | PackageMaintainer PackageName
                 deriving (Read,Show,Ord,Typeable,Eq)
data Permissions = Permissions
       { permissions :: Map.Map GroupName UserGroup
       } deriving Typeable

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
lookupUserGroup groupName = lookupUserGroups [groupName]

lookupUserGroups :: [GroupName] -> Query Permissions UserGroup
lookupUserGroups groups
    = do m <- asks permissions
         return $ Group.unions [ Map.findWithDefault Group.empty groupName m
                                 | groupName <- groups ]

addToGroup :: GroupName -> UserId -> Update Permissions ()
addToGroup groupName userId
    = State.modify $ \st -> st{permissions = Map.alter fn groupName (permissions st)}
    where fn mbGroup = Just $ Group.add userId (fromMaybe Group.empty mbGroup)

removeFromGroup :: GroupName -> UserId -> Update Permissions ()
removeFromGroup groupName userId
    = State.modify $ \st -> st{permissions = Map.alter fn groupName (permissions st)}
    where fn Nothing = Nothing
          fn (Just group) = Just $ Group.remove userId group



-- overwrites existing permissions
bulkImportPermissions :: [(UserId, GroupName)] -> Update Permissions ()
bulkImportPermissions perms = do

  State.put $ Permissions Map.empty
  mapM_ (\(user, group) -> addToGroup group user) perms

         
$(mkMethods ''Permissions ['lookupUserGroup
                          ,'lookupUserGroups
                          ,'addToGroup
                          ,'removeFromGroup

                          -- Import
                          ,'bulkImportPermissions])
