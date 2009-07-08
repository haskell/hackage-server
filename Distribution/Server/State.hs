{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators #-}
module Distribution.Server.State where

import Distribution.Server.Instances ()

import Distribution.Package (PackageIdentifier,Package(packageId),PackageName)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Server.Types (PkgInfo(..))
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserGroup)
import Distribution.Server.Users.Types (UserId,UserName)
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

data Documentation = Documentation {
     documentation :: Map.Map PackageIdentifier BlobId
     } deriving Typeable

instance Component Documentation where
    type Dependencies Documentation = End
    initialValue = Documentation Map.empty

instance Version Documentation where
    mode = Versioned 0 Nothing -- Version 0, no previous types

instance Serialize Documentation where
    putCopy (Documentation m) = contain $ safePut m
    getCopy = contain $ liftM Documentation safeGet



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


data PackagesState = PackagesState {
    packageList  :: !(PackageIndex.PackageIndex PkgInfo),
    buildReports :: !BuildReports,
    userDb       :: !Users
  }
  deriving Typeable

instance Component PackagesState where
  type Dependencies PackagesState = End
  initialValue = PackagesState {
    packageList  = mempty,
    buildReports = BuildReports.empty,
    userDb       = Users.empty
  }

instance Version PackagesState where
    mode = Versioned 0 Nothing

instance Serialize PackagesState where
  putCopy (PackagesState idx rpts users) = contain $ do
    safePut $ PackageIndex.allPackages idx
    safePut rpts
    safePut users
  getCopy = contain $ do
    packages <- safeGet
    reports  <- safeGet
    users    <- safeGet
    return PackagesState {
      packageList  = PackageIndex.fromList packages,
      buildReports = reports,
      userDb       = users
    }

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

instance Version BuildReports where
  mode = Versioned 0 Nothing

instance Serialize BuildReports where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get

instance Version PackageIdentifier where
  mode = Versioned 0 Nothing

instance Serialize PackageIdentifier where
  putCopy = contain . Binary.put . show
  getCopy = contain $ fmap read Binary.get

instance Version PkgInfo where
  mode = Versioned 0 Nothing

instance Serialize PkgInfo where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get

instance Version UTCTime where
  mode = Versioned 0 Nothing

instance Serialize UTCTime where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get

instance Version BlobId where
  mode = Versioned 0 Nothing

instance Serialize BlobId where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get




lookupDocumentation :: PackageIdentifier -> Query Documentation (Maybe BlobId)
lookupDocumentation pkgId
    = do m <- asks documentation
         return $ Map.lookup pkgId m

insertDocumentation :: PackageIdentifier -> BlobId -> Update Documentation ()
insertDocumentation pkgId blob
    = State.modify $ \doc -> doc{documentation = Map.insert pkgId blob (documentation doc)}



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




insert :: PkgInfo -> Update PackagesState Bool
insert pkg
    = do pkgsState <- State.get
         case PackageIndex.lookupPackageId (packageList pkgsState) (packageId pkg) of
           Nothing -> do State.put $ pkgsState { packageList = PackageIndex.insert pkg (packageList pkgsState) }
                         return True
           Just{}  -> do return False

updateUsers :: Users -> Update PackagesState ()
updateUsers u = do
    st <- State.get
    State.put st { userDb = u }

-- NOTE! overwrites any existing data
bulkImport :: [PkgInfo] -> Users -> Update PackagesState ()
bulkImport newIndex users = do
  pkgsState <- State.get
  State.put pkgsState {
    packageList = PackageIndex.fromList newIndex,
    userDb = users
  }

addReport :: BuildReport -> Update PackagesState BuildReportId
addReport report
    = do pkgsState <- State.get
         let (reports, reportId) = BuildReports.addReport (buildReports pkgsState) report
         State.put pkgsState{buildReports = reports}
         return reportId

addBuildLog :: BuildReportId -> BuildLog -> Update PackagesState Bool
addBuildLog reportId buildLog
    = do pkgsState <- State.get
         case BuildReports.addBuildLog (buildReports pkgsState) reportId buildLog of
           Nothing -> return False
           Just reports -> do State.put pkgsState{buildReports = reports}
                              return True


getPackagesState :: Query PackagesState PackagesState
getPackagesState = ask

listGroupMembers :: UserGroup -> Query PackagesState [UserName]
listGroupMembers userGroup
    = do users <- asks userDb
         return [ Users.idToName users uid | uid <- Group.enumerate userGroup ]
         


$(mkMethods ''Documentation ['insertDocumentation
                            ,'lookupDocumentation])

$(mkMethods ''Permissions ['lookupUserGroup
                          ,'lookupUserGroups
                          ,'addToGroup
                          ,'removeFromGroup])

$(mkMethods ''PackagesState ['getPackagesState
                            ,'listGroupMembers
                            ,'bulkImport
                            ,'insert
                            ,'addReport
                            ,'addBuildLog
                            ,'updateUsers
                            ])



data HackageEntryPoint = HackageEntryPoint deriving Typeable

instance Version HackageEntryPoint
instance Serialize HackageEntryPoint where
    putCopy HackageEntryPoint = contain $ return ()
    getCopy = contain $ return HackageEntryPoint

instance Component HackageEntryPoint where
    type Dependencies HackageEntryPoint = PackagesState :+: Documentation :+: Permissions :+: End
    initialValue = HackageEntryPoint


$(mkMethods ''HackageEntryPoint [])

