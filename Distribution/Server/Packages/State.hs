{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}
module Distribution.Server.Packages.State where

import Distribution.Server.Instances ()
import Distribution.Server.Users.State ()

import Distribution.Package (PackageIdentifier,Package(packageId))
import qualified Distribution.Server.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types (PkgInfo(..))
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserGroup)
import Distribution.Server.Users.Types (UserId,UserName,UserAuth)
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Users (Users)
import Distribution.Server.Util.BlobStorage (BlobId)
import qualified Distribution.Server.BuildReport.BuildReports as BuildReports
import Distribution.Server.BuildReport.BuildReports (BuildReports,BuildReportId,BuildLog)
import Distribution.Server.BuildReport.BuildReport (BuildReport)

import Happstack.State
import qualified Data.Binary as Binary

import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Maybe (isJust)
import Data.Monoid
import Data.Time.Clock (UTCTime(..))

import qualified Data.Map as Map

data Documentation = Documentation {
     documentation :: Map.Map PackageIdentifier BlobId
     } deriving (Typeable, Show)

instance Component Documentation where
    type Dependencies Documentation = End
    initialValue = Documentation Map.empty

instance Version Documentation where
    mode = Versioned 0 Nothing -- Version 0, no previous types

instance Serialize Documentation where
    putCopy (Documentation m) = contain $ safePut m
    getCopy = contain $ liftM Documentation safeGet



data PackagesState = PackagesState {
    packageList  :: !(PackageIndex.PackageIndex PkgInfo),
    buildReports :: !BuildReports,
    userDb       :: !Users
  }
  deriving (Typeable, Show)

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

{-- These are included in happstack-state 0.5.*
instance Version UTCTime where
  mode = Versioned 0 Nothing

instance Serialize UTCTime where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get
-}

instance Version BlobId where
  mode = Versioned 0 Nothing

instance Serialize BlobId where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get




lookupDocumentation :: PackageIdentifier -> Query Documentation (Maybe BlobId)
lookupDocumentation pkgId
    = do m <- asks documentation
         return $ Map.lookup pkgId m

hasDocumentation :: PackageIdentifier -> Query Documentation Bool
hasDocumentation pkgId
    = lookupDocumentation pkgId >>= \x -> case x of
         Just{} -> return True
         _      -> return False

insertDocumentation :: PackageIdentifier -> BlobId -> Update Documentation ()
insertDocumentation pkgId blob
    = State.modify $ \doc -> doc{documentation = Map.insert pkgId blob (documentation doc)}

getDocumentation :: Query Documentation Documentation
getDocumentation = ask

-- |Replace all existing documentation
replaceDocumentation :: Documentation -> Update Documentation ()
replaceDocumentation = State.put

insert :: PkgInfo -> Update PackagesState Bool
insert pkg
    = do pkgsState <- State.get
         case PackageIndex.lookupPackageId (packageList pkgsState) (packageId pkg) of
           Nothing -> do State.put $ pkgsState { packageList = PackageIndex.insert pkg (packageList pkgsState) }
                         return True
           Just{}  -> do return False

-- NOTE! overwrites any existing data
bulkImport :: [PkgInfo] -> Users -> Update PackagesState ()
bulkImport newIndex users = do
  pkgsState <- State.get
  State.put pkgsState {
    packageList = PackageIndex.fromList newIndex,
    userDb = users
  }

-- |Replace all existing packages, users and reports
replacePackagesState :: PackagesState -> Update PackagesState ()
replacePackagesState = State.put

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

-- Returns 'Nothing' if the user name is in use
addUser :: UserName -> UserAuth -> Update PackagesState (Maybe UserId)
addUser userName auth = updateUsers' updateFn formatFn

  where updateFn = Users.add userName auth
        formatFn = id

-- Disables the indicated user
disableUser :: UserId -> Update PackagesState Bool
disableUser = updateUsers . Users.disable

-- Enables the indicated previously disabled user
enableUser :: UserId -> Update PackagesState Bool
enableUser = updateUsers . Users.enable

-- Deletes the indicated user. Cannot be re-enabled. The associated
-- user name is available for re-use 
deleteUser :: UserId -> Update PackagesState Bool
deleteUser = updateUsers . Users.delete

-- Re-set the user autenication info
replaceUserAuth :: UserId -> UserAuth -> Update PackagesState Bool
replaceUserAuth userId auth
    = updateUsers $ \users ->
      Users.replaceAuth users userId auth
    

-- updates the user db with a simpler function
updateUsers :: (Users -> Maybe Users) -> Update PackagesState Bool
updateUsers f = updateUsers' updateFn isJust

    where updateFn users = fmap (swap . (,) ()) $ f users
          swap (x,y) = (y,x)

-- Helper function for updating the users db
updateUsers' :: (Users -> Maybe (Users, a)) -> (Maybe a -> b) -> Update PackagesState b
updateUsers' f format = do
  state <- State.get
  let users = userDb state
      result = f users

  liftM format $ case result of
    Nothing -> return Nothing
    Just (users',a) -> do
      State.put state { userDb = users' }
      return (Just a)

lookupUserName :: UserName -> Query PackagesState (Maybe UserId)
lookupUserName = queryUsers . Users.lookupName

queryUsers :: (Users -> a) -> Query PackagesState a
queryUsers queryFn = liftM queryFn (asks userDb)

listGroupMembers :: UserGroup -> Query PackagesState [UserName]
listGroupMembers userGroup
    = do users <- asks userDb
         return [ Users.idToName users uid | uid <- Group.enumerate userGroup ]


$(mkMethods ''Documentation ['insertDocumentation
                            ,'lookupDocumentation
                            ,'hasDocumentation
                            ,'getDocumentation
                            ,'replaceDocumentation
                            ])

$(mkMethods ''PackagesState ['getPackagesState
                            ,'listGroupMembers
                            ,'bulkImport
                            ,'replacePackagesState
                            ,'insert
                            ,'addReport
                            ,'addBuildLog

                            --TODO: move these to a separate state component
                            -- User management
                            ,'addUser
                            ,'disableUser
                            ,'enableUser
                            ,'deleteUser
                            ,'replaceUserAuth
                            ,'lookupUserName
                            ])
