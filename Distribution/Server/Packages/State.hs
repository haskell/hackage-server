{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}
module Distribution.Server.Packages.State where

import Distribution.Server.Instances ()
import Distribution.Server.Users.State ()

import Distribution.Package (PackageIdentifier,PackageName,Package(packageId))
import qualified Distribution.Server.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types (PkgInfo(..))
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Group (UserList)
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Util.BlobStorage (BlobId)
import qualified Distribution.Server.BuildReport.BuildReports as BuildReports
import Distribution.Server.BuildReport.BuildReports (BuildReports,BuildReportId,BuildLog)
import Distribution.Server.BuildReport.BuildReport (BuildReport)

import Happstack.State
import qualified Data.Binary as Binary

import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Monoid

import qualified Data.Map as Map

---------------------------------- Index of metadata and tarballs
data PackagesState = PackagesState {
    packageList  :: !(PackageIndex.PackageIndex PkgInfo)
  }
  deriving (Typeable, Show)

instance Component PackagesState where
  type Dependencies PackagesState = End
  initialValue = PackagesState {
    packageList  = mempty
  }

instance Version PackagesState where
    mode = Versioned 0 Nothing

instance Serialize PackagesState where
  putCopy (PackagesState idx) = contain $ do
    safePut $ PackageIndex.allPackages idx
  getCopy = contain $ do
    packages <- safeGet
    return PackagesState {
      packageList  = PackageIndex.fromList packages
    }

instance Version PkgInfo where
  mode = Versioned 0 Nothing

instance Serialize PkgInfo where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get

insert :: PkgInfo -> Update PackagesState Bool
insert pkg
    = do pkgsState <- State.get
         case PackageIndex.lookupPackageId (packageList pkgsState) (packageId pkg) of
           Nothing -> do State.put $ pkgsState { packageList = PackageIndex.insert pkg (packageList pkgsState) }
                         return True
           Just{}  -> do return False

-- NOTE! overwrites any existing data
bulkImport :: [PkgInfo] -> Update PackagesState ()
bulkImport newIndex = do
  pkgsState <- State.get
  State.put pkgsState {
    packageList = PackageIndex.fromList newIndex
  }

-- |Replace all existing packages and reports
replacePackagesState :: PackagesState -> Update PackagesState ()
replacePackagesState = State.put

getPackagesState :: Query PackagesState PackagesState
getPackagesState = ask


$(mkMethods ''PackagesState ['getPackagesState
                            ,'bulkImport
                            ,'replacePackagesState
                            ,'insert
                            ])
---------------------------------- Documentation
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
    = State.modify $ \doc -> doc {documentation = Map.insert pkgId blob (documentation doc)}

getDocumentation :: Query Documentation Documentation
getDocumentation = ask

-- |Replace all existing documentation
replaceDocumentation :: Documentation -> Update Documentation ()
replaceDocumentation = State.put

$(mkMethods ''Documentation ['insertDocumentation
                            ,'lookupDocumentation
                            ,'hasDocumentation
                            ,'getDocumentation
                            ,'replaceDocumentation
                            ])
-------------------------------- Build reports
instance Version BuildReports where
  mode = Versioned 0 Nothing

instance Serialize BuildReports where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get

instance Component BuildReports where
  type Dependencies BuildReports = End
  initialValue = BuildReports.empty

addReport :: BuildReport -> Update BuildReports BuildReportId
addReport report
    = do buildReports <- State.get
         let (reports, reportId) = BuildReports.addReport buildReports report
         State.put reports
         return reportId

addBuildLog :: BuildReportId -> BuildLog -> Update BuildReports Bool
addBuildLog reportId buildLog
    = do buildReports <- State.get
         case BuildReports.addBuildLog buildReports reportId buildLog of
           Nothing -> return False
           Just reports -> State.put reports >> return True

getBuildReports :: Query BuildReports BuildReports
getBuildReports = ask

replaceBuildReports :: BuildReports -> Update BuildReports ()
replaceBuildReports = State.put

$(mkMethods ''BuildReports ['addReport
                           ,'addBuildLog
                           ,'getBuildReports
                           ,'replaceBuildReports
                           ])

-------------------------------- Maintainer list
data PackageMaintainers = PackageMaintainers {
    maintainers :: Map.Map PackageName UserList
} deriving (Show, Typeable)

instance Version PackageMaintainers where
  mode = Versioned 0 Nothing
$(deriveSerialize ''PackageMaintainers)

instance Component PackageMaintainers where
  type Dependencies PackageMaintainers = End
  initialValue = PackageMaintainers Map.empty

getPackageMaintainers :: PackageName -> Query PackageMaintainers (Maybe UserList)
getPackageMaintainers name = fmap (Map.lookup name) (asks maintainers)

modifyPackageMaintainers :: PackageName -> (UserList -> UserList) -> Update PackageMaintainers ()
modifyPackageMaintainers name func = State.modify (\pm -> pm {maintainers = Map.update (Just . func) name (maintainers pm) })

addPackageMaintainer :: PackageName -> UserId -> Update PackageMaintainers ()
addPackageMaintainer name uid = modifyPackageMaintainers name (Group.add uid)

removePackageMaintainer :: PackageName -> UserId -> Update PackageMaintainers ()
removePackageMaintainer name uid = modifyPackageMaintainers name (Group.remove uid)

$(mkMethods ''PackageMaintainers ['getPackageMaintainers
                                 ,'addPackageMaintainer
                                 ,'removePackageMaintainer
                                 ])

