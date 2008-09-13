{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses  #-}
module Distribution.Server.State where

import Distribution.Server.Instances ()

import Distribution.Package (PackageIdentifier,Package(packageId))
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Server.Types (PkgInfo(..))
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Users (Users)
import Distribution.Server.Util.BlobStorage (BlobId)
import qualified Distribution.Server.BuildReport.BuildReports as BuildReports
import Distribution.Server.BuildReport.BuildReports (BuildReports,BuildReportId,BuildLog)
import Distribution.Server.BuildReport.BuildReport (BuildReport)

import HAppS.State
import HAppS.Data.Serialize
import qualified Data.Binary as Binary

import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Monoid
import Data.Time.Clock (UTCTime(..))

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

$(mkMethods ''PackagesState ['getPackagesState
                            ,'bulkImport
                            ,'insert
                            ,'addReport
                            ,'addBuildLog
                            ])
