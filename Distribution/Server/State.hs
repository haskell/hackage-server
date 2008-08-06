{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses  #-}
module Distribution.Server.State where

import Distribution.Package (PackageIdentifier,Package(packageId))
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.PackageDescription (parsePackageDescription, ParseResult(..))
import Distribution.Server.Types (PkgInfo(..))
import Distribution.Server.BlobStorage (BlobId)
import qualified Distribution.Server.BuildReports as BuildReports
import Distribution.Server.BuildReports (BuildReports,BuildReportId,BuildLog)
import Distribution.Server.BuildReport (BuildReport)

import HAppS.State
import HAppS.Data.Serialize
import qualified Data.Binary as Binary

import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BS (unpack)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (Day(..))

import Distribution.Simple.Utils (fromUTF8)

data PackagesState = PackagesState {
    packageList  :: !(PackageIndex.PackageIndex PkgInfo),
    buildReports :: !BuildReports
  }
  deriving Typeable

instance Component PackagesState where
  type Dependencies PackagesState = End
  initialValue = PackagesState {
    packageList  = mempty,
    buildReports = BuildReports.empty
  }

instance Version PackagesState where
    mode = Versioned 0 Nothing

instance Serialize PackagesState where
  putCopy (PackagesState idx rpts) = contain $ do
    safePut $ PackageIndex.allPackages idx
    safePut rpts
  getCopy = contain $ do
    packages <- safeGet
    reports  <- safeGet
    return PackagesState {
      packageList  = PackageIndex.fromList packages,
      buildReports = reports
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
  putCopy pkgInfo = contain $ do
    safePut (pkgInfoId pkgInfo)
    safePut (pkgUploadTime pkgInfo)
    safePut (pkgUploadUser pkgInfo)
    safePut (pkgUploadOld pkgInfo)
    safePut (pkgTarball pkgInfo)
    Binary.put (pkgData pkgInfo)

  getCopy = contain $ do
    infoId <- safeGet
    mtime  <- safeGet
    user   <- safeGet
    old    <- safeGet
    tarball<- safeGet
    bstring <- Binary.get
    return PkgInfo {
      pkgInfoId = infoId,
      pkgDesc   = case parse bstring of
                    -- XXX: Better error message?
                    ParseFailed e -> error $ "Internal error: " ++ show e
                    ParseOk _ x   -> x,
      pkgUploadTime = mtime,
      pkgUploadUser = user,
      pkgUploadOld  = old,
      pkgData   = bstring,
      pkgTarball= tarball
    }
    where parse = parsePackageDescription . fromUTF8 . BS.unpack

deriving instance Typeable UTCTime

instance Version UTCTime where
  mode = Versioned 0 Nothing

instance Serialize UTCTime where
  putCopy = contain . Binary.put
  getCopy = contain Binary.get

instance Binary.Binary UTCTime where
  put time = do
    Binary.put (toModifiedJulianDay $ utctDay time)
    Binary.put (toRational $ utctDayTime time)
  get = do
    day  <- Binary.get
    secs <- Binary.get
    return (UTCTime (ModifiedJulianDay day) (fromRational secs))

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

bulkImport :: [PkgInfo] -> Update PackagesState ()
bulkImport newIndex
    = do pkgsState <- State.get
         State.put $ pkgsState { packageList = packageList pkgsState `mappend`
                                               PackageIndex.fromList newIndex }

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
