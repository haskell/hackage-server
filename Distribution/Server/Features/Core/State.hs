{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, BangPatterns #-}

module Distribution.Server.Features.Core.State where

import Distribution.Package
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Framework.MemSize

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Monoid
import Data.Time (UTCTime)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (maybeToList)


---------------------------------- Index of metadata and tarballs
data PackagesState = PackagesState {
    packageList  :: !(PackageIndex PkgInfo)
  }
  deriving (Eq, Typeable, Show)

deriveSafeCopy 0 'base ''PackagesState

instance MemSize PackagesState where
    memSize (PackagesState a) = 2 + memSize a

initialPackagesState :: PackagesState
initialPackagesState = PackagesState {
    packageList = mempty
  }

addPackage :: PackageId -> CabalFileText -> UploadInfo -> Maybe PkgTarball
           -> Update PackagesState (Maybe PkgInfo)
addPackage pkgid cabalfile uploadinfo mtarball = do 
    PackagesState pkgindex <- State.get
    case PackageIndex.lookupPackageId pkgindex pkgid of
      Just _  -> return Nothing
      Nothing -> do
        let !pkginfo = PkgInfo {
              pkgInfoId     = pkgid,
              pkgData       = cabalfile,
              pkgTarball    = [ (tarball, uploadinfo)
                              | tarball <- maybeToList mtarball ],
              pkgDataOld    = [],
              pkgUploadData = uploadinfo
            }
            pkgindex' = PackageIndex.insert pkginfo pkgindex
        State.put $! PackagesState pkgindex'
        return (Just pkginfo)

deletePackage :: PackageId -> Update PackagesState (Maybe PkgInfo)
deletePackage pkgid = do
    PackagesState pkgindex <- State.get
    case PackageIndex.lookupPackageId pkgindex pkgid of
      Nothing      -> return Nothing
      Just pkginfo -> do
        let pkgindex' = PackageIndex.deletePackageId pkgid pkgindex
        State.put $! PackagesState pkgindex'
        return (Just pkginfo)

addPackageRevision :: PackageId -> CabalFileText -> UploadInfo
                   -> Update PackagesState (Maybe PkgInfo, PkgInfo)
addPackageRevision pkgid cabalfile uploadinfo = do
    PackagesState pkgindex <- State.get
    case PackageIndex.lookupPackageId pkgindex pkgid of
      Just pkginfo -> do
        let !pkginfo' = pkginfo {
              pkgData       = cabalfile,
              pkgUploadData = uploadinfo,
              pkgDataOld    = (pkgData pkginfo, pkgUploadData pkginfo)
                            : pkgDataOld pkginfo
            }
            pkgindex' = PackageIndex.insert pkginfo' pkgindex
        State.put $! PackagesState pkgindex'
        return (Just pkginfo, pkginfo')
      Nothing -> do
        let !pkginfo = PkgInfo {
              pkgInfoId     = pkgid,
              pkgData       = cabalfile,
              pkgTarball    = [],
              pkgDataOld    = [],
              pkgUploadData = uploadinfo
            }
            pkgindex' = PackageIndex.insert pkginfo pkgindex
        State.put $! PackagesState pkgindex'
        return (Nothing, pkginfo)

addPackageTarball :: PackageId -> PkgTarball -> UploadInfo
                  -> Update PackagesState (Maybe (PkgInfo, PkgInfo))
addPackageTarball pkgid tarball uploadinfo =
    alterPackage pkgid $ \pkginfo ->
      pkginfo {
        pkgTarball = (tarball, uploadinfo) : pkgTarball pkginfo
      }

setPackageUploader :: PackageId -> UserId
                   -> Update PackagesState (Maybe (PkgInfo, PkgInfo))
setPackageUploader pkgid uid =
    alterPackage pkgid $ \pkginfo ->
      pkginfo {
        pkgUploadData = (pkgUploadTime pkginfo, uid)
      }

setPackageUploadTime :: PackageId -> UTCTime
                     -> Update PackagesState (Maybe (PkgInfo, PkgInfo))
setPackageUploadTime pkgid time =
    alterPackage pkgid $ \pkginfo ->
      pkginfo {
        pkgUploadData = (time, pkgUploadUser pkginfo)
      }

alterPackage :: PackageId -> (PkgInfo -> PkgInfo)
             -> Update PackagesState (Maybe (PkgInfo, PkgInfo))
alterPackage pkgid alter = do
    PackagesState pkgindex <- State.get
    case PackageIndex.lookupPackageId pkgindex pkgid of
      Nothing      -> return Nothing
      Just pkginfo -> do
        let !pkginfo' = alter pkginfo
            pkgindex' = PackageIndex.insert pkginfo' pkgindex
        State.put $! PackagesState pkgindex'
        return (Just (pkginfo, pkginfo'))

-- Keep old versions for a bit, for acid-state log compatability

insertPkgIfAbsent :: PkgInfo -> Update PackagesState Bool
insertPkgIfAbsent pkg = do
    pkgsState <- State.get
    case PackageIndex.lookupPackageId (packageList pkgsState) (packageId pkg) of
        Nothing -> do State.put $ pkgsState { packageList = PackageIndex.insert pkg (packageList pkgsState) }
                      return True
        Just{}  -> do return False

-- could also return something to indicate existence
mergePkg :: PkgInfo -> Update PackagesState ()
mergePkg pkg = State.modify $ \pkgsState -> pkgsState { packageList = PackageIndex.insertWith mergeFunc pkg (packageList pkgsState) }
  where
    mergeFunc newPkg oldPkg =
      let cabalH : cabalT = sortDesc $ (pkgData newPkg, pkgUploadData newPkg)
                                     : (pkgData oldPkg, pkgUploadData oldPkg)
                                     : pkgDataOld newPkg
                                    ++ pkgDataOld oldPkg
          tarballs        = sortDesc $ pkgTarball newPkg
                                    ++ pkgTarball oldPkg
      in PkgInfo {
             pkgInfoId     = pkgInfoId oldPkg -- should equal pkgInfoId newPkg
           , pkgData       = fst cabalH
           , pkgTarball    = tarballs
           , pkgDataOld    = cabalT
           , pkgUploadData = snd cabalH
           }

    sortDesc :: Ord a => [(a1, (a, b))] -> [(a1, (a, b))]
    sortDesc = sortBy $ flip (comparing (fst . snd))

deletePackageVersion :: PackageId -> Update PackagesState ()
deletePackageVersion pkg = State.modify $ \pkgsState -> pkgsState { packageList = deleteVersion (packageList pkgsState) }
    where deleteVersion = PackageIndex.deletePackageId pkg

replacePackageUploader :: PackageId -> UserId -> Update PackagesState (Either String (PkgInfo, PkgInfo))
replacePackageUploader pkg uid = modifyPkgInfo pkg $ \pkgInfo -> pkgInfo { pkgUploadData = (pkgUploadTime pkgInfo, uid) }

replacePackageUploadTime :: PackageId -> UTCTime -> Update PackagesState (Either String (PkgInfo, PkgInfo))
replacePackageUploadTime pkg time = modifyPkgInfo pkg $ \pkgInfo -> pkgInfo { pkgUploadData = (time, pkgUploadUser pkgInfo) }

addTarball :: PackageId -> PkgTarball -> UploadInfo -> Update PackagesState (Either String (PkgInfo, PkgInfo))
addTarball pkg tarball uploadInfo = modifyPkgInfo pkg $ \pkgInfo -> pkgInfo { pkgTarball = (tarball, uploadInfo) : pkgTarball pkgInfo }

modifyPkgInfo :: PackageId -> (PkgInfo -> PkgInfo) -> Update PackagesState (Either String (PkgInfo, PkgInfo))
modifyPkgInfo pkg f = do
  pkgsState <- State.get
  case PackageIndex.lookupPackageId (packageList pkgsState) pkg of
    Nothing -> return (Left "No such package")
    Just pkgInfo -> do
      let pkgInfo' = f pkgInfo
      State.put $ pkgsState { packageList = PackageIndex.insert pkgInfo' (packageList pkgsState) }
      return $ Right (pkgInfo, pkgInfo')

-- |Replace all existing packages and reports
replacePackagesState :: PackagesState -> Update PackagesState ()
replacePackagesState = State.put

getPackagesState :: Query PackagesState PackagesState
getPackagesState = ask

makeAcidic ''PackagesState ['getPackagesState
                           ,'replacePackagesState
                           ,'addPackage
                           ,'deletePackage
                           ,'addPackageRevision
                           ,'addPackageTarball
                           ,'setPackageUploader
                           ,'setPackageUploadTime

                             -- Keep old versions for a bit,
                             -- for acid-state log compatability
                           ,'replacePackageUploadTime
                           ,'replacePackageUploader
                           ,'addTarball
                           ,'insertPkgIfAbsent
                           ,'mergePkg
                           ,'deletePackageVersion
                           ]

