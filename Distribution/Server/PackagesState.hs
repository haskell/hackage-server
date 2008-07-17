{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses  #-}
module Distribution.Server.PackagesState where

import Distribution.Package (PackageIdentifier,Package)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.PackageDescription (parsePackageDescription, ParseResult(..))
import Hackage.Types (PkgInfo(..))

import HAppS.State
import HAppS.Data.Serialize
import Data.Binary

import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Monoid

import Data.ByteString.Lazy.Char8 (unpack,empty,ByteString)


data PackagesState = PackagesState { packageList  :: PackageIndex.PackageIndex PkgInfo
                                   , packageIndex :: ByteString }
  deriving (Typeable)

instance Version PackagesState where
    mode = Versioned 0 Nothing

instance Serialize PackagesState where
  putCopy (PackagesState idx _) = contain $ safePut $ PackageIndex.allPackages idx
  getCopy = contain $ do packages <- safeGet
                         return $ cachePackagesState $
                                  PackagesState { packageList = PackageIndex.fromList packages
                                                , packageIndex = empty }

cachePackagesState :: PackagesState -> PackagesState
cachePackagesState pkgsState = pkgsState { packageIndex = index }
    where index = generatePackageIndex (packageList pkgsState)

generatePackageIndex :: PackageIndex.PackageIndex PkgInfo -> ByteString
generatePackageIndex = const empty



instance Version PackageIdentifier where
  mode = Versioned 0 Nothing

instance Serialize PackageIdentifier where
  putCopy = contain . put . show
  getCopy = contain $ fmap read get

instance Version PkgInfo where
  mode = Versioned 0 Nothing

instance Serialize PkgInfo where
  putCopy pkgInfo = contain $ do safePut (pkgInfoId pkgInfo)
                                 put (pkgData pkgInfo)
  getCopy = contain $ do infoId <- safeGet
                         bstring <- get
                         return $ PkgInfo { pkgInfoId = infoId
                                          , pkgDesc   = case parsePackageDescription (unpack bstring) of
                                                          -- XXX: Better error message?
                                                          ParseFailed e -> error $ "Internal error: " ++ show e
                                                          ParseOk _ x   -> x
                                          , pkgData   = bstring }

--insert

bulkImport :: [PkgInfo] -> Update PackagesState ()
bulkImport newIndex
    = do pkgsState <- State.get
         State.put $ cachePackagesState $ pkgsState { packageList = packageList pkgsState `mappend`
                                                                    PackageIndex.fromList newIndex }


lookupPackageId :: PackageIdentifier -> Query PackagesState (Maybe PkgInfo)
lookupPackageId pkgid = do
  pkgsState <- ask
  return (PackageIndex.lookupPackageId (packageList pkgsState) pkgid)

$(mkMethods ''PackagesState ['lookupPackageId
                            ,'bulkImport])

instance Component PackagesState where
  type Dependencies PackagesState = End
  initialValue = PackagesState mempty empty

