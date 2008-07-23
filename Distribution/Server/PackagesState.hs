{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses  #-}
module Distribution.Server.PackagesState where

import Distribution.Package (PackageIdentifier)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.PackageDescription (parsePackageDescription, ParseResult(..))
import Distribution.Server.Types (PkgInfo(..))
import qualified Distribution.Server.IndexUtils as PackageIndex (write)

import HAppS.State
import HAppS.Data.Serialize
import qualified Data.Binary as Binary

import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BS (unpack)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Codec.Compression.GZip as GZip

import Distribution.Simple.Utils (fromUTF8)

data PackagesState = PackagesState {
    packageList  :: PackageIndex.PackageIndex PkgInfo
  }
  deriving (Typeable)

instance Version PackagesState where
    mode = Versioned 0 Nothing

instance Serialize PackagesState where
  putCopy (PackagesState idx) = contain $ safePut $ PackageIndex.allPackages idx
  getCopy = contain $ do packages <- safeGet
                         return $ PackagesState { packageList = PackageIndex.fromList packages }


generatePackageIndex :: PackageIndex.PackageIndex PkgInfo -> ByteString
generatePackageIndex = GZip.compress . PackageIndex.write pkgData



instance Version PackageIdentifier where
  mode = Versioned 0 Nothing

instance Serialize PackageIdentifier where
  putCopy = contain . Binary.put . show
  getCopy = contain $ fmap read Binary.get

instance Version PkgInfo where
  mode = Versioned 0 Nothing

instance Serialize PkgInfo where
  putCopy pkgInfo = contain $ do safePut (pkgInfoId pkgInfo)
                                 Binary.put (pkgData pkgInfo)
  getCopy = contain $ do
    infoId <- safeGet
    bstring <- Binary.get
    return PkgInfo {
      pkgInfoId = infoId,
      pkgDesc   = case parse bstring of
                    -- XXX: Better error message?
                    ParseFailed e -> error $ "Internal error: " ++ show e
                    ParseOk _ x   -> x,
      pkgData   = bstring
    }
    where parse = parsePackageDescription . fromUTF8 . BS.unpack

--insert

bulkImport :: [PkgInfo] -> Update PackagesState ()
bulkImport newIndex
    = do pkgsState <- State.get
         State.put $ pkgsState { packageList = packageList pkgsState `mappend`
                                               PackageIndex.fromList newIndex }

getPackagesState :: Query PackagesState PackagesState
getPackagesState = ask

lookupPackageId :: PackageIdentifier -> Query PackagesState (Maybe PkgInfo)
lookupPackageId pkgid = do
  pkgsState <- ask
  return (PackageIndex.lookupPackageId (packageList pkgsState) pkgid)

$(mkMethods ''PackagesState ['lookupPackageId
                            ,'bulkImport
                            ,'getPackagesState])

instance Component PackagesState where
  type Dependencies PackagesState = End
  initialValue = PackagesState mempty

