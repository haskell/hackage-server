{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleContexts #-}

-- | 'Typeable' and 'Binary' instances for various types from Cabal
-- Major version changes may break this module.
--

module Distribution.Server.Instances () where

import Distribution.Text

import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.PackageDescription (GenericPackageDescription(..))
import Distribution.Version (Version(..), VersionRange(..))

import qualified Data.Array.Unboxed as UA

import Data.Typeable
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (Day(..))

import Control.DeepSeq

import qualified Data.Binary as Binary
import Data.Binary (Binary)

import Happstack.State hiding (Version)
import qualified Happstack.State as Happs
import Happstack.Server

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)

deriving instance Typeable PackageIdentifier
deriving instance Typeable GenericPackageDescription
deriving instance Typeable PackageName
deriving instance Typeable VersionRange

instance Binary PackageIdentifier where
  put = Binary.put . show
  get = fmap read Binary.get

instance Happs.Version PackageIdentifier
instance Serialize PackageIdentifier where
  putCopy = contain . Binary.put . show
  getCopy = contain $ fmap read Binary.get

instance Happs.Version PackageName
instance Serialize PackageName where
    getCopy = contain textGet
    putCopy = contain . textPut

instance Happs.Version Version
instance Serialize Version where
    getCopy = contain textGet
    putCopy = contain . textPut

instance Happs.Version VersionRange
instance Serialize VersionRange where
    getCopy = contain textGet
    putCopy = contain . textPut

instance FromReqURI PackageIdentifier where
  fromReqURI = simpleParse

instance FromReqURI PackageName where
  fromReqURI = simpleParse

instance FromReqURI Version where
  fromReqURI = simpleParse

-- These assume that the text representations
-- for Cabal types will be stable over time
textGet :: Text a => Binary.Get a
textGet = (fromJust . simpleParse)  `fmap` Binary.get

textPut :: Text a => a -> Binary.Put
textPut = Binary.put . display

instance Happs.Version (UA.UArray ix e) where
    mode = Primitive

instance (Serialize ix, Serialize e, UA.Ix ix,
          UA.IArray UA.UArray e) => Serialize (UA.UArray ix e) where
    getCopy = contain $ do
                bounds <- safeGet
                assocs <- safeGet
                return $ UA.array bounds assocs

    putCopy arr = contain
                  (safePut (UA.bounds arr) >> safePut (UA.assocs arr))

instance Binary UTCTime where
  put time = do
    Binary.put (toModifiedJulianDay $ utctDay time)
    Binary.put (toRational $ utctDayTime time)
  get = do
    day  <- Binary.get
    secs <- Binary.get
    return (UTCTime (ModifiedJulianDay day) (fromRational secs))

-- rough versions of RNF for these
instance NFData ByteString where
    rnf bs = BS.length bs `seq` ()

instance NFData Response where
    rnf res@(Response{}) = rnf $ rsBody res
    rnf _ = ()

instance NFData PackageName where
    rnf (PackageName pkg) = rnf pkg

instance NFData Version where
    rnf (Version cont tags) = rnf cont `seq` rnf tags

instance NFData PackageIdentifier where
    rnf (PackageIdentifier name version) = rnf name `seq` rnf version

instance NFData Day where
    rnf (ModifiedJulianDay day) = rnf day


