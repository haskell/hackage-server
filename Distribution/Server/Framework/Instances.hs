{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleContexts #-}

-- | 'Typeable', 'Binary', 'Serialize', and 'NFData' instances for various
-- types from Cabal, and other standard libraries.
--
-- Major version changes may break this module.
--

module Distribution.Server.Framework.Instances () where

import Distribution.Text

import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.PackageDescription (GenericPackageDescription(..))
import Distribution.Version (Version(..), VersionRange(..))

import Data.Typeable
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (Day(..))

import Control.DeepSeq

import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.SafeCopy (SafeCopy(getCopy, putCopy), contain)

import Happstack.Server

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)

deriving instance Typeable PackageIdentifier
deriving instance Typeable GenericPackageDescription
deriving instance Typeable PackageName
deriving instance Typeable VersionRange

instance Serialize PackageIdentifier where
  put = Serialize.put . show
  get = fmap read Serialize.get

instance SafeCopy PackageIdentifier where
  putCopy = contain . Serialize.put . show
  getCopy = contain $ fmap read Serialize.get

instance SafeCopy PackageName where
    getCopy = contain textGet
    putCopy = contain . textPut

instance SafeCopy Version where
    getCopy = contain textGet
    putCopy = contain . textPut

instance SafeCopy VersionRange where
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
textGet :: Text a => Serialize.Get a
textGet = (fromJust . simpleParse)  `fmap` Serialize.get

textPut :: Text a => a -> Serialize.Put
textPut = Serialize.put . display

instance Serialize UTCTime where
  put time = do
    Serialize.put (toModifiedJulianDay $ utctDay time)
    Serialize.put (toRational $ utctDayTime time)
  get = do
    day  <- Serialize.get
    secs <- Serialize.get
    return (UTCTime (ModifiedJulianDay day) (fromRational secs))

-- rough versions of RNF for these
instance NFData ByteString where
    rnf bs = BS.length bs `seq` ()

instance NFData Response where
    rnf res@(Response{}) = rnf $ rsBody res
    rnf _ = ()

instance NFData PackageName where
    rnf (PackageName pkg) = rnf pkg

instance NFData PackageIdentifier where
    rnf (PackageIdentifier name version) = rnf name `seq` rnf version

