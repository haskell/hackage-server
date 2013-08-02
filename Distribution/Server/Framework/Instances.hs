{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, CPP #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'Typeable', 'Binary', 'Serialize', 'Text', and 'NFData' instances for various
-- types from Cabal, and other standard libraries.
--
-- Major version changes may break this module.
--

module Distribution.Server.Framework.Instances () where

import Distribution.Text
import Distribution.Server.Framework.MemSize

import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.PackageDescription
         (GenericPackageDescription(..))
import Distribution.Version (Version(..), VersionRange(..))

import Data.Typeable
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (Day(..))

import Control.DeepSeq

import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize)
import Data.SafeCopy (SafeCopy(getCopy, putCopy), contain)

import Happstack.Server

import Data.Maybe (fromJust)

import qualified Text.PrettyPrint as PP (text)
import Distribution.Compat.ReadP (readS_to_P)

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
#if !(MIN_VERSION_bytestring(0,10,0))
instance NFData ByteString where
    rnf bs = BS.length bs `seq` ()

instance NFData SBS.ByteString where
    rnf bs = bs `seq` ()
#endif

instance NFData Response where
    rnf res@(Response{}) = rnf (rsBody res) `seq` rnf (rsHeaders res)
    rnf _ = ()

instance NFData HeaderPair where
    rnf (HeaderPair a b) = rnf a `seq` rnf b

instance NFData PackageName where
    rnf (PackageName pkg) = rnf pkg

instance NFData PackageIdentifier where
    rnf (PackageIdentifier name version) = rnf name `seq` rnf version

#if !(MIN_VERSION_deepseq(1,3,0))
instance NFData Version where
    rnf (Version branch tags) = rnf branch `seq` rnf tags
#endif

#if !(MIN_VERSION_time(1,4,0))
instance NFData Day where
    rnf (ModifiedJulianDay a) = rnf a
#endif

instance MemSize Response where
    memSize (Response a b c d e) = memSize5 a b c d e
    memSize (SendFile{})         = 42

instance MemSize HeaderPair where
    memSize (HeaderPair a b) = memSize2 a b

instance MemSize RsFlags where
    memSize (RsFlags a) = memSize1 a

instance MemSize Length where
    memSize _ = memSize0

instance Text Day where
  disp  = PP.text . show 
  parse = readS_to_P (reads :: ReadS Day)

