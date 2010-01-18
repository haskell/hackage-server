{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, FlexibleContexts #-}

-- | 'Typeable' and 'Binary' instances for various types from Cabal
--
module Distribution.Server.Instances () where

import Distribution.Text

import Distribution.Package
         ( PackageIdentifier(..), PackageName(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(..) )
import Distribution.Version
         ( Version )

import qualified Data.Array.Unboxed as UA

import Data.Typeable
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (Day(..))

import qualified Data.Binary as Binary
import Data.Binary (Binary)

import qualified Happstack.State as Happs

import Data.Maybe (fromJust)

deriving instance Typeable PackageIdentifier
deriving instance Typeable GenericPackageDescription
deriving instance Typeable PackageName

instance Binary PackageIdentifier where
  put = Binary.put . show
  get = fmap read Binary.get

instance Happs.Version PackageName
instance Happs.Serialize PackageName where
    getCopy = Happs.contain textGet
    putCopy = Happs.contain . textPut

instance Happs.Version Version
instance Happs.Serialize Version where
    getCopy = Happs.contain textGet
    putCopy = Happs.contain . textPut

-- These assume that the text representations
-- for Cabal types will be stable over time
textGet :: Text a => Binary.Get a
textGet = (fromJust . simpleParse)  `fmap` Binary.get

textPut :: Text a => a -> Binary.Put
textPut = Binary.put . display

instance Happs.Version (UA.UArray ix e) where
    mode = Happs.Primitive

instance (Happs.Serialize ix, Happs.Serialize e, UA.Ix ix,
          UA.IArray UA.UArray e) => Happs.Serialize (UA.UArray ix e) where
    getCopy = Happs.contain $ do
                bounds <- Happs.safeGet
                assocs <- Happs.safeGet
                return $ UA.array bounds assocs

    putCopy arr = Happs.contain
                  (Happs.safePut (UA.bounds arr) >> Happs.safePut (UA.assocs arr))

instance Binary UTCTime where
  put time = do
    Binary.put (toModifiedJulianDay $ utctDay time)
    Binary.put (toRational $ utctDayTime time)
  get = do
    day  <- Binary.get
    secs <- Binary.get
    return (UTCTime (ModifiedJulianDay day) (fromRational secs))
