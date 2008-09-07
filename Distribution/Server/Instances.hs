{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}

-- | 'Typeable' and 'Binary' instances for various types from Cabal
--
module Distribution.Server.Instances () where

import Distribution.Package
         ( PackageIdentifier(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(..) )

import Data.Typeable
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (Day(..))

import qualified Data.Binary as Binary
import Data.Binary (Binary)

deriving instance Typeable UTCTime
deriving instance Typeable PackageIdentifier
deriving instance Typeable GenericPackageDescription

instance Binary PackageIdentifier where
  put = Binary.put . show
  get = fmap read Binary.get

instance Binary UTCTime where
  put time = do
    Binary.put (toModifiedJulianDay $ utctDay time)
    Binary.put (toRational $ utctDayTime time)
  get = do
    day  <- Binary.get
    secs <- Binary.get
    return (UTCTime (ModifiedJulianDay day) (fromRational secs))
