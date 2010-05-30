{-# LANGUAGE
    DeriveDataTypeable
  , GeneralizedNewtypeDeriving
  , TemplateHaskell
  #-}



module Distribution.Server.Distributions.Types where

import Distribution.Server.Instances()

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Distribution.Version as Version
import Distribution.Package

import Control.Applicative ((<$>))

import Distribution.Text
         ( Text(..) )

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint          as Disp
import qualified Data.Char as Char

import Happstack.State

import Data.Typeable


-- | Distribution names may contain letters, numbers and punctuation.
newtype DistroName = DistroName String
 deriving (Eq, Ord, Read, Show, Typeable)
instance Version DistroName
instance Text DistroName where
  disp (DistroName name) = Disp.text name
  parse = DistroName <$> Parse.munch1 (\c -> Char.isAlphaNum c || c `elem` "-_()[]{}=$,;")


-- | Listing of known distirbutions 
data Distributions = Distributions
    { name_map :: !(Set.Set DistroName)
    }
 deriving (Typeable, Show)
instance Version Distributions

-- | Listing of which distirbutions have which version of particular
-- packages.
data DistroVersions = DistroVersions
    { package_map :: !(Map.Map PackageName (Map.Map DistroName DistroPackageInfo))
    , distro_map  :: !(Map.Map DistroName (Set.Set PackageName))
    }
 deriving (Typeable, Show)
instance Version DistroVersions


data DistroPackageInfo
    = DistroPackageInfo
      { distro_version :: Version.Version
      , distro_url     :: String
      }
 deriving (Typeable, Show)
instance Version DistroPackageInfo

-- happstack magic
$(deriveSerializeFor
  [ ''DistroName
  , ''Distributions
  , ''DistroVersions
  , ''DistroPackageInfo
  ]
 )
