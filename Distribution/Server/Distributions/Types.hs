{-# LANGUAGE
    DeriveDataTypeable
  , GeneralizedNewtypeDeriving
  , TemplateHaskell
  #-}



module Distribution.Server.Distributions.Types where

import Distribution.Server.Instances()
import Distribution.Server.Users.State()
import Distribution.Server.Users.Group (UserList)

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Distribution.Version as Version
import Distribution.Package

import Control.Applicative ((<$>))

import Distribution.Text (Text(..))

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


-- | Listing of known distirbutions and their maintainers
data Distributions = Distributions {
    nameMap :: !(Map.Map DistroName UserList)
}
 deriving (Typeable, Show)
instance Version Distributions

-- | Listing of which distirbutions have which version of particular
-- packages.
data DistroVersions = DistroVersions {
    packageDistroMap :: !(Map.Map PackageName (Map.Map DistroName DistroPackageInfo)),
    distroMap  :: !(Map.Map DistroName (Set.Set PackageName))
} deriving (Typeable, Show)
instance Version DistroVersions

data DistroPackageInfo
    = DistroPackageInfo
      { distroVersion :: Version.Version
      , distroUrl     :: String
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
