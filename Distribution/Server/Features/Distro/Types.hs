{-# LANGUAGE
    DeriveDataTypeable
  , GeneralizedNewtypeDeriving
  , TemplateHaskell
  #-}



module Distribution.Server.Features.Distro.Types where

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize
import Distribution.Server.Users.State()
import Distribution.Server.Users.Group (UserIdSet)

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Distribution.Version as Version
import Distribution.Package

import Control.Applicative ((<$>))

import Distribution.Pretty (Pretty(..))
import Distribution.Parsec (Parsec(..))
import qualified Distribution.Compat.CharParsing as P

import qualified Text.PrettyPrint as Disp
import qualified Data.Char as Char

import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable


-- | Distribution names may contain letters, numbers and punctuation.
newtype DistroName = DistroName String
 deriving (Eq, Ord, Read, Show, Typeable, MemSize)

instance Pretty DistroName where
  pretty (DistroName name) = Disp.text name

instance Parsec DistroName where
  parsec = DistroName <$> P.munch1 (\c -> Char.isAlphaNum c || c `elem` "-_()[]{}=$,;")

-- | Listing of known distirbutions and their maintainers
data Distributions = Distributions {
    nameMap :: !(Map.Map DistroName UserIdSet)
}
 deriving (Eq, Typeable, Show)

-- | Listing of which distirbutions have which version of particular
-- packages.
data DistroVersions = DistroVersions {
    packageDistroMap :: !(Map.Map PackageName (Map.Map DistroName DistroPackageInfo)),
    distroMap  :: !(Map.Map DistroName (Set.Set PackageName))
} deriving (Eq, Typeable, Show)

data DistroPackageInfo
    = DistroPackageInfo
      { distroVersion :: Version.Version
      , distroUrl     :: String
      }
 deriving (Eq, Typeable, Show)

$(deriveSafeCopy 0 'base ''DistroName)
$(deriveSafeCopy 0 'base ''Distributions)
$(deriveSafeCopy 0 'base ''DistroVersions)
$(deriveSafeCopy 0 'base ''DistroPackageInfo)

instance MemSize Distributions where
    memSize (Distributions a) = memSize1 a

instance MemSize DistroVersions where
    memSize (DistroVersions a b) = memSize2 a b

instance MemSize DistroPackageInfo where
    memSize (DistroPackageInfo a b) = memSize2 a b
