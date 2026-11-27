{-# LANGUAGE TemplateHaskell #-}
module Distribution.Server.Features.Core.Types (
    AddPackageResult(..)
  ) where

import Data.SafeCopy (base, deriveSafeCopy)

-- | The outcome of adding a package version.
data AddPackageResult
  = PackageAdded
    -- | This package version already exists.
  | PackageVersionExists
    -- | The versions of packages whose name equals this one modulo case are
    -- not the expected ones.
  | PackagesChangedModuloCase
  deriving (Eq, Show)

deriveSafeCopy 0 'base ''AddPackageResult
