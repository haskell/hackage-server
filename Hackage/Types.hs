{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Types
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- All data types for the entire cabal-install system gathered here to avoid some .hs-boot files.
-----------------------------------------------------------------------------
module Hackage.Types where

import Distribution.Package
         ( PackageIdentifier(..), Package(..), Dependency )
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Text
         ( display )
import Distribution.Simple.Utils (intercalate)

import Data.Typeable
import System.FilePath ((</>), (<.>))

type Username = String
type Password = String

-- | We re-use @GenericPackageDescription@ and use the @package-url@
-- field to store the tarball URL.
data PkgInfo = PkgInfo {
    pkgInfoId :: PackageIdentifier,
    pkgDesc   :: GenericPackageDescription
  }
  deriving (Read, Show, Typeable)

instance Package PkgInfo where packageId = pkgInfoId

