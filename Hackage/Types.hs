{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, StandaloneDeriving #-}
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
import Distribution.PackageDescription (GenericPackageDescription(..), parsePackageDescription, ParseResult(..))
import Distribution.Text
         ( display )
import Distribution.Simple.Utils (intercalate)

import Data.Typeable
import System.FilePath ((</>), (<.>))

type Username = String
type Password = String

instance Read GenericPackageDescription where
    readsPrec _ s = case parsePackageDescription s of
        ParseFailed e -> [(undefined, "")]
        ParseOk _ x   -> [(x, "")]

deriving instance Typeable GenericPackageDescription
deriving instance Typeable PackageIdentifier

-- | We re-use @GenericPackageDescription@ and use the @package-url@
-- field to store the tarball URL.
data PkgInfo = PkgInfo {
    pkgInfoId :: PackageIdentifier,
    pkgDesc   :: GenericPackageDescription
  }
  deriving (Read, Show, Typeable)

instance Package PkgInfo where packageId = pkgInfoId

