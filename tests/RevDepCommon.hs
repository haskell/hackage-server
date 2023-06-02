{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module RevDepCommon where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Vector as Vector
import           Data.Time (UTCTime(..), fromGregorian)
import           GHC.Generics (Generic)

import           Distribution.Server.Users.Types (UserId(..))
import           Distribution.Package (PackageIdentifier(..), PackageName, mkPackageName, unPackageName)
import           Distribution.Server.Packages.Types (PkgInfo(..))
import           Distribution.Server.Packages.Types (CabalFileText(..))
import           Distribution.Version (mkVersion, versionNumbers)

data Package b =
  Package
    { pName :: b
    , pVersion :: Int
    , pDeps :: [ b ]
    }
  deriving (Ord, Show, Eq)

packToPkgInfo :: Show b => Package b -> PkgInfo
packToPkgInfo Package {pName, pVersion, pDeps} =
    mkPackage (mkPackageName $ show pName) [pVersion] (depsToBS pDeps)

mkPackage :: PackageName -> [Int] -> [BSL.ByteString] -> PkgInfo
mkPackage name intVersion depends =
  mkPackageWithCabalFileSuffix name intVersion $
    if depends /= []
      then "library\n  build-depends: " <> BSL.intercalate "," depends
      else ""

mkPackageWithCabalFileSuffix :: PackageName -> [Int] -> BSL.ByteString -> PkgInfo
mkPackageWithCabalFileSuffix name intVersion cabalFileSuffix =
  let
    version = mkVersion intVersion
    -- e.g. "2.3" for [2,3]
    dotVersion :: BSL.ByteString
    dotVersion = BSL.fromStrict . Char8.intercalate "." . map (Char8.pack . show) $ versionNumbers version
    cabalFilePrefix :: BSL.ByteString
    cabalFilePrefix = "\
\name: " <> BSL.fromStrict (Char8.pack $ unPackageName name) <> "\n\
\version: " <> dotVersion <> "\n"
    cabalFile :: CabalFileText
    cabalFile = CabalFileText $ cabalFilePrefix <> cabalFileSuffix
  in
  PkgInfo
  (PackageIdentifier name version)
  (Vector.fromList [(cabalFile, (UTCTime (fromGregorian 2020 1 1) 0, UserId 1))])
  mempty

depsToBS :: Show b => [ b ] -> [BSL.ByteString]
depsToBS =
  map (BSL.fromStrict . Char8.pack . show)


newtype TestPackage = TestPackage Word
  deriving stock Generic
  deriving newtype (Bounded, Enum, Eq, Ord)

instance Show TestPackage where
  show (TestPackage word) = "package" <> show word
