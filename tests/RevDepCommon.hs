{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, DeriveGeneric #-}
module RevDepCommon where

import           GHC.Generics (Generic)
import Distribution.Server.Users.Types (UserId(..))
import           Data.List (intercalate)
import Distribution.Package (PackageIdentifier(..), PackageName, mkPackageName, unPackageName)
import Distribution.Server.Packages.Types (PkgInfo(..))
import qualified Data.Vector as Vector
import Distribution.Server.Packages.Types (CabalFileText(..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as Char8
import           Data.Time (UTCTime(..), fromGregorian)
import Distribution.Version (mkVersion, versionNumbers)

data Package b =
  Package
    { name :: b
    , version :: Int
    , deps :: [ b ]
    }
  deriving (Ord, Show, Eq)

packToPkgInfo :: Show b => Package b -> PkgInfo
packToPkgInfo Package {name, version, deps} =
    mkPackage (mkPackageName $ show name) [version] (depsToBS deps)

mkPackage :: PackageName -> [Int] -> BSL.ByteString -> PkgInfo
mkPackage name intVersion depends =
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
    cabalFile = CabalFileText $ cabalFilePrefix <> if depends /= "" then "library\n  build-depends: " <> depends else ""
  in
  PkgInfo
  (PackageIdentifier name version)
  (Vector.fromList [(cabalFile, (UTCTime (fromGregorian 2020 1 1) 0, UserId 1))])
  mempty

depsToBS :: Show b => [ b ] -> BSL.ByteString
depsToBS deps =
  let
    depsBS :: String
    depsBS = intercalate "," $ map show deps
 in BSL.fromStrict $ Char8.pack $ depsBS


newtype TestPackage = TestPackage Word
  deriving stock Generic
  deriving newtype (Bounded, Enum, Eq, Ord)

instance Show TestPackage where
  show (TestPackage word) = "package" <> show word
