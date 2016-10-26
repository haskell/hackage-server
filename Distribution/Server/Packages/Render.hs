-- TODO: Review and possibly move elsewhere. This code was part of the
-- RecentPackages (formerly "Check") feature, but that caused some cyclic
-- dependencies.
{-# LANGUAGE FlexibleInstances, DeriveGeneric, OverloadedStrings, RankNTypes, TypeSynonymInstances #-}

module Distribution.Server.Packages.Render (
    -- * Package render
    PackageRender(..)
  , DependencyTree
  , IsBuildable (..)
  , doPackageRender

    -- * Utils
  , categorySplit

  ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (guard, mzero)
import Control.Arrow ((&&&), second)
import           Data.Aeson ((.=),(.:),(.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Char (toLower, isSpace)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust, mapMaybe, maybeToList)
import qualified Data.Vector as Vec
import Data.Ord (comparing)
import Data.List (sortBy, intercalate)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics
import System.FilePath.Posix ((</>), (<.>))
import Text.Read             (readMaybe)

-- Cabal
import Distribution.Compiler (CompilerFlavor)
import Distribution.License
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Package
import Distribution.Text
import Distribution.Version
import Distribution.ModuleName as ModuleName

-- hackage-server
import Distribution.Server.Framework.CacheControl (ETag)
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.ModuleForest
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Types
import Distribution.System (Arch, OS(..))
import qualified Data.TarIndex as TarIndex
import Data.TarIndex (TarIndex, TarEntryOffset)

import Language.Haskell.Extension (Extension, KnownExtension, Language)

-- This should provide the caller enough information to encode the package information
-- in its particular format (text, html, json) with minimal effort on its part.
-- This is why some fields of PackageDescription are preprocessed, and others aren't.
data PackageRender = PackageRender {
    rendPkgId        :: PackageIdentifier,
    rendDepends      :: [Dependency],
    rendExecNames    :: [String],
    rendLibraryDeps  :: Maybe DependencyTree,
    rendExecutableDeps :: [(String, DependencyTree)],
    rendLicenseName  :: String,
    rendLicenseFiles :: [FilePath],
    rendMaintainer   :: Maybe String,
    rendCategory     :: [String],
    rendRepoHeads    :: [(RepoType, String, SourceRepo)],
    rendModules      :: Maybe TarIndex -> Maybe ModuleForest,
    rendHasTarball   :: Bool,
    rendChangeLog    :: Maybe (FilePath, ETag, TarEntryOffset, FilePath),
    rendReadme       :: Maybe (FilePath, ETag, TarEntryOffset, FilePath),
    rendUploadInfo   :: (UTCTime, Maybe UserInfo),
    rendUpdateInfo   :: Maybe (Int, UTCTime, Maybe UserInfo),
    rendPkgUri       :: String,
    rendFlags        :: [Flag],
    -- rendOther contains other useful fields which are merely strings, possibly empty
    --     for example: description, home page, copyright, author, stability
    -- If PackageRender is the One True Resource Representation, should they
    -- instead be fields of PackageRender?
    rendOther        :: PackageDescription
}

instance A.ToJSON PackageRender where
  toJSON p = A.object [
      "pkgId"          .= A.toJSON (rendPkgId p)
    , "depends"        .= map A.toJSON (rendDepends p)
    , "execNames"      .= rendExecNames p
    , "libraryDeps"    .= fmap A.toJSON (rendLibraryDeps p)
    , "executableDeps" .= map (\(str,dep) -> [A.String (T.pack str), A.toJSON dep])
                          (rendExecutableDeps p)
    , "licenseName"    .= rendLicenseName p
    , "licenseFiles"   .= rendLicenseFiles p
    , "maintainer"     .= rendMaintainer p
    , "category"       .= rendCategory p
    , "repoHeads"      .= map A.toJSON (rendRepoHeads p)
    , "modules"        .= ((rendModules p) Nothing) -- TODO probably wrong? Can't serialize a function anyway
    , "hasTarball"     .= rendHasTarball p
    , "changeLog"      .= rendChangeLog p
    , "readme"         .= rendReadme p
    , "uploadInfo"     .= rendUploadInfo p
    , "updateInfo"     .= rendUpdateInfo p
    , "pkgUri"         .= rendPkgUri p
    , "flags"          .= map A.toJSON (rendFlags p)
    , "other"          .= A.toJSON (rendOther p)
    ]

doPackageRender :: Users.Users -> PkgInfo -> PackageRender
doPackageRender users info = PackageRender
    { rendPkgId        = pkgInfoId info
    , rendDepends      = flatDependencies genDesc
    , rendExecNames    = map exeName (executables flatDesc)
    , rendLibraryDeps  = depTree libBuildInfo `fmap` condLibrary genDesc
    , rendExecutableDeps = second (depTree buildInfo) `map` condExecutables genDesc
    , rendLicenseName  = display (license desc) -- maybe make this a bit more human-readable
    , rendLicenseFiles = licenseFiles desc
    , rendMaintainer   = case maintainer desc of
                           "None" -> Nothing
                           "none" -> Nothing
                           ""     -> Nothing
                           person -> Just person
    , rendCategory     = case category desc of
                           []  -> []
                           str -> categorySplit str
    , rendRepoHeads    = mapMaybe rendRepo (sourceRepos desc)
    , rendModules      = \docindex ->
                             fmap (moduleForest
                           . map (\m -> (m, moduleHasDocs docindex m))
                           . exposedModules)
                          (library flatDesc)
    , rendHasTarball   = not . Vec.null $ pkgTarballRevisions info
    , rendChangeLog    = Nothing -- populated later
    , rendReadme       = Nothing -- populated later
    , rendUploadInfo   = let (utime, uid) = pkgOriginalUploadInfo info
                         in (utime, Users.lookupUserId uid users)
    , rendUpdateInfo   = let maxrevision  = Vec.length (pkgMetadataRevisions info) - 1
                             (utime, uid) = pkgLatestUploadInfo info
                             uinfo        = Users.lookupUserId uid users
                         in if maxrevision > 0
                              then Just (maxrevision, utime, uinfo)
                              else Nothing
    , rendPkgUri       = pkgUri
    , rendFlags        = genPackageFlags genDesc
    , rendOther        = desc
    }
  where
    genDesc  = pkgDesc info
    flatDesc = flattenPackageDescription genDesc
    desc     = packageDescription genDesc
    pkgUri   = "/package/" ++ display (pkgInfoId info)

    depTree :: (a -> BuildInfo) -> CondTree ConfVar [Dependency] a -> DependencyTree
    depTree getBuildInfo = mapTreeData isBuildable . mapTreeConstrs simplifyDeps
      where
        simplifyDeps = sortDeps . combineDepsBy intersectVersionIntervals
        isBuildable ctData = if buildable $ getBuildInfo ctData
                               then Buildable
                               else NotBuildable

    moduleHasDocs :: Maybe TarIndex -> ModuleName -> Bool
    moduleHasDocs Nothing       = const False
    moduleHasDocs (Just doctar) = isJust . TarIndex.lookup doctar
                                         . moduleDocTarPath (packageId genDesc)

    moduleDocTarPath :: PackageId -> ModuleName -> FilePath
    moduleDocTarPath pkgid modname =
      display pkgid ++ "-docs" </>
      intercalate "-" (ModuleName.components modname) <.> "html"

    rendRepo r = do
        guard $ repoKind r == RepoHead
        ty <- repoType r
        loc <- repoLocation r
        return (ty, loc, r)

type DependencyTree = CondTree ConfVar [Dependency] IsBuildable

data IsBuildable = Buildable
                 | NotBuildable
                   deriving (Eq, Show, Read, Generic)

{-------------------------------------------------------------------------------
  Util
-------------------------------------------------------------------------------}

categorySplit :: String -> [String]
categorySplit xs | all isSpace xs = []
categorySplit xs = map (dropWhile isSpace) $ splitOn ',' xs
  where
    splitOn x ys = front : case back of
                           [] -> []
                           (_:ys') -> splitOn x ys'
      where (front, back) = break (== x) ys

-----------------------------------------------------------------------
--
-- Flatten the dependencies of a GenericPackageDescription into a
-- simple summary form. Library and executable dependency ranges
-- are combined using intersection, except for dependencies within
-- if and else branches, which are unioned together. Skip
-- dependencies introduced by manual flags.
--
flatDependencies :: GenericPackageDescription -> [Dependency]
flatDependencies pkg =
      sortOn (\(Dependency pkgname _) -> map toLower (display pkgname)) pkgDeps
  where
    pkgDeps :: [Dependency]
    pkgDeps = fromMap $ Map.unionsWith intersectVersions $
                  map condTreeDeps (maybeToList $ condLibrary pkg)
               ++ map (condTreeDeps . snd) (condExecutables pkg)
      where
        fromMap = map fromPair . Map.toList
        fromPair (pkgname, Versions _ ver) =
            Dependency pkgname $ fromVersionIntervals ver

    manualFlags :: FlagAssignment
    manualFlags = map assignment . filter flagManual $ genPackageFlags pkg
        where assignment = flagName &&& flagDefault

    condTreeDeps :: CondTree ConfVar [Dependency] a -> PackageVersions
    condTreeDeps (CondNode _ ds comps) =
        Map.unionsWith intersectVersions $
          toMap ds : map fromComponent comps
      where
        fromComponent (cond, then_part, else_part) =
            let thenDeps = condTreeDeps then_part
                elseDeps = maybe Map.empty condTreeDeps else_part
            in case evalCondition manualFlags cond of
                 Just True -> thenDeps
                 Just False -> elseDeps
                 Nothing -> unionPackageVersions thenDeps elseDeps

        toMap = Map.fromListWith intersectVersions . map toPair
        toPair (Dependency pkgname ver) =
            (pkgname, Versions All $ toVersionIntervals ver)

-- Note that 'unionPackageVersions Map.empty' is not identity.
unionPackageVersions :: PackageVersions -> PackageVersions -> PackageVersions
unionPackageVersions ds1 ds2 = Map.unionWith unionVersions
                               (Map.union ds1 defaults)
                               (Map.union ds2 defaults)
  where
    defaults = Map.map (const notSpecified) $ Map.union ds1 ds2
    notSpecified = Versions Some $ toVersionIntervals noVersion

-- | Version intervals for a dependency that also indicate whether the
-- dependency has been specified on all branches. For example, package x's
-- version intervals use 'All' while package y's version intervals use
-- 'Some':
--
-- > if flag(f)
-- >   build-depends: x < 1, y < 1
-- > else
-- >   build-depends: x >= 1
--
-- This distinction affects the intersection of intervals.
data Versions = Versions Branches VersionIntervals

data Branches = All | Some deriving Eq

type PackageVersions = Map.Map PackageName Versions

unionVersions :: Versions -> Versions -> Versions
unionVersions (Versions b1 v1) (Versions b2 v2) =
    let b3 = if b1 == Some || b2 == Some
                then Some
                else All
    in Versions b3 $ unionVersionIntervals v1 v2

intersectVersions :: Versions -> Versions -> Versions
intersectVersions (Versions Some v1) (Versions Some v2) =
    Versions Some $ unionVersionIntervals v1 v2
intersectVersions (Versions Some _) v@(Versions All _) = v
intersectVersions v@(Versions All _) (Versions Some _) = v
intersectVersions (Versions All v1) (Versions All v2) =
    Versions All $ intersectVersionIntervals v1 v2

sortDeps :: [Dependency] -> [Dependency]
sortDeps = sortOn $ \(Dependency pkgname _) -> map toLower (display pkgname)

combineDepsBy :: (VersionIntervals -> VersionIntervals -> VersionIntervals)
              -> [Dependency] -> [Dependency]
combineDepsBy f =
    map (\(pkgname, ver) -> Dependency pkgname (fromVersionIntervals ver))
  . Map.toList
  . Map.fromListWith f
  . map (\(Dependency pkgname ver) -> (pkgname, toVersionIntervals ver))

-- | Evaluate a 'Condition' with a partial 'FlagAssignment', returning
-- | 'Nothing' if the result depends on additional variables.
evalCondition :: FlagAssignment -> Condition ConfVar -> Maybe Bool
evalCondition flags cond =
    let eval = evalCondition flags
    in case cond of
         Var (Flag f) -> lookup f flags
         Var _ -> Nothing
         Lit b -> Just b
         CNot c -> not `fmap` eval c
         COr c1 c2 -> eval $ CNot (CNot c1 `CAnd` CNot c2)
         CAnd c1 c2 -> case (eval c1, eval c2) of
                         (Just False, _) -> Just False
                         (_, Just False) -> Just False
                         (Just True, Just True) -> Just True
                         _ -> Nothing

-- Same as @sortBy (comparing f)@, but without recomputing @f@.
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f xs = map snd (sortBy (comparing fst) [(f x, x) | x <- xs])

instance A.FromJSON PackageIdentifier
instance A.ToJSON PackageIdentifier

instance A.FromJSON Dependency
instance A.ToJSON Dependency

instance A.ToJSON ModuleTree
instance A.FromJSON ModuleTree

instance A.ToJSON SourceRepo
instance A.FromJSON SourceRepo

instance A.ToJSON RepoType
instance A.FromJSON RepoType

instance A.ToJSON RepoKind
instance A.FromJSON RepoKind

instance A.ToJSON Flag
instance A.FromJSON Flag

instance A.ToJSON PackageName
instance A.FromJSON PackageName

instance A.ToJSON BuildInfo
instance A.FromJSON BuildInfo

instance A.ToJSON Arch
instance A.FromJSON Arch

instance A.ToJSON FlagName
instance A.FromJSON FlagName

instance A.ToJSON PackageDescription
instance A.FromJSON PackageDescription

instance A.ToJSONKey PackageName
instance A.FromJSONKey PackageName

instance A.ToJSON CompilerFlavor
instance A.FromJSON CompilerFlavor

instance A.ToJSON TestSuite
instance A.FromJSON TestSuite

instance A.ToJSON BenchmarkInterface
instance A.FromJSON BenchmarkInterface

instance A.ToJSON TestSuiteInterface
instance A.FromJSON TestSuiteInterface

instance A.ToJSON SetupBuildInfo
instance A.FromJSON SetupBuildInfo

instance A.ToJSON OS
instance A.FromJSON OS

instance A.ToJSON DependencyTree
instance A.FromJSON DependencyTree

instance A.ToJSON IsBuildable
instance A.FromJSON IsBuildable

instance A.ToJSON a => A.ToJSON (Condition a)
instance A.FromJSON a => A.FromJSON (Condition a)

instance A.ToJSON ConfVar
instance A.FromJSON ConfVar

instance A.FromJSON License
instance A.ToJSON License

instance A.FromJSON BuildType
instance A.ToJSON BuildType

instance A.FromJSON Extension
instance A.ToJSON Extension

instance A.FromJSON KnownExtension
instance A.ToJSON KnownExtension

instance A.FromJSON Language
instance A.ToJSON Language

instance A.FromJSON Benchmark
instance A.ToJSON Benchmark

instance A.FromJSON BenchmarkType
instance A.ToJSON BenchmarkType

instance A.FromJSON Library
instance A.ToJSON Library

instance A.FromJSON Executable
instance A.ToJSON Executable

instance A.FromJSON VersionRange
instance A.ToJSON VersionRange

instance A.FromJSON ModuleName
instance A.ToJSON ModuleName

instance A.FromJSON ModuleRenaming
instance A.ToJSON ModuleRenaming

instance A.FromJSON ModuleReexport
instance A.ToJSON ModuleReexport

instance A.FromJSON TestType
instance A.ToJSON TestType
