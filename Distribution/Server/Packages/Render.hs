-- TODO: Review and possibly move elsewhere. This code was part of the
-- RecentPackages (formerly "Check") feature, but that caused some cyclic
-- dependencies.
module Distribution.Server.Packages.Render (
    -- * Package render
    PackageRender(..)
  , DependencyTree
  , IsBuildable (..)
  , doPackageRender
  , ModSigIndex(..)

    -- * Utils
  , categorySplit,
  ) where

import Prelude ()
import Distribution.Server.Prelude hiding (All)

import Control.Arrow ((&&&), (***))
import Data.Char (toLower, isSpace)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Data.List (intercalate)
import Data.Time.Clock (UTCTime)
import System.FilePath.Posix ((</>), (<.>))

-- Cabal
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Package
import Distribution.Text
import Distribution.Pretty (prettyShow)
import Distribution.Version
import Distribution.ModuleName as ModuleName
import Distribution.Types.CondTree
import Distribution.Types.UnqualComponentName

-- hackage-server
import Distribution.Server.Framework.CacheControl (ETag)
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.ModuleForest
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Types
import Distribution.Utils.ShortText (fromShortText)
import qualified Data.TarIndex as TarIndex
import Data.TarIndex (TarIndex, TarEntryOffset)

data ModSigIndex = ModSigIndex {
        modIndex :: ModuleForest,
        sigIndex :: ModuleForest
    }

-- This should provide the caller enough information to encode the package information
-- in its particular format (text, html, json) with minimal effort on its part.
-- This is why some fields of PackageDescription are preprocessed, and others aren't.
data PackageRender = PackageRender {
    rendPkgId        :: PackageIdentifier,
    rendDepends      :: [Dependency],
    rendExecNames    :: [String],
    rendLibraryDeps  :: Maybe DependencyTree,
    rendSublibraryDeps :: [(String, DependencyTree)],
    rendExecutableDeps :: [(String, DependencyTree)],
    rendLicenseName  :: String,
    rendLicenseFiles :: [FilePath],
    rendMaintainer   :: Maybe String,
    rendCategory     :: [String],
    rendRepoHeads    :: [(RepoType, String, SourceRepo)],
    -- | The optional 'TarIndex' is of the documentation tarball; we use this
    -- to test if a module actually has a corresponding documentation HTML
    -- file we can link to.  If no 'TarIndex' is provided, it is assumed
    -- all links are dead.
    rendModules      :: Maybe TarIndex -> Maybe ModSigIndex,
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

doPackageRender :: Users.Users -> PkgInfo -> PackageRender
doPackageRender users info = PackageRender
    { rendPkgId        = pkgInfoId info
    , rendDepends      = flatDependencies genDesc
    , rendExecNames    = map (unUnqualComponentName . exeName) (executables flatDesc)
    , rendLibraryDeps  = depTree libBuildInfo `fmap` condLibrary genDesc
    , rendExecutableDeps = (unUnqualComponentName *** depTree buildInfo)
                                `map` condExecutables genDesc
    , rendSublibraryDeps = (unUnqualComponentName *** depTree libBuildInfo)
                                `map` condSubLibraries genDesc
    , rendLicenseName  = prettyShow (license desc) -- maybe make this a bit more human-readable
    , rendLicenseFiles = licenseFiles desc
    , rendMaintainer   = case fromShortText $ maintainer desc of
                           "None" -> Nothing
                           "none" -> Nothing
                           ""     -> Nothing
                           person -> Just person
    , rendCategory     = case fromShortText $ category desc of
                           []  -> []
                           str -> categorySplit str
    , rendRepoHeads    = catMaybes (map rendRepo $ sourceRepos desc)
    , rendModules      = renderModules
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

    renderModules docindex
      | Just lib <- library flatDesc
      = let mod_ix = mkForest $ exposedModules lib
                           -- Assumes that there is an HTML per reexport
                           ++ map moduleReexportName (reexportedModules lib)
            sig_ix = mkForest $ signatures lib
            mkForest = moduleForest . map (\m -> (m, moduleHasDocs docindex m))
        in Just (ModSigIndex { modIndex = mod_ix, sigIndex = sig_ix })
      | otherwise
      = Nothing

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
                   deriving (Eq, Show)

{-------------------------------------------------------------------------------
  Util
-------------------------------------------------------------------------------}

categorySplit :: String -> [String]
categorySplit xs | all isSpace xs = []
categorySplit xs = if last res == "" then init res else res
  where
    res = map (dropWhile isSpace) $ splitOn ',' xs
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
      sortOn (\(Dependency pkgname _ _) -> map toLower (display pkgname)) pkgDeps
  where
    pkgDeps :: [Dependency]
    pkgDeps = fromMap $ Map.filterWithKey notSubLib $ Map.unionsWith intersectVersions $
                  map condTreeDeps (maybeToList $ condLibrary pkg)
               ++ map (condTreeDeps . snd) (condSubLibraries pkg)
               ++ map (condTreeDeps . snd) (condExecutables pkg)
      where
        fromMap = map fromPair . Map.toList
        fromPair (pkgname, Versions _ ver) =
            Dependency pkgname (fromVersionIntervals ver) Set.empty -- XXX: ok?

        notSubLib pn _ = packageNameToUnqualComponentName pn `Set.notMember` sublibs
        sublibs = Set.fromList $ map fst (condSubLibraries pkg)

    manualFlags :: [(FlagName,Bool)] -- FlagAssignment
    manualFlags = map assignment . filter flagManual $ genPackageFlags pkg
        where assignment = flagName &&& flagDefault

    condTreeDeps :: CondTree ConfVar [Dependency] a -> PackageVersions
    condTreeDeps (CondNode _ ds comps) =
        Map.unionsWith intersectVersions $
          toMap ds : map fromComponent comps
      where
        fromComponent (CondBranch cond then_part else_part) =
            let thenDeps = condTreeDeps then_part
                elseDeps = maybe Map.empty condTreeDeps else_part
            in case evalCondition manualFlags cond of
                 Just True -> thenDeps
                 Just False -> elseDeps
                 Nothing -> unionPackageVersions thenDeps elseDeps

        toMap = Map.fromListWith intersectVersions . map toPair
        toPair (Dependency pkgname ver _) =
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
sortDeps = sortOn $ \(Dependency pkgname _ _) -> map toLower (display pkgname)

combineDepsBy :: (VersionIntervals -> VersionIntervals -> VersionIntervals)
              -> [Dependency] -> [Dependency]
combineDepsBy f =
    map (\(pkgname, ver) -> Dependency pkgname (fromVersionIntervals ver) Set.empty) -- XXX: ok?
  . Map.toList
  . Map.fromListWith f
  . map (\(Dependency pkgname ver _) -> (pkgname, toVersionIntervals ver))

-- | Evaluate a 'Condition' with a partial 'FlagAssignment', returning
-- | 'Nothing' if the result depends on additional variables.
evalCondition :: [(FlagName,Bool)] -> Condition ConfVar -> Maybe Bool
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
