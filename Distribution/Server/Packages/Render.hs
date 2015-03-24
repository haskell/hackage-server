-- TODO: Review and possibly move elsewhere. This code was part of the
-- RecentPackages (formerly "Check") feature, but that caused some cyclic
-- dependencies.
module Distribution.Server.Packages.Render (
    -- * Package render
    PackageRender(..)
  , doPackageRender

    -- * Utils
  , categorySplit,
  ) where

import Data.Maybe (catMaybes, maybeToList)
import Control.Monad (guard)
import Data.Char (toLower, isSpace)
import qualified Data.Map as Map
import qualified Data.Vector as Vec
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Time.Clock (UTCTime)

-- Cabal
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Package
import Distribution.Text
import Distribution.Version

-- hackage-server
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.ModuleForest
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Types

-- This should provide the caller enough information to encode the package information
-- in its particular format (text, html, json) with minimal effort on its part.
-- This is why some fields of PackageDescription are preprocessed, and others aren't.
data PackageRender = PackageRender {
    rendPkgId        :: PackageIdentifier,
    rendDepends      :: [Dependency],
    rendExecNames    :: [String],
    rendLicenseName  :: String,
    rendMaintainer   :: Maybe String,
    rendCategory     :: [String],
    rendRepoHeads    :: [(RepoType, String, SourceRepo)],
    rendModules      :: Maybe ModuleForest,
    rendHasTarball   :: Bool,
    rendHasChangeLog :: Bool,
    rendUploadInfo   :: (UTCTime, Maybe UserInfo),
    rendUpdateInfo   :: Maybe (Int, UTCTime, Maybe UserInfo),
    rendPkgUri       :: String,
    rendFlags        :: [Flag],
    -- rendOther contains other useful fields which are merely strings, possibly empty
    --     for example: description, home page, copyright, author, stability
    -- If PackageRender is the One True Resource Representation, should they
    -- instead be fields of PackageRender?
    rendOther        :: PackageDescription
} deriving (Show)

doPackageRender :: Users.Users -> PkgInfo -> Bool -> IO PackageRender
doPackageRender users info hasChangeLog = return $ PackageRender
    { rendPkgId        = pkgInfoId info
    , rendDepends      = flatDependencies genDesc
    , rendExecNames    = map exeName (executables flatDesc)
    , rendLicenseName  = display (license desc) -- maybe make this a bit more human-readable
    , rendMaintainer   = case maintainer desc of
                           "None" -> Nothing
                           "none" -> Nothing
                           ""     -> Nothing
                           person -> Just person
    , rendCategory     = case category desc of
                           []  -> []
                           str -> categorySplit str
    , rendRepoHeads    = catMaybes (map rendRepo $ sourceRepos desc)
    , rendModules      = fmap (moduleForest . exposedModules) (library flatDesc)
    , rendHasTarball   = not . Vec.null $ pkgTarballRevisions info
    , rendHasChangeLog = hasChangeLog
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

    rendRepo r = do
        guard $ repoKind r == RepoHead
        ty <- repoType r
        loc <- repoLocation r
        return (ty, loc, r)

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
-- simple summary form. Dependency version ranges within each executable
-- or library are unioned, and the resulting sets are intersected.
--
flatDependencies :: GenericPackageDescription -> [Dependency]
flatDependencies =
      sortOn (\(Dependency pkgname _) -> map toLower (display pkgname))
    . combineDepsBy intersectVersionIntervals
    . concat
    . map (combineDepsBy unionVersionIntervals)
    . targetDeps
  where
    combineDepsBy :: (VersionIntervals -> VersionIntervals -> VersionIntervals)
                  -> [Dependency] -> [Dependency]
    combineDepsBy f =
        map (\(pkgname, ver) -> Dependency pkgname (fromVersionIntervals ver))
      . Map.toList
      . Map.fromListWith f
      . map (\(Dependency pkgname ver) -> (pkgname, toVersionIntervals ver))

    targetDeps :: GenericPackageDescription -> [[Dependency]]
    targetDeps pkg = map condTreeDeps (maybeToList $ condLibrary pkg)
                  ++ map (condTreeDeps . snd) (condExecutables pkg)
 
    condTreeDeps :: CondTree v [Dependency] a -> [Dependency]
    condTreeDeps (CondNode _ ds comps) =
        ds ++ concatMap fromComponent comps
      where
        fromComponent (_, then_part, Nothing) =
          condTreeDeps then_part
        fromComponent (_, then_part, Just else_part) =
          condTreeDeps then_part ++ condTreeDeps else_part

-- Same as @sortBy (comparing f)@, but without recomputing @f@.
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f xs = map snd (sortBy (comparing fst) [(f x, x) | x <- xs])

