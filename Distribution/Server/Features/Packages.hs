module Distribution.Server.Features.Packages (
    PackagesFeature(..),
    initPackagesFeature
  ) where

import Distribution.Server.Feature
import Distribution.Server.Features.Core
import Distribution.Server.Packages.Types
import qualified Distribution.Server.Packages.State as State
import qualified Distribution.Server.PackageIndex as PackageIndex

import Happstack.State (query)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.Version
import Distribution.Server.Packages.ModuleForest
import Distribution.Text
import Data.Maybe (catMaybes)
import Control.Monad (guard)
import Data.List (maximumBy, sort, find)
import Data.Ord (comparing)

-- the goal is to have the HTML module import /this/ one, not the other way around
import Distribution.Server.Pages.Package (flatDependencies)

data PackagesFeature = PackagesFeature {
    packageResource :: PackageResource,
    -- necessary information for the representation of a package resource
    packageRender :: PackageId -> IO (Maybe PackageRender)
    -- other informational hooks: perhaps a simplified CondTree so a browser script can dynamically change the package page based on flags
}

data PackageResource = PackageResource

instance HackageFeature PackagesFeature where
    getFeature _ = HackageModule
      { featureName = "packages"
      , resources   = []
      , dumpBackup    = return []
      , restoreBackup = Nothing
      }

initPackagesFeature :: CoreFeature -> IO PackagesFeature
initPackagesFeature _ = return PackagesFeature { packageResource = PackageResource, packageRender = doPackageRender }

-- This should provide the caller enough information to encode the package information
-- in its particular format (text, html, json) with minimal effort on its part.
-- This is why some fields of PackageDescription are preprocessed, and others aren't.
data PackageRender = PackageRender {
    rendAllVersions :: [Version],
    rendDepends :: [[Dependency]],
    rendExecNames :: [String],
    rendLicenseName :: String,
    rendMaintainer :: Maybe String,
    rendCategory :: [String],
    rendRepoHeads :: [(RepoType, String, SourceRepo)],
    rendModules :: Maybe ModuleForest,
    rendHasTarball :: Bool,
    -- rendOther contains other useful fields which are merely strings, possibly empty
    --     for example: description, home page, copyright, author, stability
    -- If PackageRender is the One True Resource Representation, should they
    -- instead be fields of PackageRender?
    rendOther :: PackageDescription
}
doPackageRender :: PackageId -> IO (Maybe PackageRender)
doPackageRender (PackageIdentifier name version) = do
    pkgIndex <- fmap State.packageList $ query State.GetPackagesState
    return $ do
        let infos = PackageIndex.lookupPackageName pkgIndex name
        guard (not . null $ infos)
        info <- if version == Version [] [] then Just $ maximumBy (comparing packageVersion) infos
                                            else find ((==version) . packageVersion) infos
        let genDesc  = pkgDesc info
            flatDesc = flattenPackageDescription genDesc
            desc     = packageDescription genDesc
        return $ PackageRender
          { rendAllVersions = sort $ map packageVersion infos
          , rendDepends   = flatDependencies genDesc
          , rendExecNames = map exeName (executables flatDesc)
          , rendLicenseName = display (license desc) -- maybe make this a bit more human-readable (sans camel case)
          , rendMaintainer  = case maintainer desc of "none" -> Nothing; person -> Just person
          , rendCategory = case category desc of [] -> []; str -> [str] -- TODO: split on commas and whatnot
          , rendRepoHeads = catMaybes (map rendRepo $ sourceRepos desc)
          , rendModules = fmap (moduleForest . exposedModules) (library flatDesc)
          , rendHasTarball = not . null $ pkgTarball info
          , rendOther = desc
          }
  where
    rendRepo r = do
        guard $ repoKind r == RepoHead
        ty <- repoType r
        loc <- repoLocation r
        return (ty, loc, r)

{-
Some quick notes about the kind of data needed in Distribution.Server.Pages.Package:

Package name (from package id): description (synopsis of desc)
prologue - take description from desc and try to parse it in haddock
properties data
    versions: linking to other versions with pdAllVersions pd (partitioned into less than + greater than) with current link bolded
    dependencies: from [[Dependencies]], disjunctive lists
    other standard fields relying on package descriptions: license, copyright, author, maintainer, stability, categories
        optionally: home page, bug trackers, any executables.
        These are all taken from the flattened description, although this only matters for execs and library      
    other hackage-specific tags: currently, upload information from PkgInfo
    server-side build information, currently unused
module data - moduleForest of exposedModules (library genDesc), using base doc URL passed to it - again, this is 
download section data - tarball url and cabal url (does not currently check for tarball existence)
reports section data - just a link
admin section data - likewise
-}
