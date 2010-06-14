module Distribution.Server.Features.Packages (
    PackagesFeature(..),
    PackagesResource(..),
    initPackagesFeature,
    doPackageRender
  ) where

import Distribution.Server.Feature
import Distribution.Server.Resource
import Distribution.Server.Hook
import Distribution.Server.Features.Core
import Distribution.Server.Packages.Types
import qualified Distribution.Server.Users.State as State
import qualified Distribution.Server.Packages.State as State
import qualified Distribution.Server.PackageIndex as PackageIndex
import Distribution.Server.PackageIndex (PackageIndex)
import qualified Distribution.Server.Cache as Cache
import qualified Distribution.Server.ResourceTypes as Resource

import Happstack.Server
import Happstack.State (query)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.Version
import Distribution.Server.Packages.ModuleForest
import Distribution.Text
import Data.Maybe (catMaybes, fromJust)
import Control.Monad (guard)
import Data.Time.Clock (getCurrentTime)
import Data.List (maximumBy, sort, sortBy, find)
import Data.Ord (comparing)
import qualified Network.URI as URI

-- the goal is to have the HTML modules import /this/ one, not the other way around
import Distribution.Server.Pages.Package (flatDependencies)
import qualified Distribution.Server.Pages.Recent as Pages

data PackagesFeature = PackagesFeature {
    packagesResource :: PackagesResource,
    -- recent caches. in lieu of a log feature
    cacheRecent :: Cache.GenCache (Response, Response), -- (html, rss)
    -- necessary information for the representation of a package resource
    packageRender :: PackageId -> IO (Maybe PackageRender)
    -- other informational hooks: perhaps a simplified CondTree so a browser script can dynamically change the package page based on flags
}

data PackagesResource = PackagesResource {
    -- replace with log feature
    packagesRecent :: Resource
}

instance HackageFeature PackagesFeature where
    getFeature pkgsf = HackageModule
      { featureName = "packages"
      , resources   = map ($packagesResource pkgsf) [packagesRecent]
      , dumpBackup    = return []
      , restoreBackup = Nothing
      }

initPackagesFeature :: CoreFeature -> IO PackagesFeature
initPackagesFeature core = do
    recents <- Cache.newCacheable
    registerHook (packageIndexChange core) $ do
        state <- query State.GetPackagesState
        users <- query State.GetUserDb
        now   <- getCurrentTime
        let recentChanges = reverse $ sortBy (comparing pkgUploadTime) (PackageIndex.allPackages . State.packageList $ state)
        Cache.putCache recents (toResponse $ Resource.XHtml $ Pages.recentPage users recentChanges,
                                toResponse $ Pages.recentFeed users (fromJust $ URI.uriAuthority =<< URI.parseURI "http://hackage.haskell.org") now recentChanges)
    return PackagesFeature
      { packagesResource = PackagesResource
          { packagesRecent = (resourceAt "/recent") { resourceGet = [("html", Cache.respondCache recents fst), ("rss", Cache.respondCache recents snd)] }
          }
      , cacheRecent   = recents
      , packageRender = \pkg -> do
            state <- query State.GetPackagesState
            return $ doPackageRender (State.packageList state) pkg
      }

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
doPackageRender :: PackageIndex PkgInfo -> PackageId -> Maybe PackageRender
doPackageRender pkgIndex (PackageIdentifier name version) = do
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
