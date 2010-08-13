module Distribution.Server.Features.Core (
    CoreFeature(..),
    CoreResource(..),
    initCoreFeature,
    basicPackageSection,

    withPackageId,
    withPackageName,
    withPackage,
    withPackagePath,
    withPackageAll,
    withPackageAllPath,
    withPackageVersion,
    withPackageVersionPath,
    withPackageTarball,

    packageExists,
    packageIdExists,
    doDeletePackage,
    doAddPackage,
    doMergePackage
  ) where

--import Distribution.Server.Users.Resource (makeGroupResources)
import qualified Distribution.Server.Cache as Cache
import Distribution.Server.Packages.PackageBackup
import Distribution.Server.Users.UserBackup
import Distribution.Server.Feature
import Distribution.Server.Resource
import Distribution.Server.Types
import Distribution.Server.Hook
import Distribution.Server.Error
import Distribution.Server.Backup.Export
import Distribution.Server.Instances ()

import Distribution.Server.Packages.Types
import Distribution.Server.Packages.State
import Distribution.Server.Users.State
import qualified Distribution.Server.Packages.Index as Packages.Index
import qualified Distribution.Server.Pages.Index as Pages
import qualified Codec.Compression.GZip as GZip
import qualified Distribution.Server.ResourceTypes as Resource
import qualified Distribution.Server.PackageIndex as PackageIndex
import qualified Distribution.Server.Util.BlobStorage as BlobStorage
import Distribution.Server.Util.BlobStorage (BlobStorage)

import Control.Monad (guard, mzero, when)
import Control.Monad.Trans (liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (mconcat)
import Data.Function (fix)
import Happstack.Server
import Happstack.State (update, query)
import Text.XHtml.Strict (Html, toHtml, unordList, h3, (<<), anchor, href, (!))
import Data.Ord (comparing)
import Data.List (sortBy, find)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)

import Distribution.Text (display)
import Distribution.Package
import Distribution.Version (Version(..))


data CoreFeature = CoreFeature {
    coreResource :: CoreResource,

    cacheIndexTarball :: Cache.Cache ByteString,
    -- FIXME: this is HTML and doesn't belong here. Instead, the HTML feature
    -- should sign up for its own hooks
    cachePackagesPage :: Cache.Cache Response,
    -- other files to put in the index tarball like preferred-versions
    indexExtras :: Cache.Cache (Map String ByteString),

    -- Updating top-level packages
    -- This is run after a package is added
    packageAddHook    :: Hook (PkgInfo -> IO ()),
    -- This is run after a package is removed (but the PkgInfo is retained just for the hook)
    packageRemoveHook :: Hook (PkgInfo -> IO ()),
    -- This is run after a package is changed in some way (essentially added, then removed)
    packageChangeHook :: Hook (PkgInfo -> PkgInfo -> IO ()),
    -- This should be run whenever the index tarball needs to be updated, such as when
    -- indexExtras is updated, or when a package is added/removed/changed.
    -- FIXME: it's also conflated with updating the HTML index page, which should be
    -- regulated by the HTML feature signing up for other hooks
    packageIndexChange :: Hook (IO ()),
    -- A package is added where no package by that name existed previously.
    newPackageHook :: Hook (PkgInfo -> IO ()),
    -- A package is removed such that no more versions of that package exists.
    noPackageHook :: Hook (PkgInfo -> IO ()),
    -- For download counters
    tarballDownload :: Hook (PackageId -> IO ())
}

data CoreResource = CoreResource {
    coreIndexPage    :: Resource,
    coreIndexTarball :: Resource,
    corePackagesPage :: Resource,
    corePackagePage  :: Resource,
    corePackageRedirect :: Resource,
    coreCabalFile    :: Resource,
    corePackageTarball :: Resource,

    indexTarballUri   :: String,
    indexPackageUri   :: String -> String,
    corePackageUri  :: String -> PackageId -> String,
    corePackageName :: String -> PackageName -> String,
    coreCabalUri   :: PackageId -> String,
    coreTarballUri :: PackageId -> String
}

instance HackageFeature CoreFeature where
    getFeature core = HackageModule
      { featureName = "core"
      , resources   = map ($coreResource core)
          [ coreIndexPage, coreIndexTarball, corePackagesPage, corePackagePage
          , corePackageRedirect, corePackageTarball, coreCabalFile ]
      , dumpBackup = Just $ \store -> do
            users    <- query GetUserDb
            packages <- query GetPackagesState
            admins   <- query GetHackageAdmins
            packageEntries <- readExportBlobs store $ indexToAllVersions packages
            return $ packageEntries ++ [csvToBackup ["users.csv"] $ usersToCSV users, csvToBackup ["admins.csv"] $ groupToCSV admins]
      , restoreBackup = Just $ \store -> mconcat [userBackup, packagesBackup store, groupBackup ["admins.csv"] ReplaceHackageAdmins]
      }
    initHooks core = [runHook (packageIndexChange core)]

initCoreFeature :: Config -> IO CoreFeature
initCoreFeature config = do
    -- Caches
    thePackages <- Cache.newCacheable $ toResponse ()
    indexTar <- Cache.newCacheable BS.empty
    extraMap <- Cache.newCacheable Map.empty

    downHook <- newHook
    addHook  <- newHook
    removeHook <- newHook
    changeHook <- newHook
    indexHook <- newHook
    newPkgHook <- newHook
    noPkgHook <- newHook
    registerHook indexHook $ computeCache thePackages indexTar

    return CoreFeature
      { coreResource = fix $ \r -> CoreResource {
            -- the rudimentary HTML resources are for when we don't want an additional HTML feature
            coreIndexPage = (resourceAt "/.:format") { resourceGet = [("html", indexPage $ serverStaticDir config)] }
          , coreIndexTarball = (resourceAt "/packages/index.tar.gz") { resourceGet = [("tarball", Cache.respondCache indexTar Resource.IndexTarball)] }
          , corePackagesPage = (resourceAt "/packages/.:format") { resourceGet = [("html", Cache.respondCache thePackages id)] }
          , corePackagePage = (resourceAt "/package/:package.:format") { resourceGet = [("html", basicPackagePage r)] }
          , corePackageRedirect = (resourceAt "/package/") { resourceGet = [("", \_ -> seeOther "/packages/" $ toResponse ())] }
          , corePackageTarball = (resourceAt "/package/:package/:tarball.tar.gz") { resourceGet = [("tarball", servePackageTarball downHook $ serverStore config)] }
          , coreCabalFile  = (resourceAt "/package/:package/:cabal.cabal") { resourceGet = [("cabal", serveCabalFile)] }

          , indexTarballUri = renderResource (coreIndexTarball r) []
          , indexPackageUri = \format -> renderResource (corePackagesPage r) [format]
          , corePackageUri  = \format pkgid -> renderResource (corePackagePage r) [display pkgid, format]
          , corePackageName = \format pkgname -> renderResource (corePackagePage r) [display pkgname, format]
          , coreCabalUri   = \pkgid -> renderResource (coreCabalFile r) [display pkgid, display (packageName pkgid)]
          , coreTarballUri = \pkgid -> renderResource (corePackageTarball r) [display pkgid, display pkgid]
          }
      , cacheIndexTarball  = indexTar
      , cachePackagesPage  = thePackages
      , indexExtras = extraMap
      , packageAddHook = addHook
      , packageRemoveHook = removeHook
      , packageChangeHook = changeHook
      , packageIndexChange = indexHook
      , newPackageHook = newPkgHook
      , noPackageHook = noPkgHook
      , tarballDownload = downHook
    }
  where
    indexPage staticDir _ = serveFile (const $ return "text/html") (staticDir ++ "/hackage.html")
    computeCache thePackages indexTar = do
        users <- query GetUserDb
        index <- fmap packageList $ query GetPackagesState
        -- TODO: instead of using a complicated pages module, make a basicPackageIndex function
        Cache.putCache thePackages (toResponse $ Resource.XHtml $ Pages.packageIndex index)
        Cache.putCache indexTar (GZip.compress $ Packages.Index.write users index)

-- Should probably look more like an Apache index page (Name / Last modified / Size / Content-type)
basicPackagePage :: CoreResource -> DynamicPath -> ServerPart Response
basicPackagePage r dpath = textResponse $ withPackagePath dpath $ \_ pkgs ->
  returnOk $ toResponse $ Resource.XHtml $ showAllP $ sortBy (flip $ comparing packageVersion) pkgs
  where
    showAllP :: [PkgInfo] -> Html
    showAllP pkgs = toHtml [
        h3 << "Downloads",
        unordList $ map (basicPackageSection (coreCabalUri r) (coreTarballUri r)) pkgs
     ]

basicPackageSection :: (PackageId -> String) -> (PackageId -> String) -> PkgInfo -> [Html]
basicPackageSection cabalUrl tarUrl pkgInfo = let pkgId = packageId pkgInfo; pkgStr = display pkgId in [
    toHtml pkgStr,
    unordList $ [
        [anchor ! [href (cabalUrl pkgId)] << "Package description",
         toHtml " (included in the package)"],
        case pkgTarball pkgInfo of
            [] -> [toHtml "Package not available"];
            _ ->  [anchor ! [href (tarUrl pkgId)] << (pkgStr ++ ".tar.gz"),
                   toHtml " (Cabal source package)"]
    ]
 ]

------------------------------------------------------------------------------
packageExists, packageIdExists :: (Package pkg, Package pkg') => PackageIndex pkg -> pkg' -> Bool
packageExists   state pkg = not . null $ PackageIndex.lookupPackageName state (packageName pkg)
packageIdExists state pkg = maybe False (const True) $ PackageIndex.lookupPackageId state (packageId pkg)

withPackageId :: DynamicPath -> (PackageId -> ServerPart a) -> ServerPart a
withPackageId dpath = require (return $ lookup "package" dpath >>= fromReqURI)

withPackageName :: DynamicPath -> (PackageName -> ServerPart a) -> ServerPart a
withPackageName dpath = require (return $ lookup "package" dpath >>= fromReqURI)

packageError :: [Message] -> MServerPart a
packageError = returnError 404 "Package not found"

withPackage :: PackageId -> (PkgInfo -> [PkgInfo] -> MServerPart a) -> MServerPart a
withPackage pkgid func = query GetPackagesState >>= \state ->
    case PackageIndex.lookupPackageName (packageList state) (packageName pkgid) of
        []   ->  packageError [MText "No such package in package index"]
        pkgs  | pkgVersion pkgid == Version [] [] ->
            -- pkgs is sorted by version number and non-empty
            func (last pkgs) pkgs
        pkgs -> case find ((== packageVersion pkgid) . packageVersion) pkgs of
            Nothing  -> packageError [MText $ "No such package version for " ++ display (packageName pkgid)]
            Just pkg -> func pkg pkgs

withPackagePath :: DynamicPath -> (PkgInfo -> [PkgInfo] -> MServerPart a) -> MServerPart a
withPackagePath dpath func = withPackageId dpath $ \pkgid -> withPackage pkgid func

withPackageAll :: PackageName -> ([PkgInfo] -> MServerPart a) -> MServerPart a
withPackageAll pkgname func = query GetPackagesState >>= \state ->
    case PackageIndex.lookupPackageName (packageList state) pkgname of
        []   -> packageError [MText "No such package in package index"]
        pkgs -> func pkgs

withPackageAllPath :: DynamicPath -> (PackageName -> [PkgInfo] -> MServerPart a) -> MServerPart a
withPackageAllPath dpath func = withPackageName dpath $ \pkgname -> withPackageAll pkgname (func pkgname)

withPackageVersion :: PackageId -> (PkgInfo -> MServerPart a) -> MServerPart a
withPackageVersion pkgid func = do
    guard (packageVersion pkgid /= Version [] [])
    query GetPackagesState >>= \state -> case PackageIndex.lookupPackageId (packageList state) pkgid of
        Nothing -> packageError [MText $ "No such package version for " ++ display (packageName pkgid)]
        Just pkg -> func pkg

withPackageVersionPath :: DynamicPath -> (PkgInfo -> MServerPart a) -> MServerPart a
withPackageVersionPath dpath func = withPackageId dpath $ \pkgid -> withPackageVersion pkgid func

withPackageTarball :: DynamicPath -> (PackageId -> ServerPart a) -> ServerPart a
withPackageTarball dpath func = withPackageId dpath $ \(PackageIdentifier name version) ->
    require (return $ lookup "tarball" dpath >>= fromReqURI) $ \pkgid@(PackageIdentifier name' version') -> do
    -- rules:
    -- * the package name and tarball name must be the same
    -- * the tarball must specify a version
    -- * the package must either have no version or the same version as the tarball
    guard $ name == name' && version' /= Version [] [] && (version == version' || version == Version [] [])
    func pkgid

------------------------------------------------------------------------
-- result: tarball or not-found error
servePackageTarball :: Hook (PackageId -> IO ()) -> BlobStorage -> DynamicPath -> MServerPart Response
servePackageTarball hook store dpath = withPackageTarball dpath $ \pkgid ->
                                       withPackageVersion pkgid $ \pkg ->
    case pkgTarball pkg of
        [] -> returnError 404 "Tarball not found" [MText "No tarball exists for this package version."]
        ((blobId, _):_) -> do
            file <- liftIO $ BlobStorage.fetch store blobId
            liftIO $ runHook' hook pkgid
            returnOk $ toResponse $ Resource.PackageTarball file blobId (pkgUploadTime pkg)

-- result: cabal file or not-found error
serveCabalFile :: DynamicPath -> MServerPart Response
serveCabalFile dpath = withPackagePath dpath $ \pkg _ -> do
    -- check that the cabal name matches the package
    case lookup "cabal" dpath == Just (display $ packageName pkg) of
        True  -> returnOk $ toResponse (Resource.CabalFile (pkgData pkg))
        False -> mzero

-- A wrapper around DeletePackageVersion that runs the proper hooks.
-- (no authentication though)
doDeletePackage :: CoreFeature -> PackageId -> MServerPart ()
doDeletePackage core pkgid = withPackageVersion pkgid $ \pkg -> do
    update $ DeletePackageVersion pkgid
    nowPkgs <- fmap (flip PackageIndex.lookupPackageName (packageName pkgid) . packageList) $ query GetPackagesState
    runHook' (packageRemoveHook core) pkg
    runHook (packageIndexChange core)
    when (null nowPkgs) $ runHook' (noPackageHook core) pkg
    returnOk ()

-- This is a wrapper around InsertPkgIfAbsent that runs the necessary hooks in core.
doAddPackage :: CoreFeature -> PkgInfo -> IO Bool
doAddPackage core pkgInfo = do
    state <- fmap packageList $ query GetPackagesState
    success <- update $ InsertPkgIfAbsent pkgInfo
    when success $ do
        let existedBefore = packageExists state pkgInfo
        when (not existedBefore) $ do
            runHook' (newPackageHook core) pkgInfo
        runHook' (packageAddHook core) pkgInfo
        runHook (packageIndexChange core)
    return success

-- A wrapper around MergePkg.
doMergePackage :: CoreFeature -> PkgInfo -> IO ()
doMergePackage core pkgInfo = do
    state <- fmap packageList $ query GetPackagesState
    let mprev = PackageIndex.lookupPackageId state (packageId pkgInfo)
        nameExists = packageExists state pkgInfo
    update $ MergePkg pkgInfo
    when (not nameExists) $ do
        runHook' (newPackageHook core) pkgInfo
    case mprev of
        -- TODO: modify MergePkg to get the newly merged package info, not the pre-merge argument
        Just prev -> runHook'' (packageChangeHook core) prev pkgInfo
        Nothing -> runHook' (packageAddHook core) pkgInfo
    runHook (packageIndexChange core)

