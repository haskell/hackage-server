module Distribution.Server.Features.Core (
    CoreFeature,
    coreResource,
    CoreResource(..),
    indexExtras, --FIXME: this is internal state and should not be exported.
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
    doMergePackage,

    -- * Hooks
    packageAddHook,
    packageRemoveHook,
    packageChangeHook,
    packageIndexChange,
    newPackageHook,
    noPackageHook,
    tarballDownload,
  ) where

import Distribution.Server.Acid (query, update)
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump

import Distribution.Server.Packages.Backup
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.State
import Distribution.Server.Users.Backup
import Distribution.Server.Users.State
import qualified Distribution.Server.Framework.Cache as Cache
import qualified Distribution.Server.Packages.Index as Packages.Index
import qualified Codec.Compression.GZip as GZip
import qualified Distribution.Server.Framework.ResourceTypes as Resource
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage
import Distribution.Server.Framework.BlobStorage (BlobStorage)
import Distribution.Server.Util.ServeTarball (serveTarEntry)
import Distribution.Server.Util.ChangeLog (lookupChangeLog)

import Control.Monad (guard, mzero, when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Time.Clock (UTCTime)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (mconcat)
--TODO: why are we importing xhtml here!?
import Text.XHtml.Strict (Html, toHtml, unordList, h3, (<<), anchor, href, (!))
import Data.Ord (comparing)
import Data.List (sortBy, find)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)

import Distribution.Text (display)
import Distribution.Package
import Distribution.Version (Version(..))


data CoreFeature = CoreFeature {
    featureInterface :: HackageFeature,
    coreResource :: CoreResource,

    -- index.tar.gz
    cacheIndexTarball :: Cache.Cache ByteString,
    -- other files to put in the index tarball like preferred-versions
    indexExtras :: Cache.Cache (Map String (ByteString, UTCTime)),

    -- Updating top-level packages
    -- This is run after a package is added
    packageAddHook    :: Hook (PkgInfo -> IO ()),
    -- This is run after a package is removed (but the PkgInfo is retained just for the hook)
    packageRemoveHook :: Hook (PkgInfo -> IO ()),
    -- This is run after a package is changed in some way (essentially added, then removed)
    packageChangeHook :: Hook (PkgInfo -> PkgInfo -> IO ()),
    -- This is called whenever any of the above three hooks is called, but
    -- also for other updates of the index tarball  (e.g. when indexExtras is updated)
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
    coreChangeLogFile :: Resource,
    corePackageTarball :: Resource,

    indexTarballUri :: String,
    indexPackageUri :: String -> String,
    corePackageUri  :: String -> PackageId -> String,
    corePackageName :: String -> PackageName -> String,
    coreCabalUri   :: PackageId -> String,
    coreTarballUri :: PackageId -> String,
    coreChangeLogUri :: PackageId -> String
}

instance IsHackageFeature CoreFeature where
    getFeatureInterface = featureInterface

initCoreFeature :: ServerEnv -> IO CoreFeature
initCoreFeature config = do
    -- Caches
    indexTar <- Cache.newCacheable BS.empty
    extraMap <- Cache.newCacheable Map.empty

    let computeCache = do
            users <- query GetUserDb
            index <- fmap packageList $ query GetPackagesState
            extras <- Cache.getCache extraMap
            Cache.putCache indexTar (GZip.compress $ Packages.Index.write users extras index)

    downHook <- newHook
    addHook  <- newHook
    removeHook <- newHook
    changeHook <- newHook
    indexHook  <- newHook
    newPkgHook <- newHook
    noPkgHook <- newHook
    registerHook indexHook computeCache

    let store = serverBlobStore config
        resources = CoreResource {
            -- the rudimentary HTML resources are for when we don't want an additional HTML feature
            coreIndexPage = (resourceAt "/.:format") { resourceGet = [("html", indexPage $ serverStaticDir config)] }
          , coreIndexTarball = (resourceAt "/packages/index.tar.gz") { resourceGet = [("tarball", Cache.respondCache indexTar Resource.IndexTarball)] }
          , corePackagesPage = (resourceAt "/packages/.:format") { resourceGet = [] } -- -- have basic packages listing?
          , corePackagePage = (resourceAt "/package/:package.:format") { resourceGet = [("html", basicPackagePage resources)] }
          , corePackageRedirect = (resourceAt "/package/") { resourceGet = [("", \_ -> seeOther "/packages/" $ toResponse ())] }
          , corePackageTarball = (resourceAt "/package/:package/:tarball.tar.gz") { resourceGet = [("tarball", runServerPartE . servePackageTarball downHook store)] }
          , coreCabalFile  = (resourceAt "/package/:package/:cabal.cabal") { resourceGet = [("cabal", runServerPartE . serveCabalFile)] }
          , coreChangeLogFile  = (resourceAt "/package/:package/changelog") { resourceGet = [("changelog", runServerPartE . serveChangeLog store)] }
          , indexTarballUri = renderResource (coreIndexTarball resources) []
          , indexPackageUri = \format -> renderResource (corePackagesPage resources) [format]
          , corePackageUri  = \format pkgid -> renderResource (corePackagePage resources) [display pkgid, format]
          , corePackageName = \format pkgname -> renderResource (corePackagePage resources) [display pkgname, format]
          , coreCabalUri   = \pkgid -> renderResource (coreCabalFile resources) [display pkgid, display (packageName pkgid)]
          , coreTarballUri = \pkgid -> renderResource (corePackageTarball resources) [display pkgid, display pkgid]
          , coreChangeLogUri = \pkgid -> renderResource (coreChangeLogFile resources) [display pkgid, display (packageName pkgid)]
          }
    return CoreFeature {
        featureInterface = (emptyHackageFeature "core") {
            featureResources = map ($ resources)
              [ coreIndexPage, coreIndexTarball, corePackagesPage, corePackagePage
              , corePackageRedirect, corePackageTarball, coreCabalFile, coreChangeLogFile ]
          , featurePostInit = runHook indexHook
          , featureDumpRestore = Just (dumpBackup store, restoreBackup store, testRoundtripDummy)
          }
      , coreResource = resources
      , cacheIndexTarball  = indexTar
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
    dumpBackup store = do
      users    <- query GetUserDb
      packages <- query GetPackagesState
      admins   <- query GetHackageAdmins
      packageEntries <- readExportBlobs store $ indexToAllVersions packages
      return $ packageEntries ++ [csvToBackup ["users.csv"] $ usersToCSV users, csvToBackup ["admins.csv"] $ groupToCSV admins]
    restoreBackup store = mconcat [userBackup, packagesBackup store, groupBackup ["admins.csv"] ReplaceHackageAdmins]

-- Should probably look more like an Apache index page (Name / Last modified / Size / Content-type)
basicPackagePage :: CoreResource -> DynamicPath -> ServerPart Response
basicPackagePage r dpath = runServerPartE $ withPackagePath dpath $ \_ pkgs ->
  return $ toResponse $ Resource.XHtml $ showAllP $ sortBy (flip $ comparing packageVersion) pkgs
  where
    showAllP :: [PkgInfo] -> Html
    showAllP pkgs = toHtml [
        h3 << "Downloads",
        unordList $ map (basicPackageSection (coreCabalUri r)
                                             (coreTarballUri r)
                                             (coreChangeLogUri r)) pkgs
     ]

basicPackageSection :: (PackageId -> String)
                    -> (PackageId -> String)
                    -> (PackageId -> String)
                    -> PkgInfo -> [Html]
basicPackageSection cabalUrl tarUrl changeLogUrl pkgInfo =
    let pkgId = packageId pkgInfo; pkgStr = display pkgId in [
    toHtml pkgStr,
    unordList $ [
        [anchor ! [href (cabalUrl pkgId)] << "Package description",
         toHtml " (included in the package)"],
        [anchor ! [href (changeLogUrl pkgId)] << "Package changelog",
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

withPackageId :: DynamicPath -> (PackageId -> ServerPartE a) -> ServerPartE a
withPackageId dpath = require (return $ lookup "package" dpath >>= fromReqURI)

withPackageName :: MonadIO m => DynamicPath -> (PackageName -> ServerPartT m a) -> ServerPartT m a
withPackageName dpath = require (return $ lookup "package" dpath >>= fromReqURI)

packageError :: [MessageSpan] -> ServerPartE a
packageError = errNotFound "Package not found"

withPackage :: PackageId -> (PkgInfo -> [PkgInfo] -> ServerPartE a) -> ServerPartE a
withPackage pkgid func = query GetPackagesState >>= \state ->
    case PackageIndex.lookupPackageName (packageList state) (packageName pkgid) of
        []   ->  packageError [MText "No such package in package index"]
        pkgs  | pkgVersion pkgid == Version [] [] ->
            -- pkgs is sorted by version number and non-empty
            func (last pkgs) pkgs
        pkgs -> case find ((== packageVersion pkgid) . packageVersion) pkgs of
            Nothing  -> packageError [MText $ "No such package version for " ++ display (packageName pkgid)]
            Just pkg -> func pkg pkgs

withPackagePath :: DynamicPath -> (PkgInfo -> [PkgInfo] -> ServerPartE a) -> ServerPartE a
withPackagePath dpath func = withPackageId dpath $ \pkgid -> withPackage pkgid func

withPackageAll :: PackageName -> ([PkgInfo] -> ServerPartE a) -> ServerPartE a
withPackageAll pkgname func = query GetPackagesState >>= \state ->
    case PackageIndex.lookupPackageName (packageList state) pkgname of
        []   -> packageError [MText "No such package in package index"]
        pkgs -> func pkgs

withPackageAllPath :: DynamicPath -> (PackageName -> [PkgInfo] -> ServerPartE a) -> ServerPartE a
withPackageAllPath dpath func = withPackageName dpath $ \pkgname -> withPackageAll pkgname (func pkgname)

withPackageVersion :: PackageId -> (PkgInfo -> ServerPartE a) -> ServerPartE a
withPackageVersion pkgid func = do
    guard (packageVersion pkgid /= Version [] [])
    query GetPackagesState >>= \state -> case PackageIndex.lookupPackageId (packageList state) pkgid of
        Nothing -> packageError [MText $ "No such package version for " ++ display (packageName pkgid)]
        Just pkg -> func pkg

withPackageVersionPath :: DynamicPath -> (PkgInfo -> ServerPartE a) -> ServerPartE a
withPackageVersionPath dpath func = withPackageId dpath $ \pkgid -> withPackageVersion pkgid func

withPackageTarball :: DynamicPath -> (PackageId -> ServerPartE a) -> ServerPartE a
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
servePackageTarball :: Hook (PackageId -> IO ()) -> BlobStorage -> DynamicPath -> ServerPartE Response
servePackageTarball hook store dpath = withPackageTarball dpath $ \pkgid ->
                                       withPackageVersion pkgid $ \pkg ->
    case pkgTarball pkg of
        [] -> errNotFound "Tarball not found" [MText "No tarball exists for this package version."]
        ((tb, _):_) -> do
            let blobId = pkgTarballGz tb
            file <- liftIO $ BlobStorage.fetch store blobId
            liftIO $ runHook' hook pkgid
            return $ toResponse $ Resource.PackageTarball file blobId (pkgUploadTime pkg)

-- result: cabal file or not-found error
serveCabalFile :: DynamicPath -> ServerPartE Response
serveCabalFile dpath = withPackagePath dpath $ \pkg _ -> do
    -- check that the cabal name matches the package
    case lookup "cabal" dpath == Just (display $ packageName pkg) of
        True  -> return $ toResponse (Resource.CabalFile (pkgData pkg))
        False -> mzero

-- result: changelog or not-found error
serveChangeLog :: BlobStorage -> DynamicPath -> ServerPartE Response
serveChangeLog store dpath = withPackagePath dpath $ \pkg _ -> do
    res <- liftIO $ lookupChangeLog store pkg
    case res of
      Left err -> errNotFound "Changelog not found" [MText err]
      Right (fp, offset, name) -> liftIO $ serveTarEntry fp offset name

-- A wrapper around DeletePackageVersion that runs the proper hooks.
-- (no authentication though)
doDeletePackage :: CoreFeature -> PackageId -> ServerPartE ()
doDeletePackage core pkgid = withPackageVersion pkgid $ \pkg -> do
    update $ DeletePackageVersion pkgid
    nowPkgs <- fmap (flip PackageIndex.lookupPackageName (packageName pkgid) . packageList) $ query GetPackagesState
    runHook' (packageRemoveHook core) pkg
    runHook (packageIndexChange core)
    when (null nowPkgs) $ runHook' (noPackageHook core) pkg
    return ()

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

