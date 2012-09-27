{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.Core (
    CoreFeature(..),
    CoreResource(..),
    initCoreFeature,

    -- Misc other utils?
    basicPackageSection,
    packageExists,
    packageIdExists,
  ) where

import Distribution.Server.Acid (query, update)
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump

import Distribution.Server.Features.Users

import Distribution.Server.Packages.Backup
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.State
import qualified Distribution.Server.Framework.Cache as Cache
import qualified Distribution.Server.Packages.Index as Packages.Index
import qualified Codec.Compression.GZip as GZip
import qualified Distribution.Server.Framework.ResourceTypes as Resource
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage

import Control.Applicative
import Control.Monad
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Time.Clock (UTCTime)
import Data.Map (Map)
import qualified Data.Map as Map
--TODO: why are we importing xhtml here!?
import Text.XHtml.Strict (Html, toHtml, unordList, h3, (<<), anchor, href, (!))
import Data.Ord (comparing)
import Data.List (sortBy, find)
import Data.ByteString.Lazy.Char8 (ByteString)

import Distribution.Text (display)
import Distribution.Package
import Distribution.Version (Version(..))


data CoreFeature = CoreFeature {
    coreFeatureInterface :: HackageFeature,

    coreResource     :: CoreResource,

    --TODO: should be made into transactions
    doAddPackage    :: PkgInfo -> IO Bool,
    doMergePackage  :: PkgInfo -> IO (),

    -- Caches
    -- index.tar.gz
    cacheIndexTarball :: Cache.CacheableAction ByteString,
    -- other files to put in the index tarball like preferred-versions
    --FIXME: this is internal state and should not be exported.
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
    tarballDownload :: Hook (PackageId -> IO ()),

    withPackageId   :: forall   a.              DynamicPath -> (PackageId   -> ServerPartE a)   -> ServerPartE a,
    withPackageName :: forall m a. MonadIO m => DynamicPath -> (PackageName -> ServerPartT m a) -> ServerPartT m a,
    withPackage     :: forall   a. PackageId -> (PkgInfo -> [PkgInfo] -> ServerPartE a) -> ServerPartE a,
    withPackagePath :: forall   a. DynamicPath -> (PkgInfo -> [PkgInfo] -> ServerPartE a) -> ServerPartE a,
    withPackageAll  :: forall   a.              PackageName -> ([PkgInfo]   -> ServerPartE a)   -> ServerPartE a,
    withPackageAllPath     :: forall a. DynamicPath -> (PackageName -> [PkgInfo] -> ServerPartE a) -> ServerPartE a,
    withPackageVersion     :: forall a. PackageId   -> (PkgInfo   -> ServerPartE a) -> ServerPartE a,
    withPackageVersionPath :: forall a. DynamicPath -> (PkgInfo   -> ServerPartE a) -> ServerPartE a,
    withPackageTarball     :: forall a. DynamicPath -> (PackageId -> ServerPartE a) -> ServerPartE a
}

data CoreResource = CoreResource {
    coreIndexPage    :: Resource,
    coreIndexTarball :: Resource,
    corePackagesPage :: Resource,
    corePackagePage  :: Resource,
    corePackageRedirect :: Resource,
    coreCabalFile       :: Resource,
    corePackageTarball  :: Resource,

    indexTarballUri :: String,
    indexPackageUri :: String -> String,
    corePackageUri  :: String -> PackageId -> String,
    corePackageName :: String -> PackageName -> String,
    coreCabalUri    :: PackageId -> String,
    coreTarballUri  :: PackageId -> String
}

instance IsHackageFeature CoreFeature where
    getFeatureInterface = coreFeatureInterface

initCoreFeature :: Bool -> ServerEnv -> UserFeature -> IO CoreFeature
initCoreFeature enableCaches env UserFeature{..} = do
    -- Caches
    extraMap <- Cache.newCacheable Map.empty

    indexTar <- Cache.newCacheableAction enableCaches $ do
            users  <- queryGetUserDb
            index  <- packageList <$> query GetPackagesState
            extras <- Cache.getCache extraMap
            return $ GZip.compress $ Packages.Index.write users extras index

    downHook   <- newHook
    addHook    <- newHook
    removeHook <- newHook
    changeHook <- newHook
    indexHook  <- newHook
    newPkgHook <- newHook
    noPkgHook  <- newHook
    registerHook indexHook $ Cache.refreshCacheableAction indexTar

    return $ coreFeature env extraMap indexTar
                         downHook addHook removeHook changeHook
                         indexHook newPkgHook noPkgHook

coreFeature :: ServerEnv
            -> Cache.Cache (Map String (ByteString, UTCTime))
            -> Cache.CacheableAction ByteString
            -> Hook (PackageId -> IO ())
            -> Hook (PkgInfo -> IO ())
            -> Hook (PkgInfo -> IO ())
            -> Hook (PkgInfo -> PkgInfo -> IO ())
            -> Hook (IO ())
            -> Hook (PkgInfo -> IO ())
            -> Hook (PkgInfo -> IO ())
            -> CoreFeature

coreFeature ServerEnv{serverBlobStore = store, serverStaticDir}
            indexExtras cacheIndexTarball
            tarballDownload packageAddHook packageRemoveHook packageChangeHook
            packageIndexChange newPackageHook noPackageHook
  = CoreFeature{..}
  where
    coreFeatureInterface = (emptyHackageFeature "core") {
        featureResources =
          [ coreIndexPage, coreIndexTarball, corePackagesPage, corePackagePage
          , corePackageRedirect, corePackageTarball, coreCabalFile ]
      , featurePostInit    = runHook packageIndexChange
      , featureDumpRestore = Just (dumpBackup, restoreBackup, testRoundtrip)
      }

    coreResource = CoreResource {..}
            -- the rudimentary HTML resources are for when we don't want an additional HTML feature
    coreIndexPage    = (resourceAt "/.:format") { resourceGet = [("html", indexPage serverStaticDir)] }
    coreIndexTarball = (resourceAt "/packages/index.tar.gz") { resourceGet = [("tarball", const $ liftM (toResponse . Resource.IndexTarball) $ Cache.getCacheableAction cacheIndexTarball)] }
    corePackagesPage = (resourceAt "/packages/.:format") { resourceGet = [] } -- -- have basic packages listing?
    corePackagePage  = (resourceAt "/package/:package.:format") { resourceGet = [("html", basicPackagePage)] }
    corePackageRedirect = (resourceAt "/package/") { resourceGet = [("", \_ -> seeOther "/packages/" $ toResponse ())] }
    corePackageTarball  = (resourceAt "/package/:package/:tarball.tar.gz") { resourceGet = [("tarball", runServerPartE . servePackageTarball)] }
    coreCabalFile   = (resourceAt "/package/:package/:cabal.cabal") { resourceGet = [("cabal", runServerPartE . serveCabalFile)] }
    indexTarballUri = renderResource coreIndexTarball []
    indexPackageUri = \format -> renderResource corePackagesPage [format]
    corePackageUri  = \format pkgid -> renderResource corePackagePage [display pkgid, format]
    corePackageName = \format pkgname -> renderResource corePackagePage [display pkgname, format]
    coreCabalUri    = \pkgid -> renderResource coreCabalFile [display pkgid, display (packageName pkgid)]
    coreTarballUri  = \pkgid -> renderResource corePackageTarball [display pkgid, display pkgid]

    indexPage staticDir _ = serveFile (const $ return "text/html") (staticDir ++ "/hackage.html")

    dumpBackup = do
      packages <- query GetPackagesState
      readExportBlobs store $ indexToAllVersions packages

    restoreBackup = packagesBackup store

    testRoundtrip = testRoundtripByQuery' (query GetPackagesState) $ \packages ->
      testBlobsExist store [ blob
                           | pkgInfo <- PackageIndex.allPackages (packageList packages)
                           , (tarball, _) <- pkgTarball pkgInfo
                           , blob <- [pkgTarballGz tarball, pkgTarballNoGz tarball]
                           ]

    -- Should probably look more like an Apache index page (Name / Last modified / Size / Content-type)
    basicPackagePage :: DynamicPath -> ServerPart Response
    basicPackagePage dpath =
      runServerPartE $
        withPackagePath dpath $ \_ pkgs ->
         return $ toResponse $ Resource.XHtml $
           showAllP $ sortBy (flip $ comparing packageVersion) pkgs
      where
        showAllP :: [PkgInfo] -> Html
        showAllP pkgs = toHtml [
            h3 << "Downloads",
            unordList $ map (basicPackageSection coreCabalUri
                                                 coreTarballUri) pkgs
         ]

    ------------------------------------------------------------------------------
    withPackageId :: DynamicPath -> (PackageId -> ServerPartE a) -> ServerPartE a
    withPackageId dpath = require (return $ lookup "package" dpath >>= fromReqURI)

    withPackageName :: MonadIO m => DynamicPath -> (PackageName -> ServerPartT m a) -> ServerPartT m a
    withPackageName dpath = require (return $ lookup "package" dpath >>= fromReqURI)

    packageError :: [MessageSpan] -> ServerPartE a
    packageError = errNotFound "Package not found"

    withPackage :: PackageId -> (PkgInfo -> [PkgInfo] -> ServerPartE a) -> ServerPartE a
    withPackage pkgid func = do
        state <- query GetPackagesState
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
    withPackageAll pkgname func = do
        state <- query GetPackagesState
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
    servePackageTarball :: DynamicPath -> ServerPartE Response
    servePackageTarball dpath =
        withPackageTarball dpath $ \pkgid ->
        withPackageVersion pkgid $ \pkg ->
        case pkgTarball pkg of
            [] -> errNotFound "Tarball not found" [MText "No tarball exists for this package version."]
            ((tb, _):_) -> do
                let blobId = pkgTarballGz tb
                file <- liftIO $ BlobStorage.fetch store blobId
                liftIO $ runHook' tarballDownload pkgid
                return $ toResponse $ Resource.PackageTarball file blobId (pkgUploadTime pkg)

    -- result: cabal file or not-found error
    serveCabalFile :: DynamicPath -> ServerPartE Response
    serveCabalFile dpath =
        withPackagePath dpath $ \pkg _ ->
        -- check that the cabal name matches the package
        case lookup "cabal" dpath == Just (display $ packageName pkg) of
            True  -> return $ toResponse (Resource.CabalFile (cabalFileByteString (pkgData pkg)))
            False -> mzero

{-  Currently unused, should not be in ServerPartE

    -- A wrapper around DeletePackageVersion that runs the proper hooks.
    -- (no authentication though)
    doDeletePackage :: PackageId -> ServerPartE ()
    doDeletePackage pkgid =
      withPackageVersion pkgid $ \pkg -> do
        void $ update $ DeletePackageVersion pkgid
        nowPkgs <- fmap (flip PackageIndex.lookupPackageName (packageName pkgid) . packageList) $ query GetPackagesState
        runHook' packageRemoveHook pkg
        runHook packageIndexChange
        when (null nowPkgs) $ runHook' noPackageHook pkg
        return ()
-}
    -- This is a wrapper around InsertPkgIfAbsent that runs the necessary hooks in core.
    doAddPackage :: PkgInfo -> IO Bool
    doAddPackage pkgInfo = do
        state <- fmap packageList $ query GetPackagesState
        success <- update $ InsertPkgIfAbsent pkgInfo
        when success $ do
            let existedBefore = packageExists state pkgInfo
            when (not existedBefore) $ do
                runHook' newPackageHook pkgInfo
            runHook' packageAddHook pkgInfo
            runHook packageIndexChange
        return success

    -- A wrapper around MergePkg.
    doMergePackage :: PkgInfo -> IO ()
    doMergePackage pkgInfo = do
        state <- fmap packageList $ query GetPackagesState
        let mprev = PackageIndex.lookupPackageId state (packageId pkgInfo)
            nameExists = packageExists state pkgInfo
        -- TODO: Is there a thread-safety issue here?
        void $ update $ MergePkg pkgInfo
        when (not nameExists) $ do
            runHook' newPackageHook pkgInfo
        case mprev of
            -- TODO: modify MergePkg to get the newly merged package info, not the pre-merge argument
            Just prev -> runHook'' packageChangeHook prev pkgInfo
            Nothing -> runHook' packageAddHook pkgInfo
        runHook packageIndexChange

packageExists, packageIdExists :: (Package pkg, Package pkg') => PackageIndex pkg -> pkg' -> Bool
packageExists   state pkg = not . null $ PackageIndex.lookupPackageName state (packageName pkg)
packageIdExists state pkg = maybe False (const True) $ PackageIndex.lookupPackageId state (packageId pkg)

basicPackageSection :: (PackageId -> String)
                    -> (PackageId -> String)
                    -> PkgInfo -> [Html]
basicPackageSection cabalUrl tarUrl pkgInfo =
    let pkgId = packageId pkgInfo; pkgStr = display pkgId in [
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

