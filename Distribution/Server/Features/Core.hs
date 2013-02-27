{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards, DoRec #-}
module Distribution.Server.Features.Core (
    CoreFeature(..),
    CoreResource(..),
    initCoreFeature,

    -- Misc other utils?
    basicPackageSection,
    packageExists,
    packageIdExists,
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.Core.State
import Distribution.Server.Features.Core.Backup

import Distribution.Server.Features.Users

import Distribution.Server.Packages.Types
import Distribution.Server.Users.Types (UserId)
import qualified Distribution.Server.Packages.Index as Packages.Index
import qualified Codec.Compression.GZip as GZip
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import qualified Distribution.Server.Framework.BlobStorage as BlobStorage

import Data.Time.Clock (UTCTime)
import Data.Map (Map)
import qualified Data.Map as Map
--TODO: why are we importing xhtml here!?
import Text.XHtml.Strict (Html, toHtml, unordList, h3, (<<), anchor, href, (!))
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.ByteString.Lazy.Char8 (ByteString)

import Distribution.Text (display)
import Distribution.Package
import Distribution.Version (Version(..))


data CoreFeature = CoreFeature {
    coreFeatureInterface :: HackageFeature,

    coreResource     :: CoreResource,

    -- queries
    queryGetPackageIndex :: MonadIO m => m (PackageIndex PkgInfo),

    -- update transactions
    updateReplacePackageUploader :: MonadIO m
                                 => PackageId -> UserId
                                 -> m (Maybe String),
    updateReplacePackageUploadTime :: MonadIO m
                                   => PackageId -> UTCTime
                                   -> m (Maybe String),
    -- | Set an entry in the 00-index.tar file.
    -- The 00-index.tar file contains all the package entries, but it is an
    -- extensible format and we can add more stuff. E.g. version preferences
    -- or crypto signatures.
    updateArchiveIndexEntry  :: MonadIO m => String -> (ByteString, UTCTime) -> m (),
    --TODO: should be made into transactions
    doAddPackage    :: PkgInfo -> IO Bool,
    doMergePackage  :: PkgInfo -> IO (),

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

    -- Find a package in the package DB
    lookupPackageName :: PackageName -> ServerPartE [PkgInfo],
    lookupPackageId   :: PackageId   -> ServerPartE PkgInfo
}

instance IsHackageFeature CoreFeature where
    getFeatureInterface = coreFeatureInterface

data CoreResource = CoreResource {
    corePackagesPage    :: Resource,
    corePackagePage     :: Resource,
    coreCabalFile       :: Resource,
    corePackageTarball  :: Resource,

    -- Render resources
    indexPackageUri    :: String -> String,
    corePackageIdUri   :: String -> PackageId -> String,
    corePackageNameUri :: String -> PackageName -> String,
    coreCabalUri       :: PackageId -> String,
    coreTarballUri     :: PackageId -> String,

    -- Find a PackageId or PackageName inside a path
    packageInPath :: (MonadPlus m, FromReqURI a) => DynamicPath -> m a,

    -- TODO: This is a rather ad-hoc function. Do we really need it?
    packageTarballInPath :: MonadPlus m => DynamicPath -> m PackageId,

    -- Check that a package exists (guard fails if version is empty)
    guardValidPackageId   :: PackageId   -> ServerPartE (),
    guardValidPackageName :: PackageName -> ServerPartE ()
}

initCoreFeature :: ServerEnv -> UserFeature -> IO CoreFeature
initCoreFeature env@ServerEnv{serverStateDir, serverCacheDelay,
                              serverVerbosity = verbosity} users = do
    loginfo verbosity "Initialising core feature, start"

    -- Canonical state
    packagesState <- packagesStateComponent verbosity serverStateDir

    -- Ephemeral state
    -- Additional files to put in the index tarball like preferred-versions
    extraMap <- newMemStateWHNF Map.empty

    -- Hooks
    downHook   <- newHook
    addHook    <- newHook
    removeHook <- newHook
    changeHook <- newHook
    indexHook  <- newHook
    newPkgHook <- newHook
    noPkgHook  <- newHook

    rec let (feature, getIndexTarball)
              = coreFeature env users
                            packagesState extraMap indexTar
                            downHook addHook removeHook changeHook
                            indexHook newPkgHook noPkgHook

        -- Caches
        -- The index.tar.gz file
        indexTar <- newAsyncCacheNF getIndexTarball
                      defaultAsyncCachePolicy {
                        asyncCacheName = "index tarball",
                        asyncCacheUpdateDelay  = serverCacheDelay,
                        asyncCacheSyncInit     = False,
                        asyncCacheLogVerbosity = verbosity
                      }

    registerHook indexHook (prodAsyncCache indexTar)

    loginfo verbosity "Initialising core feature, end"
    return feature


packagesStateComponent :: Verbosity -> FilePath -> IO (StateComponent PackagesState)
packagesStateComponent verbosity stateDir = do
  let stateFile = stateDir </> "db" </> "PackagesState"
  st <- logTiming verbosity "Loaded PackagesState" $
          openLocalStateFrom stateFile initialPackagesState
  return StateComponent {
       stateDesc    = "Main package database"
     , acidState    = st
     , getState     = query st GetPackagesState
     , putState     = update st . ReplacePackagesState
     , backupState  = indexToAllVersions
     , restoreState = packagesBackup
     , resetState   = packagesStateComponent verbosity
     }

coreFeature :: ServerEnv
            -> UserFeature
            -> StateComponent PackagesState
            -> MemState (Map String (ByteString, UTCTime))
            -> AsyncCache ByteString
            -> Hook (PackageId -> IO ())
            -> Hook (PkgInfo -> IO ())
            -> Hook (PkgInfo -> IO ())
            -> Hook (PkgInfo -> PkgInfo -> IO ())
            -> Hook (IO ())
            -> Hook (PkgInfo -> IO ())
            -> Hook (PkgInfo -> IO ())
            -> ( CoreFeature
               , IO ByteString )

coreFeature ServerEnv{serverBlobStore = store} UserFeature{..}
            packagesState indexExtras cacheIndexTarball
            tarballDownload packageAddHook packageRemoveHook packageChangeHook
            packageIndexChange newPackageHook noPackageHook
  = (CoreFeature{..}, getIndexTarball)
  where
    coreFeatureInterface = (emptyHackageFeature "core") {
        featureDesc = "Core functionality"
      , featureResources = [
            coreIndexTarball
          , corePackagesPage
          , corePackagePage
          , corePackageRedirect
          , corePackageTarball
          , coreCabalFile
          ]
      , featureState    = [abstractStateComponent packagesState]
      , featureCaches   = [
            CacheComponent {
              cacheDesc       = "main package index tarball",
              getCacheMemSize = memSize <$> readAsyncCache cacheIndexTarball
            }
          , CacheComponent {
              cacheDesc       = "package index extra files",
              getCacheMemSize = memSize <$> readMemState indexExtras
            }
          ]
      , featurePostInit = syncAsyncCache cacheIndexTarball
      }

    -- the rudimentary HTML resources are for when we don't want an additional HTML feature
    coreResource = CoreResource {..}
    coreIndexTarball = (resourceAt "/packages/index.tar.gz") {
        resourceDesc = [(GET, "tarball of package descriptions")]
      , resourceGet  = [("tarball", servePackagesIndex)]
      }
    corePackagesPage = (resourceAt "/packages/.:format") {
        resourceGet = [] -- have basic packages listing?
      }
    corePackagePage = (resourceAt "/package/:package.:format") {
        resourceDesc = [(GET, "Show basic package information")]
      , resourceGet  = [("html", basicPackagePage)]
      }
    corePackageRedirect = (resourceAt "/package/") {
        resourceDesc = [(GET,  "Redirect to /packages/")]
      , resourceGet  = [("", \_ -> seeOther "/packages/" $ toResponse ())]
      }
    corePackageTarball = (resourceAt "/package/:package/:tarball.tar.gz") {
        resourceDesc = [(GET, "Get package tarball")]
      , resourceGet  = [("tarball", runServerPartE . servePackageTarball)]
      }
    coreCabalFile = (resourceAt "/package/:package/:cabal.cabal") {
        resourceDesc = [(GET, "Get package .cabal file")]
      , resourceGet  = [("cabal", runServerPartE . serveCabalFile)]
      }
    indexPackageUri = \format ->
      renderResource corePackagesPage [format]
    corePackageIdUri  = \format pkgid ->
      renderResource corePackagePage [display pkgid, format]
    corePackageNameUri = \format pkgname ->
      renderResource corePackagePage [display pkgname, format]
    coreCabalUri    = \pkgid ->
      renderResource coreCabalFile [display pkgid, display (packageName pkgid)]
    coreTarballUri  = \pkgid ->
      renderResource corePackageTarball [display pkgid, display pkgid]

    packageInPath dpath = maybe mzero return (lookup "package" dpath >>= fromReqURI)

    packageTarballInPath dpath = do
      PackageIdentifier name version <- packageInPath dpath
      case lookup "tarball" dpath >>= fromReqURI of
        Nothing -> mzero
        Just pkgid@(PackageIdentifier name' version') -> do
          -- rules:
          -- * the package name and tarball name must be the same
          -- * the tarball must specify a version
          -- * the package must either have no version or the same version as the tarball
          guard $ name == name' && version' /= Version [] [] && (version == version' || version == Version [] [])
          return pkgid

    guardValidPackageId pkgid = do
      guard (pkgVersion pkgid /= Version [] [])
      void $ lookupPackageId pkgid

    guardValidPackageName pkgname =
      void $ lookupPackageName pkgname

    -- Queries
    --
    queryGetPackageIndex :: MonadIO m => m (PackageIndex PkgInfo)
    queryGetPackageIndex = return . packageList =<< queryState packagesState GetPackagesState

    -- Update transactions
    --
    updateReplacePackageUploader :: MonadIO m => PackageId -> UserId
                                 -> m (Maybe String)
    updateReplacePackageUploader pkgid userid =
      updateState packagesState (ReplacePackageUploader pkgid userid)

    updateReplacePackageUploadTime :: MonadIO m => PackageId -> UTCTime
                                   -> m (Maybe String)
    updateReplacePackageUploadTime pkgid time =
      updateState packagesState (ReplacePackageUploadTime pkgid time)

    updateArchiveIndexEntry :: MonadIO m => String -> (ByteString, UTCTime) -> m ()
    updateArchiveIndexEntry entryName entryDetails = do
      modifyMemState indexExtras (Map.insert entryName entryDetails)
      runHook packageIndexChange

    -- Cache updates
    --
    getIndexTarball :: IO ByteString
    getIndexTarball = do
      users  <- queryGetUserDb  -- note, changes here don't automatically propagate
      index  <- queryGetPackageIndex
      extras <- readMemState indexExtras
      let indexTarball' = GZip.compress (Packages.Index.write users extras index)
      return indexTarball'

    -- Should probably look more like an Apache index page (Name / Last modified / Size / Content-type)
    basicPackagePage :: DynamicPath -> ServerPart Response
    basicPackagePage dpath = runServerPartE $ do
      pkgs <- packageInPath dpath >>= lookupPackageName . pkgName
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
    packageError :: [MessageSpan] -> ServerPartE a
    packageError = errNotFound "Package not found"

    lookupPackageName :: PackageName -> ServerPartE [PkgInfo]
    lookupPackageName pkgname = do
      pkgsIndex <- queryGetPackageIndex
      case PackageIndex.lookupPackageName pkgsIndex pkgname of
        []   -> packageError [MText "No such package in package index"]
        pkgs -> return pkgs

    lookupPackageId :: PackageId -> ServerPartE PkgInfo
    lookupPackageId (PackageIdentifier name (Version [] [])) = do
      pkgs <- lookupPackageName name
      -- pkgs is sorted by version number and non-empty
      return (last pkgs)
    lookupPackageId pkgid = do
      pkgsIndex <- queryGetPackageIndex
      case PackageIndex.lookupPackageId pkgsIndex pkgid of
        Just pkg -> return pkg
        _ -> packageError [MText $ "No such package version for " ++ display (packageName pkgid)]

    ------------------------------------------------------------------------

    servePackagesIndex :: DynamicPath -> ServerPart Response
    servePackagesIndex _ = do
      indexTarball <- readAsyncCache cacheIndexTarball
      return $ toResponse (Resource.IndexTarball indexTarball)

    -- result: tarball or not-found error
    servePackageTarball :: DynamicPath -> ServerPartE Response
    servePackageTarball dpath = do
      pkgid <- packageTarballInPath dpath
      guard (pkgVersion pkgid /= Version [] [])
      pkg <- lookupPackageId pkgid
      case pkgTarball pkg of
          [] -> errNotFound "Tarball not found" [MText "No tarball exists for this package version."]
          ((tb, _):_) -> do
              let blobId = pkgTarballGz tb
              file <- liftIO $ BlobStorage.fetch store blobId
              liftIO $ runHook' tarballDownload pkgid
              return $ toResponse $ Resource.PackageTarball file blobId (pkgUploadTime pkg)

    -- result: cabal file or not-found error
    serveCabalFile :: DynamicPath -> ServerPartE Response
    serveCabalFile dpath = do
      pkg <- packageInPath dpath >>= lookupPackageId
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
        pkgs    <- queryGetPackageIndex
        success <- updateState packagesState $ InsertPkgIfAbsent pkgInfo
        when success $ do
            let existedBefore = packageExists pkgs pkgInfo
            when (not existedBefore) $ do
                runHook' newPackageHook pkgInfo
            runHook' packageAddHook pkgInfo
            runHook packageIndexChange
        return success

    -- A wrapper around MergePkg.
    doMergePackage :: PkgInfo -> IO ()
    doMergePackage pkgInfo = do
        pkgs <- queryGetPackageIndex
        let mprev = PackageIndex.lookupPackageId pkgs (packageId pkgInfo)
            nameExists = packageExists pkgs pkgInfo
        -- TODO: Is there a thread-safety issue here?
        void $ updateState packagesState $ MergePkg pkgInfo
        when (not nameExists) $ do
            runHook' newPackageHook pkgInfo
        case mprev of
            -- TODO: modify MergePkg to get the newly merged package info, not the pre-merge argument
            Just prev -> runHook'' packageChangeHook prev pkgInfo
            Nothing -> runHook' packageAddHook pkgInfo
        runHook packageIndexChange

packageExists, packageIdExists :: (Package pkg, Package pkg') => PackageIndex pkg -> pkg' -> Bool
packageExists   pkgs pkg = not . null $ PackageIndex.lookupPackageName pkgs (packageName pkg)
packageIdExists pkgs pkg = maybe False (const True) $ PackageIndex.lookupPackageId pkgs (packageId pkg)

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

