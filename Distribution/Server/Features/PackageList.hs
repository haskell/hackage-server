module Distribution.Server.Features.PackageList (
    ListFeature,
    itemUpdate,
    initListFeature,
    PackageItem(..),
    makeItemList,
    makeItemMap,
    getAllLists,
    tagHistogram
  ) where

import Distribution.Server.Acid (query)
import Distribution.Server.Framework
import Distribution.Server.Features.Core
import Distribution.Server.Features.ReverseDependencies
import Distribution.Server.Features.DownloadCount
import Distribution.Server.Features.Tags
import Distribution.Server.Features.PreferredVersions
import qualified Distribution.Server.Framework.Cache as Cache
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Distribution.Server.Packages.State
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Tag
import Distribution.Server.Packages.Preferred
import Distribution.Server.Packages.Reverse
import Distribution.Server.Packages.Downloads
import Distribution.Server.Util.Histogram

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration

import Control.Concurrent
import Control.Monad
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data ListFeature = ListFeature {
    itemCache :: Cache.Cache (Map PackageName PackageItem),
    itemUpdate :: Hook (Set PackageName -> IO ()),
    refreshDownloads :: IO ()
}

data PackageItem = PackageItem {
    -- The name of the package
    itemName :: !PackageName,
    -- The tags for this package
    itemTags :: !(Set Tag),
    -- If the package is deprecated, what is it deprecated in favor of
    itemDeprecated :: !(Maybe [PackageName]),
    -- The description of the package from its Cabal file
    itemDesc :: !String,
    -- Whether the item is in the Haskell Platform
  --itemPlatform :: Bool,
    -- The total number of downloads. (For sorting, not displaying.)
    -- Updated periodically.
    itemDownloads :: !Int,
    -- The number of direct revdeps. (Likewise.)
    -- also: distinguish direct/flat?
    itemRevDepsCount :: !Int,
    -- Whether there's a library here.
    itemHasLibrary :: !Bool,
    -- How many executables (>=0) this package has.
    itemNumExecutables :: !Int
    -- Hotness: a more heuristic way to sort packages. presently non-existent.
  --itemHotness :: Int
}
emptyPackageItem :: PackageName -> PackageItem
emptyPackageItem pkg = PackageItem pkg Set.empty Nothing "" 0 0 False 0

instance IsHackageFeature ListFeature where
    getFeatureInterface listf = (emptyHackageFeature "list") {
        featurePostInit = do itemsCache
                             void $ forkIO periodicDownloadRefresh
      }
      where itemsCache = do
                items <- constructItemIndex
                Cache.putCache (itemCache listf) items
            periodicDownloadRefresh = forever $ do
                threadDelay (10 * 60 * 1000000) -- 10 minutes
                refreshDownloads listf

initListFeature :: ServerEnv -> CoreFeature -> ReverseFeature -> DownloadFeature
                -> TagsFeature -> VersionsFeature -> IO ListFeature
initListFeature _ core revs downs tagf versions = do
    iCache <- Cache.newCache Map.empty id
    iUpdate <- newHook
    let modifyItem pkgname token = do
            hasItem <- fmap (Map.member pkgname) $ Cache.getCache iCache
            case hasItem of
                True  -> Cache.modifyCache iCache $ Map.adjust token pkgname
                False -> do
                    index <- fmap packageList $ query GetPackagesState
                    let pkgs = PackageIndex.lookupPackageName index pkgname
                    case pkgs of
                        [] -> return () --this shouldn't happen
                        _  -> Cache.modifyCache iCache . uncurry Map.insert =<< constructItem (last pkgs)
        updateDesc pkgname = do
            index <- fmap packageList $ query GetPackagesState
            let pkgs = PackageIndex.lookupPackageName index pkgname
            case pkgs of
               [] -> Cache.modifyCache iCache (Map.delete pkgname)
               _  -> modifyItem pkgname (updateDescriptionItem $ pkgDesc $ last pkgs)
            runHook' (iUpdate) $ Set.singleton pkgname
    registerHook (packageAddHook core) $ updateDesc . packageName
    registerHook (packageRemoveHook core) $ updateDesc . packageName
    registerHook (packageChangeHook core) $ \_ -> updateDesc . packageName
    registerHook (reverseUpdateHook revs) $ \mrev -> do
        let pkgs = Map.keys mrev
        forM_ pkgs $ \pkgname -> do
            revCount <- query . GetReverseCount $ pkgname
            modifyItem pkgname (updateReverseItem revCount)
        runHook' (iUpdate) $ Set.fromDistinctAscList pkgs
    registerHook (tagsUpdated tagf) $ \pkgs _ -> do
        forM_ (Set.toList pkgs) $ \pkgname -> do
            tags <- query . TagsForPackage $ pkgname
            modifyItem pkgname (updateTagItem tags)
        runHook' (iUpdate) pkgs
    registerHook (deprecatedHook versions) $ \pkgname mpkgs -> do
        modifyItem pkgname (updateDeprecation mpkgs)
        runHook' (iUpdate) $ Set.singleton pkgname
    let updateDownloads = do
            hist <- getDownloadHistogram downs
            Cache.modifyCache iCache $ Map.mapWithKey (\pkg item -> updateDownload (getCount hist pkg) item)
            -- Say all packages were updated here (detecting this is more laborious)
            mainMap <- Cache.getCache iCache
            runHook' iUpdate (Set.fromDistinctAscList $ Map.keys mainMap)
    return $ ListFeature
      { itemCache = iCache
      , itemUpdate = iUpdate
      , refreshDownloads = updateDownloads
      }

constructItemIndex :: IO (Map PackageName PackageItem)
constructItemIndex = do
    index <- fmap packageList $ query GetPackagesState
    items <- mapM (constructItem . last) $ PackageIndex.allPackagesByName index
    return $ Map.fromList items

constructItem :: PkgInfo -> IO (PackageName, PackageItem)
constructItem pkg = do
    let pkgname = packageName pkg
    revCount <- query . GetReverseCount $ pkgname
    tags <- query . TagsForPackage $ pkgname
    infos <- query . GetDownloadInfo $ pkgname
    deprs <- query . GetDeprecatedFor $ pkgname
    return $ (,) pkgname $ (updateDescriptionItem (pkgDesc pkg) $ emptyPackageItem pkgname) {
        itemTags = tags,
        itemDeprecated = deprs,
        itemDownloads = packageDowns infos,
        itemRevDepsCount = directReverseCount revCount
    }

updateDescriptionItem :: GenericPackageDescription -> PackageItem -> PackageItem
updateDescriptionItem genDesc item = 
    let desc = flattenPackageDescription genDesc
    in item {
        itemDesc = synopsis desc,
        -- This checks if the library is buildable. However, since
        -- desc is flattened, we might miss some flags. Perhaps use the
        -- CondTree instead.
        itemHasLibrary = hasLibs desc,
        itemNumExecutables = length . filter (buildable . buildInfo) $ executables desc
    }

updateTagItem :: Set Tag -> PackageItem -> PackageItem
updateTagItem tags item =
    item {
        itemTags = tags
    }

updateDeprecation :: Maybe [PackageName] -> PackageItem -> PackageItem
updateDeprecation pkgs item =
    item {
        itemDeprecated = pkgs
    }

updateReverseItem :: ReverseCount -> PackageItem -> PackageItem
updateReverseItem revCount item =
    item {
        itemRevDepsCount = directReverseCount revCount
    }

updateDownload :: Int -> PackageItem -> PackageItem
updateDownload count item =
    item {
        itemDownloads = count
    }

------------------------------
makeItemList :: ListFeature -> [PackageName] -> IO [PackageItem]
makeItemList listf pkgnames = do
    mainMap <- Cache.getCache (itemCache listf)
    return $ catMaybes $ map (flip Map.lookup mainMap) pkgnames

makeItemMap :: ListFeature -> Map PackageName a -> IO (Map PackageName (PackageItem, a))
makeItemMap listf pkgmap = do
    mainMap <- Cache.getCache (itemCache listf)
    return $ Map.intersectionWith (,) mainMap pkgmap

getAllLists :: ListFeature -> IO (Map PackageName PackageItem)
getAllLists listf = Cache.getCache (itemCache listf)

tagHistogram :: [PackageItem] -> Map Tag Int
tagHistogram = Map.fromListWith (+) . map (flip (,) 1) . concatMap (Set.toList . itemTags)

