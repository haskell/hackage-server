{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.PackageList (
    ListFeature(..),
    initListFeature,
    PackageItem(..),
    tagHistogram
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.Core
import Distribution.Server.Features.ReverseDependencies
import Distribution.Server.Features.Votes
import Distribution.Server.Features.DownloadCount
import Distribution.Server.Features.Tags
import Distribution.Server.Features.Users
import Distribution.Server.Features.Upload(UploadFeature(..))
import Distribution.Server.Users.Users
import qualified Distribution.Server.Users.UserIdSet as UserIdSet
import Distribution.Server.Users.Group(UserGroup(..), GroupDescription(..))
import Distribution.Server.Features.PreferredVersions
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Util.CountingMap (cmFind)

import Distribution.Server.Packages.Types
import Distribution.Server.Users.Types

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration

import Control.Concurrent
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


data ListFeature = ListFeature {
    listFeatureInterface :: HackageFeature,

    itemUpdate :: Hook (Set PackageName) (),

    constructItemIndex :: IO (Map PackageName PackageItem),
    makeItemList :: [PackageName] -> IO [PackageItem],
    makeItemMap  :: forall a. Map PackageName a -> IO (Map PackageName (PackageItem, a)),
    getAllLists  :: IO (Map PackageName PackageItem)
}

instance IsHackageFeature ListFeature where
    getFeatureInterface = listFeatureInterface


data PackageItem = PackageItem {
    -- The name of the package
    itemName :: !PackageName,
    -- The tags for this package
    itemTags :: !(Set Tag),
    -- If the package is deprecated, what is it deprecated in favor of
    itemDeprecated :: !(Maybe [PackageName]),
    -- The description of the package from its Cabal file
    itemDesc :: !String,
    -- Maintainer of the package
    itemMaintainer :: [UserName],
    -- Whether the item is in the Haskell Platform
    -- itemPlatform :: Bool,
    -- Number of votes for the package
    itemVotes :: !Float,
    -- The total number of downloads. (For sorting, not displaying.)
    -- Updated periodically.
    itemDownloads :: !Int,
    -- The number of direct revdeps. (Likewise.)
    -- also: distinguish direct/flat?
    itemRevDepsCount :: !Int,
    -- Whether there's a library here.
    itemHasLibrary :: !Bool,
    -- How many executables (>=0) this package has.
    itemNumExecutables :: !Int,
    -- How many test suites (>=0) this package has.
    itemNumTests :: !Int,
    -- How many benchmarks (>=0) this package has.
    itemNumBenchmarks :: !Int,
    -- Hotness: a more heuristic way to sort packages
    -- Hotness = recent downloads + stars + 2 * no rev deps
    itemHotness :: !Float
}

instance MemSize PackageItem where
    memSize (PackageItem a b c d e f g h i j k l m) = memSize13 a b c d e f g h i j k l m


emptyPackageItem :: PackageName -> PackageItem
emptyPackageItem pkg = PackageItem pkg Set.empty Nothing "" []
                                   0 0 0 False 0 0 0 0

initListFeature :: ServerEnv
                -> IO (CoreFeature
                    -> ReverseFeature
                    -> DownloadFeature
                    -> VotesFeature
                    -> TagsFeature
                    -> VersionsFeature
                    -> UserFeature
                    -> UploadFeature
                    -> IO ListFeature)
initListFeature _env = do
    itemCache  <- newMemStateWHNF Map.empty
    itemUpdate <- newHook

    return $ \core@CoreFeature{..}
              revs@ReverseFeature{..}
              download
              votesf@VotesFeature{..}
              tagsf@TagsFeature{..}
              versions@VersionsFeature{..}
              users@UserFeature{..}
              uploads@UploadFeature{..} -> do

      let (feature, modifyItem, updateDesc) =
            listFeature core revs download votesf tagsf versions users uploads
                        itemCache itemUpdate

      registerHookJust packageChangeHook isPackageChangeAny $ \(pkgid, _) ->
        updateDesc (packageName pkgid)

      registerHook groupChangedHook $ \(gd,_,_,_,_) ->
         case fmap (PackageName . fst) (groupEntity gd) of
              Just pkgname -> do
                   maintainers <- queryUserGroup (maintainersGroup pkgname)
                   users <- queryGetUserDb
                   modifyItem pkgname (\x -> x {itemMaintainer = map (userIdToName users) (UserIdSet.toList maintainers)})
                   runHook_ itemUpdate (Set.singleton pkgname)
              Nothing -> return ()


      registerHook reverseHook $ \pkgids -> do
          let pkgs = map pkgName pkgids
          forM_ pkgs $ \pkgname -> do
              revCount <- revPackageStats pkgname
              modifyItem pkgname (updateReverseItem revCount)
          runHook_ itemUpdate $ Set.fromDistinctAscList pkgs


      registerHook votesUpdated $ \(pkgname, _) -> do
          votes <- pkgNumScore pkgname
          modifyItem pkgname (updateVoteItem votes)
          runHook_ itemUpdate (Set.singleton pkgname)

      registerHook tagsUpdated $ \(pkgs, _) -> do
          forM_ (Set.toList pkgs) $ \pkgname -> do
              tags <- queryTagsForPackage pkgname
              modifyItem pkgname (updateTagItem tags)
          runHook_ itemUpdate pkgs

      registerHook deprecatedHook $ \(pkgname, mpkgs) -> do
          modifyItem pkgname (updateDeprecation mpkgs)
          runHook_ itemUpdate (Set.singleton pkgname)

      return feature


listFeature :: CoreFeature
            -> ReverseFeature
            -> DownloadFeature
            -> VotesFeature
            -> TagsFeature
            -> VersionsFeature
            -> UserFeature
            -> UploadFeature
            -> MemState (Map PackageName PackageItem)
            -> Hook (Set PackageName) ()
            -> (ListFeature,
                PackageName -> (PackageItem -> PackageItem) -> IO (),
                PackageName -> IO ())

listFeature CoreFeature{..}
            ReverseFeature{..} DownloadFeature{..} VotesFeature{..} TagsFeature{..} VersionsFeature{..} UserFeature{..} UploadFeature{..}
            itemCache itemUpdate
  = (ListFeature{..}, modifyItem, updateDesc)
  where
    listFeatureInterface = (emptyHackageFeature "list") {
        featurePostInit = do itemsCache
                             void $ forkIO periodicDownloadRefresh
      , featureState    = []
      , featureCaches   = [
            CacheComponent {
              cacheDesc       = "per-package-name summary info",
              getCacheMemSize = memSize <$> readMemState itemCache
            }
          ]
      }
      where itemsCache = do
                items <- constructItemIndex
                writeMemState itemCache items
            periodicDownloadRefresh = forever $ do
                --FIXME: don't do this if nothing has changed!
                threadDelay (30 * 60 * 1000000) -- 30 minutes
                refreshDownloads

    modifyItem pkgname token = do
        hasItem <- fmap (Map.member pkgname) $ readMemState itemCache
        if hasItem then modifyMemState itemCache $ Map.adjust token pkgname
                   else (do
                           index <- queryGetPackageIndex
                           let pkgs = PackageIndex.lookupPackageName index pkgname
                           case pkgs of
                               [] -> return () --this shouldn't happen
                               _  -> modifyMemState itemCache . uncurry Map.insert =<< constructItem (last pkgs))
    updateDesc pkgname = do
        index <- queryGetPackageIndex
        let pkgs = PackageIndex.lookupPackageName index pkgname
        case pkgs of
           [] -> modifyMemState itemCache (Map.delete pkgname)
           _  -> modifyItem pkgname (updateDescriptionItem $ pkgDesc $ last pkgs)
        runHook_ itemUpdate (Set.singleton pkgname)

    refreshDownloads = do
            downs <- recentPackageDownloads
            modifyMemState itemCache $ Map.mapWithKey $ \pkg item ->
              updateDownload (cmFind pkg downs) item
            -- Say all packages were updated here (detecting this is more laborious)
            mainMap <- readMemState itemCache
            runHook_ itemUpdate (Set.fromDistinctAscList $ Map.keys mainMap)

    constructItemIndex :: IO (Map PackageName PackageItem)
    constructItemIndex = do
        index <- queryGetPackageIndex
        items <- mapM (constructItem . last) $ PackageIndex.allPackagesByName index
        return $ Map.fromList items

    constructItem :: PkgInfo -> IO (PackageName, PackageItem)
    constructItem pkg = do
        let pkgname = packageName pkg
        users <- queryGetUserDb
        revCount <- revPackageStats pkgname
        tags  <- queryTagsForPackage pkgname
        downs <- recentPackageDownloads
        votes <- pkgNumScore pkgname
        deprs <- queryGetDeprecatedFor pkgname
        maintainers <- queryUserGroup (maintainersGroup pkgname)

        return $ (,) pkgname $ (updateDescriptionItem (pkgDesc pkg) $ emptyPackageItem pkgname) {
            itemTags       = tags
          , itemMaintainer = map (userIdToName users) (UserIdSet.toList maintainers)
          , itemDeprecated = deprs
          , itemDownloads  = cmFind pkgname downs
          , itemVotes = votes
          , itemRevDepsCount = directCount revCount
          , itemHotness = votes + fromIntegral (cmFind pkgname downs) + fromIntegral (directCount revCount)*2
          }

    ------------------------------
    makeItemList :: [PackageName] -> IO [PackageItem]
    makeItemList pkgnames = do
        mainMap <- readMemState itemCache
        return $ mapMaybe (`Map.lookup` mainMap) pkgnames

    makeItemMap :: Map PackageName a -> IO (Map PackageName (PackageItem, a))
    makeItemMap pkgmap = do
        mainMap <- readMemState itemCache
        return $ Map.intersectionWith (,) mainMap pkgmap

    getAllLists :: IO (Map PackageName PackageItem)
    getAllLists = readMemState itemCache

tagHistogram :: [PackageItem] -> Map Tag Int
tagHistogram = Map.fromListWith (+) . map (flip (,) 1) . concatMap (Set.toList . itemTags)

updateDescriptionItem :: GenericPackageDescription -> PackageItem -> PackageItem
updateDescriptionItem genDesc item =
    let desc = flattenPackageDescription genDesc
    in item {
        itemDesc = synopsis desc,
        -- This checks if the library is buildable. However, since
        -- desc is flattened, we might miss some flags. Perhaps use the
        -- CondTree instead.
        -- itemMaintainer = maintainer desc,
        itemHasLibrary = hasLibs desc,
        itemNumExecutables = length . filter (buildable . buildInfo) $ executables desc,
        itemNumTests = length . filter (buildable . testBuildInfo) $ testSuites desc,
        itemNumBenchmarks = length . filter (buildable . benchmarkBuildInfo) $ benchmarks desc
    }

updateTagItem :: Set Tag -> PackageItem -> PackageItem
updateTagItem tags item =
    item {
        itemTags = tags
    }

updateVoteItem :: Float -> PackageItem -> PackageItem
updateVoteItem score item =
    item {
        itemVotes = score,
        itemHotness = score + fromIntegral (itemDownloads item) + fromIntegral (itemRevDepsCount item)*2
    }

updateDeprecation :: Maybe [PackageName] -> PackageItem -> PackageItem
updateDeprecation pkgs item =
    item {
        itemDeprecated = pkgs
    }

updateReverseItem :: ReverseCount -> PackageItem -> PackageItem
updateReverseItem revCount item =
    item {
        itemRevDepsCount = directCount revCount,
        itemHotness = fromIntegral (directCount revCount)*2 + itemVotes item + fromIntegral (itemDownloads item)
    }

updateDownload :: Int -> PackageItem -> PackageItem
updateDownload count item =
    item {
        itemDownloads = count,
        itemHotness = fromIntegral count + itemVotes item + fromIntegral (itemRevDepsCount item)*2
    }
