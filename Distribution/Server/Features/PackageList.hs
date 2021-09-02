{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.PackageList (
    ListFeature(..),
    initListFeature,
    PackageItem(..),
    tagHistogram
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.Core
-- [reverse index disabled] import Distribution.Server.Features.ReverseDependencies
import Distribution.Server.Features.Votes
import Distribution.Server.Features.DownloadCount
import Distribution.Server.Features.Tags
import Distribution.Server.Features.Users
import Distribution.Server.Features.Upload(UploadFeature(..))
import Distribution.Server.Users.Users (userIdToName)
import qualified Distribution.Server.Users.UserIdSet as UserIdSet
import Distribution.Server.Users.Group(UserGroup(..), GroupDescription(..))
import Distribution.Server.Features.PreferredVersions
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Util.CountingMap (cmFind)

import Distribution.Server.Packages.Types
-- [reverse index disabled] import Distribution.Server.Packages.Reverse
import Distribution.Server.Users.Types

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Utils.ShortText (fromShortText)

import Control.Concurrent
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime(..))


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
  --itemPlatform :: Bool,
    -- Voting score for the package
    itemVotes :: !Float,
    -- The total number of downloads. (For sorting, not displaying.)
    -- Updated periodically.
    itemDownloads :: !Int,
    -- The number of direct revdeps. (Likewise.)
    -- also: distinguish direct/flat?
    -- [reverse index disabled] itemRevDepsCount :: !Int,
    -- Whether there's a library here.
    itemHasLibrary :: !Bool,
    -- How many executables (>=0) this package has.
    itemNumExecutables :: !Int,
    -- How many test suites (>=0) this package has.
    itemNumTests :: !Int,
    -- How many benchmarks (>=0) this package has.
    itemNumBenchmarks :: !Int,
    -- Last upload date
    itemLastUpload :: !UTCTime
    -- Hotness: a more heuristic way to sort packages. presently non-existent.
  --itemHotness :: Int
}

instance MemSize PackageItem where
    memSize (PackageItem a b c d e f g h i j k l) = memSize12 a b c d e f g h i j k l


emptyPackageItem :: PackageName -> PackageItem
emptyPackageItem pkg = PackageItem pkg Set.empty Nothing "" []
                                   0 0 False 0 0 0 (UTCTime (toEnum 0) 0)


initListFeature :: ServerEnv
                -> IO (CoreFeature
                    -- [reverse index disabled] -> ReverseFeature
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
              -- [reverse index disabled] revs
              download
              votesf@VotesFeature{..}
              tagsf@TagsFeature{..}
              versions@VersionsFeature{..}
              users@UserFeature{..}
              uploads@UploadFeature{..} -> do

      let (feature, modifyItem, updateDesc) =
            listFeature core download votesf tagsf versions users uploads
                        itemCache itemUpdate

      registerHookJust packageChangeHook isPackageChangeAny $ \(pkgid, _) ->
        updateDesc (packageName pkgid)

      registerHookJust packageChangeHook isPackageAdd $ \pkg -> do
        let pkgname = packageName . packageId $ pkg
        modifyItem pkgname (\x -> x {itemLastUpload = fst (pkgOriginalUploadInfo pkg)})
        runHook_ itemUpdate (Set.singleton pkgname)

      registerHook groupChangedHook $ \(gd,_,_,_,_) ->
         case fmap (mkPackageName . fst) (groupEntity gd) of
              Just pkgname -> do
                   maintainers <- queryUserGroup (maintainersGroup pkgname)
                   users' <- queryGetUserDb
                   modifyItem pkgname (\x -> x {itemMaintainer = map (userIdToName users') (UserIdSet.toList maintainers)})
                   runHook_ itemUpdate (Set.singleton pkgname)
              Nothing -> return ()

      {- [reverse index disabled]
              votesf@VotesFeature{..}
      registerHook (reverseUpdateHook revs) $ \mrev -> do
          let pkgs = Map.keys mrev
          forM_ pkgs $ \pkgname -> do
              revCount <- query . GetReverseCount $ pkgname
              modifyItem pkgname (updateReverseItem revCount)
          runHook' itemUpdate $ Set.fromDistinctAscList pkgs
      -}

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
            DownloadFeature{..}
            VotesFeature{..}
            TagsFeature{..}
            VersionsFeature{..}
            UserFeature{..}
            UploadFeature{..}
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
        case hasItem of
            True  -> modifyMemState itemCache $ Map.adjust token pkgname
            False -> do
                index <- queryGetPackageIndex
                let pkgs = PackageIndex.lookupPackageName index pkgname
                case pkgs of
                    [] -> return () --this shouldn't happen
                    _  -> modifyMemState itemCache . uncurry Map.insert =<< constructItem (last pkgs)

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
        -- [reverse index disabled] revCount <- query . GetReverseCount $ pkgname
        users <- queryGetUserDb
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
            -- [reverse index disabled] , itemRevDepsCount = directReverseCount revCount
          , itemVotes      = votes
          , itemLastUpload = fst (pkgOriginalUploadInfo pkg)
          }

    ------------------------------
    makeItemList :: [PackageName] -> IO [PackageItem]
    makeItemList pkgnames = do
        mainMap <- readMemState itemCache
        return $ catMaybes $ map (flip Map.lookup mainMap) pkgnames

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
        itemDesc = fromShortText $ synopsis desc,
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
        itemVotes = score
    }

updateDeprecation :: Maybe [PackageName] -> PackageItem -> PackageItem
updateDeprecation pkgs item =
    item {
        itemDeprecated = pkgs
    }

{- [reverse index disabled]
updateReverseItem :: ReverseCount -> PackageItem -> PackageItem
updateReverseItem revCount item =
    item {
        itemRevDepsCount = directReverseCount revCount
    }
-}

updateDownload :: Int -> PackageItem -> PackageItem
updateDownload count item =
    item {
        itemDownloads = count
    }
