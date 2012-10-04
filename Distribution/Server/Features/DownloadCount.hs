{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.DownloadCount (
    DownloadFeature(..),
    DownloadResource(..),
    initDownloadFeature,

    packageDowns,
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import qualified Distribution.Server.Framework.Cache as Cache

import Distribution.Server.Features.DownloadCount.State
import Distribution.Server.Features.DownloadCount.Backup

import Distribution.Server.Features.Core

import Distribution.Server.Util.Histogram

import Distribution.Package

import Data.Time.Clock
import Control.Arrow (second)
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import qualified Data.Map as Map


data DownloadFeature = DownloadFeature {
    downloadFeatureInterface :: HackageFeature,

    queryGetDownloadInfo :: MonadIO m => PackageName -> m DownloadInfo,

    downloadResource :: DownloadResource,

    getDownloadHistogram :: IO (Histogram PackageName),
    perVersionDownloads :: (MonadIO m, Package pkg) => pkg -> m (Int, Int),
    sortedPackages :: IO [(PackageName, Int)]
}

instance IsHackageFeature DownloadFeature where
    getFeatureInterface = downloadFeatureInterface

data DownloadResource = DownloadResource {
    topDownloads :: Resource
}

initDownloadFeature :: ServerEnv -> CoreFeature -> IO DownloadFeature
initDownloadFeature ServerEnv{serverStateDir} core = do

    downloadState <- openLocalStateFrom
                       (serverStateDir </> "db" </> "DownloadCounts")
                       initialDownloadCounts

    downChan <- newChan
    downHist <- Cache.newCacheable emptyHistogram
    registerHook (tarballDownload core) $ writeChan downChan

    return $
      downloadFeature core
                      downloadState
                      downChan downHist


downloadFeature :: CoreFeature
                -> AcidState DownloadCounts
                -> Chan PackageId
                -> Cache.Cache (Histogram PackageName)
                -> DownloadFeature

downloadFeature CoreFeature{}
                downloadState downloadStream downloadHistogram
  = DownloadFeature{..}
  where
    downloadFeatureInterface = (emptyHackageFeature "download") {
        featureResources = map (\x -> x $ downloadResource) [topDownloads]
      , featurePostInit  = do countCache
                              forkIO transferDownloads >> return ()
      , featureCheckpoint = do
          createCheckpoint downloadState
      , featureShutdown = do
          closeAcidState downloadState
      , featureDumpRestore = Just (dumpBackup,
                                   restoreBackup,
                                   testRoundtripByQuery (query downloadState GetDownloadCounts))
      }

    countCache = do
        dc <- query downloadState GetDownloadCounts
        let dmap = map (second packageDowns) (Map.toList $ downloadMap dc)
        Cache.putCache downloadHistogram (constructHistogram dmap)

    transferDownloads = forever $ do
        pkg <- readChan downloadStream
        time <- getCurrentTime
        (_, new) <- update downloadState $ RegisterDownload (utctDay time) pkg 1
        Cache.modifyCache downloadHistogram
            (updateHistogram (packageName pkg) new)

    dumpBackup = do
        dc <- query downloadState GetDownloadCounts
        return [csvToBackup ["downloads.csv"] $ downloadsToCSV dc]

    restoreBackup = downloadsBackup downloadState

    queryGetDownloadInfo :: MonadIO m => PackageName -> m DownloadInfo
    queryGetDownloadInfo name = query' downloadState (GetDownloadInfo name)

    downloadResource = DownloadResource
              { topDownloads = resourceAt "/packages/top.:format"
              }

    getDownloadHistogram :: IO (Histogram PackageName)
    getDownloadHistogram = Cache.getCache downloadHistogram

    --totalDownloadCount :: MonadIO m => m Int
    --totalDownloadCount = liftM totalDownloads $ query downloadState GetDownloadCounts

    -- sortedPackages and sortByDownloads both order packages by total downloads without exposing download data

    -- A lazy list of the top packages, which can be filtered, taken from, etc.
    -- Does not include packages with no downloads.
    sortedPackages :: IO [(PackageName, Int)]
    sortedPackages = fmap topCounts $ Cache.getCache downloadHistogram

    {-
    -- Sorts a list of package-y items by their download count.
    -- Use sortedPackages to get an entire list.
    -- TODO: use the Histogram's sortByCounts for this
    sortByDownloads :: MonadIO m => (a -> PackageName) -> [a] -> m [(a, Int)]
    sortByDownloads nameFunc pkgs = query' downloadState GetDownloadCounts >>= \counts -> do
        let modEntry pkg = (pkg, lookupPackageDowns (nameFunc pkg) downloadMap)
        return $ sortBy (comparing snd) $ map modEntry pkgs
    -}

    -- For at-a-glance download information.
    perVersionDownloads :: (MonadIO m, Package pkg) => pkg -> m (Int, Int)
    perVersionDownloads pkg = do
        info <- query' downloadState $ GetDownloadInfo (packageName pkg)
        let (PackageDownloads total perVersion) = packageDownloads info
        return (total, Map.findWithDefault 0 (packageVersion pkg) perVersion)

