{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.DownloadCount (
    DownloadFeature(..),
    DownloadResource(..),
    initDownloadFeature,
  ) where

import Distribution.Server.Acid (query, update)
import Distribution.Server.Framework
import Distribution.Server.Features.Core

import Distribution.Server.Packages.Downloads
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Packages.Backup.Downloads
import Distribution.Server.Util.Histogram
import qualified Distribution.Server.Framework.Cache as Cache

import Distribution.Package

import Data.Time.Clock
import Control.Arrow (second)
import Control.Monad (forever)
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import qualified Data.Map as Map
import Control.Monad.Trans (MonadIO)

data DownloadFeature = DownloadFeature {
    downloadFeatureInterface :: HackageFeature,
    
    downloadResource :: DownloadResource,
    downloadStream :: Chan PackageId,
    downloadHistogram :: Cache.Cache (Histogram PackageName),

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
initDownloadFeature _ core = do
    downChan <- newChan
    downHist <- Cache.newCacheable emptyHistogram
    registerHook (tarballDownload core) $ writeChan downChan

    return $
      downloadFeature core
                      downChan downHist 


downloadFeature :: CoreFeature
                -> Chan PackageId
                -> Cache.Cache (Histogram PackageName)
                -> DownloadFeature

downloadFeature CoreFeature{}
                downloadStream downloadHistogram
  = DownloadFeature{..}
  where
    downloadFeatureInterface = (emptyHackageFeature "download") {
        featureResources = map (\x -> x $ downloadResource) [topDownloads]
      , featurePostInit  = do countCache
                              forkIO transferDownloads >> return ()
      , featureDumpRestore = Just (dumpBackup, restoreBackup, testRoundtripByQuery (query GetDownloadCounts))
      }

    countCache = do
        dc <- query GetDownloadCounts
        let dmap = map (second packageDowns) (Map.toList $ downloadMap dc)
        Cache.putCache downloadHistogram (constructHistogram dmap)

    transferDownloads = forever $ do
        pkg <- readChan downloadStream
        time <- getCurrentTime
        (_, new) <- update $ RegisterDownload (utctDay time) pkg 1
        Cache.modifyCache downloadHistogram
            (updateHistogram (packageName pkg) new)

    dumpBackup = do
        dc <- query GetDownloadCounts
        return [csvToBackup ["downloads.csv"] $ downloadsToCSV dc]

    restoreBackup = downloadsBackup

    downloadResource = DownloadResource
              { topDownloads = resourceAt "/packages/top.:format"
              }

    getDownloadHistogram :: IO (Histogram PackageName)
    getDownloadHistogram = Cache.getCache downloadHistogram

    --totalDownloadCount :: MonadIO m => m Int
    --totalDownloadCount = liftM totalDownloads $ query GetDownloadCounts

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
    sortByDownloads nameFunc pkgs = query GetDownloadCounts >>= \counts -> do
        let modEntry pkg = (pkg, lookupPackageDowns (nameFunc pkg) downloadMap)
        return $ sortBy (comparing snd) $ map modEntry pkgs
    -}

    -- For at-a-glance download information.
    perVersionDownloads :: (MonadIO m, Package pkg) => pkg -> m (Int, Int)
    perVersionDownloads pkg = do
        info <- query $ GetDownloadInfo (packageName pkg)
        let (PackageDownloads total perVersion) = packageDownloads info
        return (total, Map.findWithDefault 0 (packageVersion pkg) perVersion)

