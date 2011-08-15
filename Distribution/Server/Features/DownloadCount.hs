module Distribution.Server.Features.DownloadCount (
    DownloadFeature,
    downloadResource,
    DownloadResource(..),
    getDownloadHistogram,
    initDownloadFeature,
    perVersionDownloads,
    sortedPackages,
  ) where

import Distribution.Server.Acid (query, update)
import Distribution.Server.Framework
import Distribution.Server.Features.Core

import Distribution.Server.Packages.Downloads
import Distribution.Server.Util.Histogram
import qualified Distribution.Server.Framework.Cache as Cache

import Distribution.Package

import Data.Time.Clock
import Control.Arrow (second)
import Control.Monad (forever)
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Data.Function (fix)
--import Data.List (sortBy)
--import Data.Ord (comparing)
import qualified Data.Map as Map
import Control.Monad.Trans (MonadIO)

data DownloadFeature = DownloadFeature {
    downloadResource :: DownloadResource,
    downloadStream :: Chan PackageId,
    downloadHistogram :: Cache.Cache (Histogram PackageName)
}

data DownloadResource = DownloadResource {
    topDownloads :: Resource
}

instance IsHackageFeature DownloadFeature where
    getFeatureInterface download = (emptyHackageFeature "download") {
        featureResources = map (\x -> x $ downloadResource download) [topDownloads]
      , featurePostInit  = do countCache
                              forkIO transferDownloads >> return ()
      , featureDumpRestore = Nothing -- TODO
      }
      where countCache = do
                dc <- query GetDownloadCounts
                let dmap = map (second packageDowns) (Map.toList $ downloadMap dc)
                Cache.putCache (downloadHistogram download) (constructHistogram dmap)
            transferDownloads = forever $ do
                pkg <- readChan (downloadStream download)
                time <- getCurrentTime
                (_, new) <- update $ RegisterDownload (utctDay time) pkg 1
                Cache.modifyCache (downloadHistogram download)
                    (updateHistogram (packageName pkg) new)

initDownloadFeature :: ServerEnv -> CoreFeature -> IO DownloadFeature
initDownloadFeature _ core = do
    downChan <- newChan
    downHist <- Cache.newCacheable emptyHistogram
    registerHook (tarballDownload core) $ writeChan downChan
    return DownloadFeature
      { downloadResource = fix $ \_ -> DownloadResource
          { topDownloads = resourceAt "/packages/top.:format"
          }
      , downloadStream = downChan
      , downloadHistogram = downHist
      }

getDownloadHistogram :: DownloadFeature -> IO (Histogram PackageName)
getDownloadHistogram = Cache.getCache . downloadHistogram

--totalDownloadCount :: MonadIO m => m Int
--totalDownloadCount = liftM totalDownloads $ query GetDownloadCounts

-- sortedPackages and sortByDownloads both order packages by total downloads without exposing download data

-- A lazy list of the top packages, which can be filtered, taken from, etc.
-- Does not include packages with no downloads.
sortedPackages :: DownloadFeature -> IO [(PackageName, Int)]
sortedPackages downs = fmap topCounts $ Cache.getCache (downloadHistogram downs)

{-
-- Sorts a list of package-y items by their download count.
-- Use sortedPackages to get an entire list.
-- TODO: use the Histogram's sortByCounts for this
sortByDownloads :: MonadIO m => (a -> PackageName) -> [a] -> m [(a, Int)]
sortByDownloads nameFunc pkgs = query GetDownloadCounts >>= \counts -> do
    let downMap = downloadMap counts
        modEntry pkg = (pkg, maybe 0 packageDowns $ Map.lookup (nameFunc pkg) downMap)
    return $ sortBy (comparing snd) $ map modEntry pkgs
-}

-- For at-a-glance download information.
perVersionDownloads :: (MonadIO m, Package pkg) => pkg -> m (Int, Int)
perVersionDownloads pkg = do
    info <- query $ GetDownloadInfo (packageName pkg)
    let (PackageDownloads total perVersion) = packageDownloads info
    return (total, Map.findWithDefault 0 (packageVersion pkg) perVersion)

