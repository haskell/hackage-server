{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.DownloadCount (
    DownloadFeature(..),
    DownloadResource(..),
    initDownloadFeature,

    packageDowns,
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump

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
initDownloadFeature ServerEnv{serverStateDir, serverVerbosity = verbosity} core = do
    loginfo verbosity "Initialising download feature, start"
    downloadState <- downloadStateComponent serverStateDir
    downChan      <- newChan
    downHist      <- newMemStateWHNF emptyHistogram
    let feature   = downloadFeature core downloadState downChan downHist
    registerHook (packageDownloadHook core) (writeChan downChan)

    loginfo verbosity "Initialising download feature, end"
    return feature

downloadStateComponent :: FilePath -> IO (StateComponent DownloadCounts)
downloadStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "DownloadCounts") initialDownloadCounts
  return StateComponent {
      stateDesc    = "Download counts"
    , acidState    = st
    , getState     = query st GetDownloadCounts
    , putState     = update st . ReplacePackageDownloads
    , backupState  = \dc -> [csvToBackup ["downloads.csv"] $ downloadsToCSV dc]
    , restoreState = downloadsBackup
    , resetState   = downloadStateComponent
    }

downloadFeature :: CoreFeature
                -> StateComponent DownloadCounts
                -> Chan PackageId
                -> MemState (Histogram PackageName)
                -> DownloadFeature

downloadFeature CoreFeature{}
                downloadState downloadStream downloadHistogram
  = DownloadFeature{..}
  where
    downloadFeatureInterface = (emptyHackageFeature "download") {
        featureResources = map ($ downloadResource) [topDownloads]
      , featurePostInit  = do countCache
                              forkIO transferDownloads >> return ()
      , featureState     = [abstractStateComponent downloadState]
      , featureCaches    = [
            CacheComponent {
              cacheDesc       = "package download histogram",
              getCacheMemSize = memSize <$> readMemState downloadHistogram
            }
          ]
      }

    countCache = do
        dc <- queryState downloadState GetDownloadCounts
        let dmap = map (second packageDowns) (Map.toList $ downloadMap dc)
        writeMemState downloadHistogram (constructHistogram dmap)

    transferDownloads = forever $ do
        pkg <- readChan downloadStream
        time <- getCurrentTime
        (_, new) <- updateState downloadState $ RegisterDownload (utctDay time) pkg 1
        modifyMemState downloadHistogram
            (updateHistogram (packageName pkg) new)

    queryGetDownloadInfo :: MonadIO m => PackageName -> m DownloadInfo
    queryGetDownloadInfo name = queryState downloadState (GetDownloadInfo name)

    downloadResource = DownloadResource
              { topDownloads = resourceAt "/packages/top.:format"
              }

    getDownloadHistogram :: IO (Histogram PackageName)
    getDownloadHistogram = readMemState downloadHistogram

    --totalDownloadCount :: MonadIO m => m Int
    --totalDownloadCount = liftM totalDownloads $ query downloadState GetDownloadCounts

    -- sortedPackages and sortByDownloads both order packages by total downloads without exposing download data

    -- A lazy list of the top packages, which can be filtered, taken from, etc.
    -- Does not include packages with no downloads.
    sortedPackages :: IO [(PackageName, Int)]
    sortedPackages = fmap topCounts $ readMemState downloadHistogram

    {-
    -- Sorts a list of package-y items by their download count.
    -- Use sortedPackages to get an entire list.
    -- TODO: use the Histogram's sortByCounts for this
    sortByDownloads :: MonadIO m => (a -> PackageName) -> [a] -> m [(a, Int)]
    sortByDownloads nameFunc pkgs = queryState downloadState GetDownloadCounts >>= \counts -> do
        let modEntry pkg = (pkg, lookupPackageDowns (nameFunc pkg) downloadMap)
        return $ sortBy (comparing snd) $ map modEntry pkgs
    -}

    -- For at-a-glance download information.
    perVersionDownloads :: (MonadIO m, Package pkg) => pkg -> m (Int, Int)
    perVersionDownloads pkg = do
        info <- queryState downloadState $ GetDownloadInfo (packageName pkg)
        let (PackageDownloads total perVersion) = packageDownloads info
        return (total, Map.findWithDefault 0 (packageVersion pkg) perVersion)

