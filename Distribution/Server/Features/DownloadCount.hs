{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
-- | Download counts
--
-- We keep only the most recent data in memory, and store everything else on
-- disk. Specifically, we keep only download data for one day in memory, and
-- update the on-disk data structures once per day. We do this lazily: we write
-- to disk on the first new data of a new day. (We don't export a generic
-- "add download count for this specific day" at all.)
module Distribution.Server.Features.DownloadCount (
    DownloadFeature(..)
  , DownloadResource(..)
  , initDownloadFeature
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump

import Distribution.Server.Features.DownloadCount.State
import Distribution.Server.Features.DownloadCount.Backup

import Distribution.Server.Features.Core

import Distribution.Package

import Data.Time.Calendar (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Data.Map (Map)

data DownloadFeature = DownloadFeature {
    downloadFeatureInterface :: HackageFeature
  , downloadResource         :: DownloadResource

    -- Although we keep detailed statistics on disk, the only statistics we
    -- keep in memory are the total download counts for each package. This
    -- is used in the HTML feature to sort the packages by download count and
    -- when displaying the basic information about a package, and it is used
    -- in the PackageList feature for similar purposes.
  , totalPackageDownloads :: MonadIO m => m (Map PackageName Int)
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
    totalsCache   <- initTotalsCache (onDiskPath serverStateDir)
    let feature   = downloadFeature core serverStateDir downloadState downChan totalsCache
    registerHook (packageDownloadHook core) (writeChan downChan)
    loginfo verbosity "Initialising download feature, end"
    return feature

inMemPath :: FilePath -> FilePath
inMemPath stateDir = stateDir </> "db" </> "InMemStats"

onDiskPath :: FilePath -> FilePath
onDiskPath stateDir = stateDir </> "db" </> "OnDiskStats"

downloadStateComponent :: FilePath -> IO (StateComponent AcidState InMemStats)
downloadStateComponent stateDir = do
  initSt <- initInMemStats <$> getToday
  st <- openLocalStateFrom (inMemPath stateDir) initSt
  return StateComponent {
      stateDesc    = "Download counts"
    , stateHandle  = st
    , getState     = query st GetInMemStats
    , putState     = update st . ReplaceInMemStats
--    , backupState  = \dc -> [csvToBackup ["downloads.csv"] $ downloadsToCSV dc]
--    , restoreState = downloadsBackup
    , resetState   = downloadStateComponent
    }

downloadFeature :: CoreFeature
                -> FilePath
                -> StateComponent AcidState InMemStats
                -> Chan PackageId
                -> MemState (Map PackageName Int)
                -> DownloadFeature

downloadFeature CoreFeature{}
                stateDir
                downloadState
                downloadStream
                totalsCache
  = DownloadFeature{..}
  where
    downloadFeatureInterface = (emptyHackageFeature "download") {
        featureResources = map ($ downloadResource) [topDownloads]
      , featurePostInit  = void $ forkIO registerDownloads
      , featureState     = [abstractAcidStateComponent downloadState]
      , featureCaches    = [
            CacheComponent {
              cacheDesc       = "total package downloads cache",
              getCacheMemSize = memSize <$> readMemState totalsCache
            }
          ]
      }

    totalPackageDownloads :: MonadIO m => m (Map PackageName Int)
    totalPackageDownloads = readMemState totalsCache

    registerDownloads = forever $ do
        pkg    <- readChan downloadStream
        today  <- getToday
        today' <- query (stateHandle downloadState) RecordedToday

        when (today /= today') $ do
          inMemStats <- getState downloadState
          putState downloadState (initInMemStats today)
          updateOnDiskStats (onDiskPath stateDir) inMemStats

        updateState downloadState $ RegisterDownload pkg
        updateTotalsCache totalsCache pkg

    downloadResource = DownloadResource {
        topDownloads = resourceAt "/packages/top.:format"
      }

{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

getToday :: IO Day
getToday = utctDay <$> getCurrentTime
