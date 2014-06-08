{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
-- | Download counts
--
-- We maintain
--
-- 1. In-memory (ACID): today's download counts per package version
--
-- 2. In-memory (cache): total download count over the last 30 days per package
--    (across all versions). This is computed once per day from the on-disk
--    statistics (3).
--
-- 3. On-disk: total download per package per version per day. These are stored
--    in safe-copy format, one file per package; this allows to quickly load
--    the statistics for a given package to compute custom reports.
--
-- 4. On-disk: total download per package per version per day, stored as a single
--    CSV file that we append (1) to once per day. Strictly speaking this is
--    redundant, as this information is also stored in (3).
module Distribution.Server.Features.DownloadCount (
    DownloadFeature(..)
  , DownloadResource(..)
  , initDownloadFeature
  , RecentDownloads
  , TotalDownloads
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

import Distribution.Server.Features.DownloadCount.State
import Distribution.Server.Features.DownloadCount.Backup
import Distribution.Server.Features.Core
import Distribution.Server.Features.Users

import Distribution.Package
import Distribution.Server.Util.CountingMap (cmFromCSV)

import Data.Time.Calendar (Day, addDays)
import Data.Time.Clock (getCurrentTime, utctDay)
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)

data DownloadFeature = DownloadFeature {
    downloadFeatureInterface :: HackageFeature
  , downloadResource         :: DownloadResource
  , totalPackageDownloads   :: MonadIO m => m TotalDownloads
  , recentPackageDownloads   :: MonadIO m => m RecentDownloads
  }

instance IsHackageFeature DownloadFeature where
    getFeatureInterface = downloadFeatureInterface

data DownloadResource = DownloadResource {
    topDownloads :: Resource
  }

initDownloadFeature :: ServerEnv -> CoreFeature -> UserFeature -> IO DownloadFeature
initDownloadFeature serverEnv@ServerEnv{serverStateDir, serverVerbosity = verbosity} core users = do
    loginfo verbosity "Initialising download feature, start"

    inMemState     <- inMemStateComponent  serverStateDir
    let onDiskState = onDiskStateComponent serverStateDir
    recentCache    <- newMemStateWHNF =<< computeRecentDownloads =<< getState onDiskState
    totalsCache    <- newMemStateWHNF =<< computeTotalDownloads =<< getState onDiskState
    downChan       <- newChan

    let feature   = downloadFeature core users serverEnv inMemState
                    onDiskState totalsCache recentCache downChan

    registerHook (packageDownloadHook core) (writeChan downChan)
    loginfo verbosity "Initialising download feature, end"
    return feature

inMemStateComponent :: FilePath -> IO (StateComponent AcidState InMemStats)
inMemStateComponent stateDir = do
  initSt <- initInMemStats <$> getToday
  st <- openLocalStateFrom (dcPath stateDir </> "inmem") initSt
  return StateComponent {
      stateDesc    = "Today's download counts"
    , stateHandle  = st
    , getState     = query st GetInMemStats
    , putState     = update st . ReplaceInMemStats
    , backupState  = \_ -> inMemBackup
    , restoreState = inMemRestore
    , resetState   = inMemStateComponent
    }

onDiskStateComponent :: FilePath -> StateComponent OnDiskState OnDiskStats
onDiskStateComponent stateDir = StateComponent {
      stateDesc    = "All time download counts"
    , stateHandle  = OnDiskState
    , getState     = readOnDiskStats (dcPath stateDir </> "ondisk")
    , putState     = writeOnDisk stateDir Nothing Nothing ReconstructLog
    , backupState  = \_ -> onDiskBackup
    , restoreState = onDiskRestore
    , resetState   = return . onDiskStateComponent
    }

data ReconstructLog = ReconstructLog | DontReconstructLog

writeOnDisk :: FilePath
            -> Maybe (MemState TotalDownloads)
            -> Maybe (MemState RecentDownloads)
            -> ReconstructLog
            -> OnDiskStats
            -> IO ()
writeOnDisk stateDir mTotalDownloads mRecentDownloads
  shouldReconstructLog onDiskStats = do
  writeOnDiskStats (dcPath stateDir </> "ondisk") onDiskStats

  case mTotalDownloads of
    Just totalDownloads ->
      writeMemState totalDownloads =<< computeTotalDownloads onDiskStats
    Nothing ->
      return ()

  case mRecentDownloads of
    Just recentDownloads ->
      writeMemState recentDownloads =<< computeRecentDownloads onDiskStats
    Nothing ->
      return ()

  case shouldReconstructLog of
    ReconstructLog ->
      reconstructLog (dcPath stateDir) onDiskStats
    DontReconstructLog ->
      return ()

downloadFeature :: CoreFeature
                -> UserFeature
                -> ServerEnv
                -> StateComponent AcidState   InMemStats
                -> StateComponent OnDiskState OnDiskStats
                -> MemState TotalDownloads
                -> MemState RecentDownloads
                -> Chan PackageId
                -> DownloadFeature

downloadFeature CoreFeature{}
                UserFeature{..}
                ServerEnv{serverStateDir}
                inMemState
                onDiskState
                totalDownloadsCache
                recentDownloadsCache
                downloadStream
  = DownloadFeature{..}
  where
    downloadFeatureInterface = (emptyHackageFeature "download") {
        featureResources = [ topDownloads downloadResource
                           , downloadCSV
                           ]
      , featurePostInit  = void $ forkIO registerDownloads
      , featureState     = [ abstractAcidStateComponent   inMemState
                           , abstractOnDiskStateComponent onDiskState
                           ]
      , featureCaches    = [
            CacheComponent {
              cacheDesc       = "recent package downloads cache",
              getCacheMemSize = memSize <$> readMemState recentDownloadsCache
            },
            CacheComponent {
              cacheDesc       = "total package downloads cache",
              getCacheMemSize = memSize <$> readMemState totalDownloadsCache
            }
          ]
      }

    recentPackageDownloads :: MonadIO m => m RecentDownloads
    recentPackageDownloads = readMemState recentDownloadsCache

    totalPackageDownloads :: MonadIO m => m TotalDownloads
    totalPackageDownloads = readMemState totalDownloadsCache

    registerDownloads = forever $ do
        pkg    <- readChan downloadStream
        today  <- getToday
        today' <- query (stateHandle inMemState) RecordedToday

        when (today /= today') $ do
          -- For the first download each day we reset the in-memory stats and..
          inMemStats <- getState inMemState
          putState inMemState $ initInMemStats today

          -- Write yesterday's downloads to the log
          appendToLog (dcPath serverStateDir) inMemStats

          -- Update the on-disk statistics and recompute recent downloads
          onDiskStats' <- updateHistory inMemStats <$> getState onDiskState
          writeOnDisk serverStateDir (Just totalDownloadsCache)
            (Just recentDownloadsCache) DontReconstructLog onDiskStats'

        updateState inMemState $ RegisterDownload pkg

    downloadResource = DownloadResource {
        topDownloads = resourceAt "/packages/top.:format"
      }

    downloadCSV = (resourceAt "/packages/downloads.:format") {
        resourceDesc = [ (GET, "Get download counts")
                       , (PUT, "Upload download counts (for import)")
                       ]
      , resourceGet  = [ ("csv", getDownloadCounts) ]
      , resourcePut  = [ ("csv", putDownloadCounts) ]
      }

    getDownloadCounts :: DynamicPath -> ServerPartE Response
    getDownloadCounts _path = do
      onDiskStats <- liftIO $ getState onDiskState
      let [BackupByteString _ bs] = onDiskBackup onDiskStats
      return $ toResponse bs

    putDownloadCounts :: DynamicPath -> ServerPartE Response
    putDownloadCounts _path = do
      guardAuthorised_ [InGroup adminGroup]
      fileContents <- expectCSV
      csv          <- importCSV "PUT input" fileContents
      onDiskStats  <- cmFromCSV csv
      liftIO $ writeOnDisk serverStateDir (Just totalDownloadsCache)
        (Just recentDownloadsCache) ReconstructLog onDiskStats
      ok $ toResponse $ "Imported " ++ show (length csv) ++ " records\n"

{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

getToday :: IO Day
getToday = utctDay <$> getCurrentTime

getRecentDayRange :: Integer -> IO DayRange
getRecentDayRange numDays = do
  lastDay <- getToday
  let firstDay  = addDays (negate numDays) lastDay
      secondDay = addDays 1 firstDay
  return [firstDay, secondDay .. lastDay]

computeRecentDownloads :: OnDiskStats -> IO RecentDownloads
computeRecentDownloads onDiskStats = do
  recentRange <- getRecentDayRange 30
  return $ initRecentDownloads recentRange onDiskStats

computeTotalDownloads :: OnDiskStats -> IO TotalDownloads
computeTotalDownloads onDiskStats = return $ initTotalDownloads onDiskStats

dcPath :: FilePath -> FilePath
dcPath stateDir = stateDir </> "db" </> "DownloadCount"
