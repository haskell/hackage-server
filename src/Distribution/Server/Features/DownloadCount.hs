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
  , totalPackageDownloads    :: forall m. MonadIO m => m TotalDownloads
  , recentPackageDownloads   :: forall m. MonadIO m => m RecentDownloads
  }

instance IsHackageFeature DownloadFeature where
    getFeatureInterface = downloadFeatureInterface

data DownloadResource = DownloadResource {
    topDownloads :: Resource
  }

initDownloadFeature :: ServerEnv
                    -> IO (CoreFeature -> UserFeature -> IO DownloadFeature)
initDownloadFeature serverEnv@ServerEnv{serverStateDir} = do
    inMemState     <- inMemStateComponent  serverStateDir
    let onDiskState = onDiskStateComponent serverStateDir
    (recentDownloads,
     totalDownloads) <- computeRecentAndTotalDownloads =<< getState onDiskState
    recentCache    <- newMemStateWHNF recentDownloads
    totalsCache    <- newMemStateWHNF totalDownloads
    downChan       <- newChan

    return $ \core users -> do
      let feature = downloadFeature core users serverEnv inMemState
                      onDiskState totalsCache recentCache downChan

      registerHook (packageDownloadHook core) (writeChan downChan)
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
    , putState     = \onDiskStats -> do
                       --TODO: we should extend the backup system so we can
                       -- write these files out incrementally
                       writeOnDiskStats (dcPath stateDir </> "ondisk") onDiskStats
                       reconstructLog (dcPath stateDir) onDiskStats
    , backupState  = \_ -> onDiskBackup
    , restoreState = onDiskRestore
    , resetState   = return . onDiskStateComponent
    }

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

        --TODO: do this asyncronously rather than blocking this request
        when (today /= today') $ do
          -- For the first download each day we reset the in-memory stats and..
          inMemStats <- getState inMemState
          putState inMemState $ initInMemStats today
          -- we can discard the large eventlog by writing a small checkpoint
          createCheckpoint (stateHandle inMemState)

          -- Write yesterday's downloads to the log
          appendToLog (dcPath serverStateDir) inMemStats

          -- Update the on-disk statistics and recompute recent downloads
          onDiskStats' <- updateHistory inMemStats <$> getState onDiskState
          writeOnDiskStats (dcPath serverStateDir </> "ondisk") onDiskStats'
          --TODO: this is still stupid, writing it out only to read it back
          -- we should be able to update the in memory ones incrementally
          (recentDownloads,
           totalDownloads) <- computeRecentAndTotalDownloads =<< getState onDiskState
          writeMemState recentDownloadsCache recentDownloads
          writeMemState totalDownloadsCache totalDownloads


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
      guardAuthorised_ [InGroup adminGroup]
      onDiskStats <- liftIO $ getState onDiskState
      let [BackupByteString _ bs] = onDiskBackup onDiskStats
      return $ toResponse bs

    putDownloadCounts :: DynamicPath -> ServerPartE Response
    putDownloadCounts _path = do
      guardAuthorised_ [InGroup adminGroup]
      fileContents <- expectCSV
      csv          <- importCSV "PUT input" fileContents
      onDiskStats  <- cmFromCSV csv
      liftIO $ do
        --TODO: if the onDiskStats are large, can we stream it?
        writeOnDiskStats (dcPath serverStateDir </> "ondisk") onDiskStats
        (recentDownloads,
         totalDownloads) <- computeRecentAndTotalDownloads onDiskStats
        writeMemState recentDownloadsCache recentDownloads
        writeMemState totalDownloadsCache totalDownloads
        reconstructLog (dcPath serverStateDir) onDiskStats

      ok $ toResponse $ "Imported " ++ show (length csv) ++ " records\n"

{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

getToday :: IO Day
getToday = utctDay <$> getCurrentTime

getRecentDayRange :: Integer -> IO (Day, Day)
getRecentDayRange numDays = do
  lastDay <- getToday
  let firstDay = addDays (negate numDays) lastDay
  return (firstDay, lastDay)

computeRecentAndTotalDownloads :: OnDiskStats -> IO (RecentDownloads, TotalDownloads)
computeRecentAndTotalDownloads onDiskStats = do
  recentRange <- getRecentDayRange 30
  return $ initRecentAndTotalDownloads recentRange onDiskStats

dcPath :: FilePath -> FilePath
dcPath stateDir = stateDir </> "db" </> "DownloadCount"
