{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Download counts
--
-- We maintain
--
-- 0. In-memory (cache): downloads that have arrived since the last flush.
--    Downloads are accumulated here and flushed into (1) as a single
--    transaction once per flush interval, so that a busy server does not write
--    one acid-state event per download. A hard crash loses at most one flush
--    interval worth of counts.
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
import Distribution.Server.Features.DownloadCount.Acid (acidStore)
import qualified Distribution.Server.Features.DownloadCount.Store as Store
import Distribution.Server.Features.Core
import Distribution.Server.Features.Users

import Distribution.Package
import Distribution.Server.Util.CountingMap (cmFromCSV, cmToList)

import Data.Time.Calendar (Day, addDays)
import Data.Time.Clock (getCurrentTime, utctDay)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, swapTVar)
import Control.Exception (SomeException, try)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import Data.List (sortBy)
import Data.Function (on)

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

data PackageDownloads = PackageDownloads {
    packageName :: !String
  , downloads   :: !Int
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (ToJSON)


initDownloadFeature :: ServerEnv
                    -> IO (CoreFeature -> UserFeature -> IO DownloadFeature)
initDownloadFeature serverEnv@ServerEnv{serverStateDir} = do
    inMemBackend   <- acidStore serverStateDir
    let onDiskState = onDiskStateComponent serverStateDir
    (recentDownloads,
     totalDownloads) <- computeRecentAndTotalDownloads =<< getState onDiskState
    recentCache    <- newMemStateWHNF recentDownloads
    totalsCache    <- newMemStateWHNF totalDownloads
    pendingDownloads <- newTVarIO Map.empty
    flushLock        <- newMVar ()

    return $ \core users -> do
      let feature = downloadFeature core users serverEnv inMemBackend
                      onDiskState totalsCache recentCache
                      pendingDownloads flushLock

      registerHook (packageDownloadHook core) $ \pkgid ->
        atomically $ modifyTVar' pendingDownloads (Map.insertWith (+) pkgid 1)
      return feature

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
                -> Store.Backend
                -> StateComponent OnDiskState OnDiskStats
                -> MemState TotalDownloads
                -> MemState RecentDownloads
                -> TVar (Map.Map PackageId Int)
                -> MVar ()
                -> DownloadFeature

downloadFeature CoreFeature{}
                UserFeature{..}
                ServerEnv{serverStateDir, serverVerbosity}
                inMemBackend
                onDiskState
                totalDownloadsCache
                recentDownloadsCache
                pendingDownloads
                flushLock
  = DownloadFeature{..}
  where
    inMemStore = Store.backendStore inMemBackend

    downloadFeatureInterface = (emptyHackageFeature "download") {
        featureResources = [ topDownloads downloadResource
                           , downloadCSV
                           ]
      , featurePostInit  = void $ forkIO flushDownloadsLoop
      , featureState     = Store.backendState inMemBackend
                        ++ [abstractOnDiskStateComponent onDiskState]
      , featurePreShutdown = shutdownFlush
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

    flushInterval :: Int
    flushInterval = 60 * 1_000_000 -- 60 seconds

    flushDownloadsLoop :: IO ()
    flushDownloadsLoop = forever $ do
        threadDelay flushInterval
        flushDownloadsSafe

    logErrors :: String -> IO () -> IO ()
    logErrors what action = do
        outcome <- try action
        case outcome of
          Right () -> return ()
          Left err -> lognotice serverVerbosity $
            what ++ ": " ++ show (err :: SomeException)

    flushDownloadsSafe :: IO ()
    flushDownloadsSafe =
        logErrors "Error recording download counts" flushDownloads

    shutdownFlush :: IO ()
    shutdownFlush = do
        flushDownloadsSafe
        logErrors "Error checkpointing download counts" $
          Store.checkpointInMemStats inMemStore

    flushDownloads :: IO ()
    flushDownloads = withMVar flushLock $ \() -> do
        checkDayRollover

        counts <- atomically $ swapTVar pendingDownloads Map.empty
        unless (Map.null counts) $
          Store.registerDownloads inMemStore (Map.toList counts)

    checkDayRollover :: IO ()
    checkDayRollover = do
        today  <- getToday
        today' <- Store.recordedToday inMemStore

        --TODO: this should be a daily cron job rather than being polled by the
        -- flush loop: the rollover does a lot of I/O (it rewrites the whole
        -- on-disk history) and it holds up the flush of the counts while it
        -- runs.
        when (today /= today') $ do
          -- For the first flush each day we reset the in-memory stats and..
          inMemStats <- Store.getInMemStats inMemStore
          Store.replaceInMemStats inMemStore $ initInMemStats today
          -- we can discard the large eventlog by writing a small checkpoint
          Store.checkpointInMemStats inMemStore

          -- Write yesterday's downloads to the log
          appendToLog (dcPath serverStateDir) inMemStats

          -- Update the on-disk statistics and recompute recent downloads.
          -- Only the packages downloaded yesterday need writing out.
          (onDiskStats',
           changedPkgs) <- updateHistory inMemStats <$> getState onDiskState
          writeOnDiskStatsFor (dcPath serverStateDir </> "ondisk")
                              changedPkgs onDiskStats'
          --TODO: we still recompute these from the whole history rather than
          -- updating them with yesterday's downloads
          (recentDownloads,
           totalDownloads) <- computeRecentAndTotalDownloads onDiskStats'
          writeMemState recentDownloadsCache recentDownloads
          writeMemState totalDownloadsCache totalDownloads


    downloadResource = DownloadResource {
      topDownloads = (resourceAt "/packages/top.:format")
        { resourceDesc = [ (GET, "Get top downloaded packages for the last 30 days")]
        , resourceGet  = [ ("json", serveDownloadTopJSON) ]
        }
      }

    serveDownloadTopJSON :: DynamicPath -> ServerPartE Response
    serveDownloadTopJSON _ = do
      pkgList <- sortedPackages <$> recentPackageDownloads
      pure $ toResponse $ Aeson.toJSON pkgList

    sortedPackages :: RecentDownloads -> [PackageDownloads]
    sortedPackages = fmap (\(p, c) -> PackageDownloads (unPackageName p) c) . sortBy (flip compare `on` snd) . cmToList

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
