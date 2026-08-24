-- | Measure the cost of recording package downloads.
--
-- Downloads used to be written to the acid-state log one event per download.
-- They are now accumulated in memory and written one event per flush (see
-- "Distribution.Server.Features.DownloadCount"). Both paths still exist in
-- the code -- 'RegisterDownload' is retained so that old event logs can still
-- be replayed -- so we can measure the old and new behaviour from a single
-- binary, without checking out an older tree.
--
-- Usage:
--
-- > DownloadCountBench MODE [DOWNLOADS] [PACKAGES] [PER-FLUSH]
--
-- where MODE is
--
--   * @individual@: one acid-state event per download, plus the per-download
--     'RecordedToday' query the old code did. This is the pre-batching path.
--     It does not include the 'Control.Concurrent.Chan' write and thread
--     wake-up the old code also paid for each download, so the numbers here
--     understate the old cost slightly.
--
--   * @batched@: accumulate into a 'TVar' exactly as the download hook now
--     does, and write one 'RegisterDownloads' event per PER-FLUSH downloads.
--     At 50 downloads/second, the server's 60 second flush interval
--     corresponds to PER-FLUSH = 3000.
--
-- Run each mode in its own process, since the memory figures are process-wide:
--
-- > cabal run --enable-benchmarks DownloadCountBench -- individual
-- > cabal run --enable-benchmarks DownloadCountBench -- batched
module Main where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, swapTVar)
import Control.Monad (forM_, unless, when)
import Data.Acid (AcidState, closeAcidState, openLocalStateFrom, query, update)
import qualified Data.Map.Strict as Map
import Data.Time.Calendar (fromGregorian)
import Distribution.Package (PackageId, PackageIdentifier (..), mkPackageName)
import Distribution.Server.Features.DownloadCount.State
import Distribution.Server.Framework.MemSize (memSize, memSizeKb)
import Distribution.Server.Util.CountingMap (cmTotal)
import Distribution.Version (mkVersion)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import System.Directory
  ( doesDirectoryExist,
    getFileSize,
    getTemporaryDirectory,
    listDirectory,
    removePathForcibly,
  )
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.FilePath ((</>))
import System.Mem (performMajorGC)
import Text.Printf (printf)

data Mode = Individual | Batched
  deriving (Eq)

main :: IO ()
main = do
  args <- getArgs
  (mode, downloads, packages, perFlush) <- case args of
    (m : rest) -> do
      mode <- case m of
        "individual" -> pure Individual
        "batched" -> pure Batched
        _ -> badUsage
      let arg n def = case drop n rest of
            (x : _) -> read x
            [] -> def
      pure
        ( mode,
          arg 0 200000, -- total num downloads
          arg 1 3000, -- number of packages
          arg 2 200 -- batch size
        )
    _ -> badUsage

  statsEnabled <- getRTSStatsEnabled
  unless statsEnabled $
    die "Run with +RTS -T (the benchmark is built with -with-rtsopts=-T)"

  tmp <- getTemporaryDirectory
  let stateDir = tmp </> "hackage-download-count-bench"
  removePathForcibly stateDir

  printf
    "%s: %d downloads over %d distinct package versions\n"
    (modeName mode)
    (downloads :: Int)
    (packages :: Int)
  when (mode == Batched) $
    printf "  flushing every %d downloads\n" (perFlush :: Int)

  st <- openLocalStateFrom stateDir (initInMemStats today)
  let pkgids = downloadStream downloads packages

  -- The work being measured.
  elapsed <- timed $ case mode of
    Individual -> recordIndividually st pkgids
    Batched -> recordBatched st perFlush pkgids

  -- Check both modes actually recorded everything, so that we cannot
  -- accidentally compare a fast path that does less work.
  final <- query st GetInMemStats
  let recorded = cmTotal (inMemCounts final)
  unless (recorded == downloads) $
    die $
      "Recorded "
        ++ show recorded
        ++ " downloads, expected "
        ++ show downloads

  performMajorGC
  stats <- getRTSStats
  logBytes <- dirSize stateDir

  closeAcidState st

  -- Reopening replays every event written since the last checkpoint, so
  -- this is what the event log costs the server at start-up.
  replay <- timed $ do
    st' <- openLocalStateFrom stateDir (initInMemStats today)
    _ <- query st' GetInMemStats
    closeAcidState st'

  removePathForcibly stateDir

  printf "  time to record        %10.3f s\n" (seconds elapsed)
  printf "  event log on disk     %10d KB\n" (logBytes `div` 1024)
  printf "  start-up replay       %10.3f s\n" (seconds replay)
  printf
    "  allocated             %10d MB\n"
    (allocated_bytes stats `div` (1024 * 1024))
  printf
    "  peak live heap        %10d KB\n"
    (max_live_bytes stats `div` 1024)
  printf "  major GCs             %10d\n" (major_gcs stats)
  printf "  InMemStats memSize    %10d KB\n" (memSizeKb (memSize final))
  where
    today = fromGregorian 2026 8 24

    badUsage = do
      pname <- getProgName
      die $
        "usage: "
          ++ pname
          ++ " (individual|batched) [DOWNLOADS] [PACKAGES] [PER-FLUSH]"

    modeName Individual = "individual (one event per download)"
    modeName Batched = "batched (one event per flush)"

-- | The stream of downloads to record.
--
-- Spread uniformly over the given number of distinct package versions, which
-- is the least favourable case for batching: real download traffic is heavily
-- skewed towards a few packages, which collapses further within a flush.
downloadStream :: Int -> Int -> [PackageId]
downloadStream downloads packages =
  [pkgid ((i * 7919) `mod` packages) | i <- [1 .. downloads]]
  where
    pkgid n =
      PackageIdentifier
        (mkPackageName ("package-" ++ show n))
        (mkVersion [1, n `mod` 20])

-- | The pre-batching path: one 'RecordedToday' query and one
-- 'RegisterDownload' event per download.
recordIndividually :: AcidState InMemStats -> [PackageId] -> IO ()
recordIndividually st pkgids =
  forM_ pkgids $ \pkgid -> do
    _ <- query st RecordedToday
    update st (RegisterDownload pkgid)

-- | The current path: accumulate in a 'TVar', write one event per flush.
recordBatched :: AcidState InMemStats -> Int -> [PackageId] -> IO ()
recordBatched st perFlush pkgids = do
  acc <- newTVarIO Map.empty
  let go _ [] = flush acc
      go n (pkgid : rest) = do
        atomically $ modifyTVar' acc (Map.insertWith (+) pkgid 1)
        if n >= perFlush
          then flush acc >> go 1 rest
          else go (n + 1 :: Int) rest
  go 1 pkgids
  where
    flush acc = do
      counts <- atomically $ swapTVar acc Map.empty
      unless (Map.null counts) $
        update st (RegisterDownloads (Map.toList counts))

-- | Time an action, in nanoseconds.
timed :: IO () -> IO Word
timed action = do
  before <- getMonotonicTimeNSec
  action
  after <- getMonotonicTimeNSec
  pure (fromIntegral (after - before))

seconds :: Word -> Double
seconds ns = fromIntegral ns / 1e9

-- | Total size of every file under a directory.
dirSize :: FilePath -> IO Integer
dirSize dir = do
  isDir <- doesDirectoryExist dir
  if not isDir
    then pure 0
    else do
      entries <- listDirectory dir
      sum <$> mapM entrySize entries
  where
    entrySize entry = do
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      if isDir then dirSize path else getFileSize path
