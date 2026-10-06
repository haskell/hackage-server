module Main where

import Control.Monad (forM_, unless)
import Data.Acid (AcidState, query, update)
import Data.Acid.Memory (openMemoryState)
import qualified Data.Map.Strict as Map
import Data.Time.Calendar (fromGregorian)
import Distribution.Package (PackageIdentifier (..), mkPackageName)
import Distribution.Server.Features.DownloadCount.State
import Distribution.Server.Util.CountingMap (cmEmpty, cmFind, cmInsert, cmToList, cmTotal)
import Distribution.Version (mkVersion)
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.Exit (die)
import System.FilePath ((</>))

main :: IO ()
main = do
  -- Recording downloads one at a time and recording them as a batch must
  -- produce exactly the same statistics.
  oneAtATime <- withStats $ \st ->
    forM_ downloadStream $ \pkgid -> update st (RegisterDownload pkgid)
  batched <- withStats $ \st ->
    update st (RegisterDownloads (tally downloadStream))
  unless (oneAtATime == batched) $
    die $
      "Batched downloads do not match individual downloads:\n"
        ++ show (cmToList (inMemCounts oneAtATime))
        ++ "\nversus\n"
        ++ show (cmToList (inMemCounts batched))

  -- Sanity check the counts themselves, so that the test above cannot be
  -- satisfied by both paths being equally wrong.
  let counts = inMemCounts batched
  unless (cmTotal counts == length downloadStream) $
    die $
      "Wrong total: "
        ++ show (cmTotal counts)
        ++ " expected "
        ++ show (length downloadStream)
  forM_ (tally downloadStream) $ \(pkgid, n) ->
    unless (cmFind pkgid counts == n) $
      die $
        "Wrong count for "
          ++ show pkgid
          ++ ": "
          ++ show (cmFind pkgid counts)
          ++ " expected "
          ++ show n

  -- Splitting a batch across several updates must accumulate, not replace.
  split <- withStats $ \st -> do
    update st (RegisterDownloads (tally (take 3 downloadStream)))
    update st (RegisterDownloads (tally (drop 3 downloadStream)))
  unless (split == batched) $
    die "Successive batches do not accumulate"

  -- An empty batch must be a no-op, and in particular must not disturb the
  -- day the statistics are recorded against.
  empty <- withStats $ \st -> update st (RegisterDownloads [])
  unless (inMemCounts empty == inMemCounts (initInMemStats today)) $
    die "Empty batch changed the counts"
  unless (inMemToday batched == today) $
    die "Recording downloads changed the recorded day"

  -- Rolling a day over writes out only the packages that were downloaded
  -- that day. The result on disk must be the same as writing every package.
  checkIncrementalWrite

  putStrLn "OK"
  where
    withStats :: (AcidState InMemStats -> IO ()) -> IO InMemStats
    withStats action = do
      st <- openMemoryState (initInMemStats today)
      action st
      query st GetInMemStats

    today = fromGregorian 2026 8 24

    -- Deliberately interleaved and repeated, so that ordering differences
    -- between the two paths would show up.
    downloadStream =
      [ pkg "foo" [1, 0],
        pkg "bar" [2],
        pkg "foo" [1, 0],
        pkg "foo" [2, 1],
        pkg "bar" [2],
        pkg "foo" [1, 0]
      ]

    pkg name version =
      PackageIdentifier (mkPackageName name) (mkVersion version)

tally :: [PackageIdentifier] -> [(PackageIdentifier, Int)]
tally = Map.toList . Map.fromListWith (+) . map (,1)

checkIncrementalWrite :: IO ()
checkIncrementalWrite = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "hackage-download-count-test"
  removePathForcibly dir

  let yesterday = fromGregorian 2026 8 23
      history =
        fst $
          updateHistory
            ( statsFor
                yesterday
                [ pkg "foo" [1, 0],
                  pkg "bar" [2]
                ]
            )
            cmEmpty
  writeOnDiskStats dir history

  let today = fromGregorian 2026 8 24
      (history', changed) =
        updateHistory
          ( statsFor
              today
              [ pkg "foo" [1, 0],
                pkg "baz" [3]
              ]
          )
          history
      expectedChanged = [mkPackageName "baz", mkPackageName "foo"]
  unless (changed == expectedChanged) $
    die $
      "Expected " ++ show expectedChanged ++ " to change, got " ++ show changed
  writeOnDiskStatsFor dir changed history'

  reread <- readOnDiskStats dir
  unless (cmToList reread == cmToList history') $
    die $
      "On-disk statistics do not match after an incremental write:\n"
        ++ show (cmToList reread)
        ++ "\nversus\n"
        ++ show (cmToList history')
  removePathForcibly dir
  where
    statsFor day pkgids =
      InMemStats day (foldr (`cmInsert` 1) cmEmpty pkgids)

    pkg name version =
      PackageIdentifier (mkPackageName name) (mkVersion version)
