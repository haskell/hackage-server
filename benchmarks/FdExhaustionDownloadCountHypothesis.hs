-- | This benchmark is unusual in that it's only useful to run with a
-- very low limit on how many file descriptors it can have open at
-- once. So run it in a way similar to this:
--
-- @
-- $ cabal build FdExhaustion-DownloadCount-Hypothesis && (ulimit -n 44 && cabal run FdExhaustion-DownloadCount-Hypothesis)
-- @
module Main where

import Control.Exception (evaluate)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Data.SafeCopy (safePut)
import Data.Serialize.Put (runPutLazy)

import Distribution.Package (PackageName)
import Distribution.Simple.Utils (writeFileAtomic)
import Distribution.Text (display, simpleParse)

import Distribution.Server.Features.DownloadCount.State
import Distribution.Server.Util.CountingMap

main :: IO ()
main = do
    withSystemTempDirectory "hackage-server-FdExhaustion-test" $ \dir -> do
        -- Write 100 files, each containing 'fakePerPkg'.
        sequence_
            [ writeFileAtomic (dir </> display (nm :: PackageName)) $
                runPutLazy $ safePut fakePerPkg
            | Just nm <- [ simpleParse ("foo" ++ show i) | i <- [1..100 :: Int] ]
            ]
        
        -- The first invocation of 'registerDownloads' on each day
        -- does some bookkeeping of the download counts that are
        -- stored on disk.
        --
        -- This silly logic below this comment is distillation of that
        -- that also hammers the file descriptor acquire and release.
        readOnDiskStatsLazily dir >>= writeOnDiskStats dir
        readOnDiskStatsLazily dir >>= evaluate . initRecentAndTotalDownloads (toEnum 0, toEnum 30)
        pure ()

-- | The resulting file is 413,433 bytes.
--
-- That's enough bytes that the results of BSL.hGetContents will not
-- immediately reach the eof. (The chunksize is rarely above 32 KB.)
--
-- We create a fake on-disk state where every package coincidentally
-- has thi equivalent (incredibly dense 'OnDiskPerPkg').
--
-- TODO if this were /smaller/ this benchmark might need more file
-- descriptors at once? Depends on the exact operational dynamics of
-- 'unsafeInterleaveIO', the context switching of the RTS
-- capabilities, etc. Hard to anticipate /before/ we have a repro.
fakePerPkg :: OnDiskPerPkg
fakePerPkg =
    foldl' (\acc k -> cmInsert k 1 acc) cmEmpty keys
  where
    keys =
      [ (toEnum day, version)
      | day <- [0..100], Just version <- map (simpleParse . show) [0..100 :: Int]
      ]
