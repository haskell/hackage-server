module Distribution.Client.Cron
  ( cron
  ) where

import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToZonedTime)

import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils hiding (warn)

cron :: Verbosity -> Int -> (a -> IO a) -> (a -> IO ())
cron verbosity interval action x = do
    x' <- action x

    interval' <- pertabate interval
    logNextSyncMessage interval'
    wait interval'
    cron verbosity interval action x'

  where
    -- to stop all mirror clients hitting the server at exactly the same time
    -- we randomly adjust the wait time by +/- 10%
    pertabate i = let deviation = i `div` 10
                   in randomRIO (i + deviation, i - deviation)

    -- Annoyingly, threadDelay takes an Int number of microseconds, so we cannot
    -- wait much longer than an hour. So have to wait repeatedly. Sigh.
    wait minutes | minutes > 60 = do threadDelay (60 * 60 * 1000000)
                                     wait (minutes - 60)
                 | otherwise    = threadDelay (minutes * 60 * 1000000)

    logNextSyncMessage minutes = do
      now       <- getCurrentTime
      tz        <- getCurrentTimeZone
      let nextSync = addUTCTime (fromIntegral (60 * minutes)) now
      notice verbosity $
          "Next sync will be in " ++ show minutes ++ " minutes, at "
       ++ formatTime defaultTimeLocale "%R %Z" (utcToZonedTime tz nextSync)
