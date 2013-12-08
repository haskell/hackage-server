{-# LANGUAGE DeriveDataTypeable #-}
module Distribution.Client.Cron
  ( cron
  , Signal(..)
  , ReceivedSignal(..)
  , rethrowSignalsAsExceptions
  ) where

import Control.Monad (forM_)
import Control.Exception (Exception)
import Control.Concurrent (myThreadId, threadDelay, throwTo)
import System.Random (randomRIO)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToZonedTime)
import Data.Typeable (Typeable)

import qualified System.Posix.Signals as Posix

import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils hiding (warn)

data ReceivedSignal = ReceivedSignal Signal UTCTime
  deriving (Show, Typeable)

data Signal = SIGABRT
            | SIGINT
            | SIGQUIT
            | SIGTERM
  deriving (Show, Typeable)

instance Exception ReceivedSignal


-- | "Re"throw signals as exceptions to the invoking thread
rethrowSignalsAsExceptions :: [Signal] -> IO ()
rethrowSignalsAsExceptions signals = do
  tid <- myThreadId
  forM_ signals $ \s ->
    let handler = do
          time <- getCurrentTime
          throwTo tid (ReceivedSignal s time)
    in Posix.installHandler (toPosixSignal s) (Posix.Catch handler) Nothing

toPosixSignal :: Signal -> Posix.Signal
toPosixSignal SIGABRT = Posix.sigABRT
toPosixSignal SIGINT  = Posix.sigINT
toPosixSignal SIGQUIT = Posix.sigQUIT
toPosixSignal SIGTERM = Posix.sigTERM

-- | @cron verbosity interval act@ runs @act@ over and over with
-- the specified interval.
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
          "Next try will be in " ++ show minutes ++ " minutes, at "
       ++ formatTime defaultTimeLocale "%R %Z" (utcToZonedTime tz nextSync)
