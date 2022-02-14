{-# LANGUAGE NamedFieldPuns #-}
module Distribution.Server.Framework.Cron (
    Cron,
    CronJob(..), JobFrequency(..),
    newCron,
    addCronJob,
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Time
import Data.Time.Calendar.Easter

import Distribution.Server.Framework.Logging


newtype Cron = Cron (MVar CronState)

data CronJob = CronJob {
                 cronJobName      :: String,
                 cronJobFrequency :: JobFrequency,
                 cronJobOneShot   :: Bool,
                 cronJobAction    :: IO ()
               }

data JobFrequency = DailyJobFrequency
                  | WeeklyJobFrequency
                    -- add more as needed
                  | TestJobFrequency NominalDiffTime

data CronState = CronState {
                   jobQueue    :: Map UTCTime [CronJob],
                   waitVar     :: MVar (),
                   timerThread :: ThreadId
                 }

newCron :: Verbosity -> IO Cron
newCron verbosity = do
    waitVar  <- newEmptyMVar
    cstate   <- armTimer CronState { jobQueue = Map.empty,
                                     waitVar,
                                     timerThread = undefined }
    stateVar <- newMVar cstate
    _        <- forkIO $ cronRunner verbosity waitVar stateVar
    return (Cron stateVar)


addCronJob :: Cron -> CronJob -> IO ()
addCronJob (Cron stateVar) job = addJobs stateVar [job]

------------------------------------------------------------------------------

-- The two actions that modify the cron state

addJobs :: MVar CronState -> [CronJob] -> IO ()
addJobs stateVar jobs =
    modifyMVar_ stateVar $ \cstate@CronState{jobQueue} -> do
      now <- getCurrentTime
      let cstate' = cstate { jobQueue = insertJobs now jobQueue jobs }
      disarmTimer cstate'
      armTimer    cstate'
  where
    insertJobs now =
      foldl' (\q job -> let jobtime = nextJobTime now (cronJobFrequency job)
                         in Map.insertWith (++) jobtime [job] q)

removeExpiredJobs :: MVar CronState -> IO [CronJob]
removeExpiredJobs stateVar =
  modifyMVar stateVar $ \cstate@CronState{jobQueue} -> do
    now  <- getCurrentTime
    let (expired, remaining) = splitMapLte now jobQueue
    return (cstate{jobQueue = remaining}, concat (Map.elems expired))

------------------------------------------------------------------------------

nextJobTime :: UTCTime -> JobFrequency -> UTCTime
nextJobTime now DailyJobFrequency  = now {
                                        utctDay     = addDays 1 (utctDay now),
                                        utctDayTime = 0
                                     }
nextJobTime now WeeklyJobFrequency = now {
                                        utctDay     = sundayAfter (utctDay now),
                                        utctDayTime = 0
                                     }
nextJobTime now (TestJobFrequency sec) = addUTCTime sec now

disarmTimer :: CronState -> IO ()
disarmTimer CronState{timerThread} =
    killThread timerThread

armTimer :: CronState -> IO CronState
armTimer cstate@CronState{waitVar, jobQueue} = do
    tid <- forkIO $
      case Map.minViewWithKey jobQueue of
        Nothing          -> return ()
        Just ((t, _), _) -> do
          threadDelayUntil t
          putMVar waitVar ()
    return cstate { timerThread = tid }

cronRunner :: Verbosity -> MVar () -> MVar CronState -> IO ()
cronRunner verbosity wait stateVar =
    forever $ do
      takeMVar wait
      jobs <- removeExpiredJobs stateVar
      execute jobs
      requeue jobs
  where
    execute = mapM_ (runSingleJob verbosity)
    requeue = addJobs stateVar . filter (not . cronJobOneShot)

runSingleJob :: Verbosity -> CronJob -> IO ()
runSingleJob verbosity job = do
    loginfo verbosity starting
    logTiming verbosity finished (cronJobAction job)
      `catch` failed
  where
    starting = "cron: running job '"  ++ cronJobName job ++ "'"
    finished = "cron: finished job '" ++ cronJobName job ++ "'"

    failed :: SomeException -> IO ()
    failed e = lognotice verbosity $
                 "cron: exception in job '" ++ cronJobName job
                  ++ "': " ++ show e

------------------------------------------------------------------------------

splitMapLte :: Ord k => k -> Map k a -> (Map k a, Map k a)
splitMapLte k m =
    case Map.splitLookup k m of
      (lt, Nothing, gt) -> (lt, gt)
      (lt, Just eq, gt) -> (Map.insert k eq lt, gt)

threadDelayUntil :: UTCTime -> IO ()
threadDelayUntil target = do
    now <- getCurrentTime
    let waittime = diffTimeInMicroseconds (diffUTCTime target now)
    when (waittime > 0) (threadDelayLong waittime)
  where
    diffTimeInMicroseconds = truncate . (* 1000000) . toRational

    -- Annoyingly, threadDelay takes an Int number of microseconds, so we cannot
    -- wait much longer than an hour. So have to wait repeatedly. Sigh.
    threadDelayLong :: Integer -> IO ()
    threadDelayLong microseconds
      | microseconds > hour = do
          threadDelay hour
          threadDelayLong (microseconds - hour)
      | otherwise =
          threadDelay (fromIntegral microseconds)

    hour :: Num a => a
    hour = 60 * 60 * 1000000

