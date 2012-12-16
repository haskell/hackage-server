module Distribution.Server.Framework.Cache (
    AsyncCache,
    newAsyncCacheNF,
    newAsyncCacheWHNF,
    AsyncCachePolicy(..),
    defaultAsyncCachePolicy,
    readAsyncCache,
    prodAsyncCache,
    syncAsyncCache,
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans (MonadIO(liftIO))
import Control.DeepSeq (NFData, rnf)

import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Distribution.Server.Framework.Logging
import qualified Distribution.Verbosity as Verbosity


-- | An in-memory cache with asynchronous updates.
--
-- Cache reads never block but may get stale results. So it is suitable for
-- cases where updates may be expensive, but where having slightly out of
-- date results is ok.
--
-- The cache is made with the action that calculates the new value.
-- So cache updates just prod the cache to recalculate.
--
-- * Note: this action is executed synchronously, it is the evaluation of
--   the result of this action that is performed asynchronously. The cache
--   is only actually updated when the evaluation of this result is complete.
--
data AsyncCache a = AsyncCache !(AsyncVar a) (IO a)

data AsyncCachePolicy = AsyncCachePolicy {
       -- | To reduce the cost of updating caches when there are lots of
       -- updates going on, we simply wait, that way more updates queue up
       -- and since we only take the last update, then we save computation.
       asyncCacheUpdateDelay :: !Int,

       -- | Usually we want to force the initial value synchronously but some
       -- apps may prefer to let that happen asynchronously (and in parallel)
       -- to speed up app initialisation. But in that case the app has to use
       -- 'syncAsyncCache' to be sure the cache is ready.
       asyncCacheSyncInit    :: !Bool,

       asyncCacheLogVerbosity:: !Verbosity,
       asyncCacheName        :: String
     }

defaultAsyncCachePolicy :: AsyncCachePolicy
defaultAsyncCachePolicy = AsyncCachePolicy 0 True Verbosity.normal "(unnamed)"

newAsyncCacheWHNF :: IO a -> AsyncCachePolicy -> IO (AsyncCache a)
newAsyncCacheWHNF = newAsyncCache (\x -> seq x ())

newAsyncCacheNF :: NFData a => IO a -> AsyncCachePolicy -> IO (AsyncCache a)
newAsyncCacheNF = newAsyncCache rnf

newAsyncCache :: (a -> ()) -> IO a -> AsyncCachePolicy -> IO (AsyncCache a)
newAsyncCache eval update (AsyncCachePolicy delay syncForce verbosity logname) = do
  x <- update
  avar <- newAsyncVar delay syncForce verbosity logname eval x
  return (AsyncCache avar update)

readAsyncCache :: MonadIO m => AsyncCache a -> m a
readAsyncCache (AsyncCache avar _) = liftIO $ readAsyncVar avar

prodAsyncCache :: MonadIO m => AsyncCache a -> m ()
prodAsyncCache (AsyncCache avar update) = liftIO $ update >>= writeAsyncVar avar

-- | Only needed if you use asynchronous initialisation
-- (i.e. ''asyncCacheSyncInit' = False@). Waits until the value in the cache
-- has been evaluated. It has no effect later on since the async cache always
-- has a value available (albeit perhaps a stale one).
syncAsyncCache :: NFData a => AsyncCache a -> IO ()
syncAsyncCache c = readAsyncCache c >>= evaluate . rnf >> return ()


-------------------------------------------------
-- A mutable variable with asynchronous updates
--

data AsyncVar state = AsyncVar !(TChan state)
                               !(TVar (Either SomeException state))

newAsyncVar :: Int -> Bool -> Verbosity -> String
            -> (state -> ()) -> state -> IO (AsyncVar state)
newAsyncVar delay syncForce verbosity logname force initial = do

    inChan <- atomically newTChan
    outVar <- atomically (newTVar (Right initial))

    if syncForce then do t  <- getCurrentTime
                         evaluate (force initial)
                         t'  <- getCurrentTime
                         loginfo verbosity $ "Cache '" ++ logname ++ "' initialised. "
                                          ++ "time: " ++ show (diffUTCTime t' t)

                 else atomically (writeTChan inChan initial)

    let loop = do

          when (delay > 0) (threadDelay delay)

          avail   <- readAllAvailable inChan
          -- We have a series of new values.
          -- We want the last one, skipping all intermediate updates.
          let value = last avail

          t   <- getCurrentTime
          res <- try $ evaluate (force value `seq` value)
          atomically (writeTVar outVar res)
          t'  <- getCurrentTime

          loginfo verbosity $ "Cache '" ++ logname ++ "' updated. "
                           ++ "time: " ++ show (diffUTCTime t' t)

          loop

    void $ forkIO loop
    return (AsyncVar inChan outVar)
  where
    -- get a list of all the input states currently queued
    readAllAvailable chan =
      atomically $ do
        x <- readTChan chan -- will block if queue is empty
        readAll [x]         -- will never block, just gets what's available
      where
        readAll xs = do
          empty <- isEmptyTChan chan
          if empty
            then return (reverse xs)
            else do x <- readTChan chan
                    readAll (x:xs)

readAsyncVar :: AsyncVar state -> IO state
readAsyncVar (AsyncVar _ outVar) =
    atomically (readTVar outVar) >>= either throw return

writeAsyncVar :: AsyncVar state -> state -> IO ()
writeAsyncVar (AsyncVar inChan _) value =
    atomically (writeTChan inChan value)

