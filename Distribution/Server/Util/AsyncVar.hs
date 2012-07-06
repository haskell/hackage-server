module Distribution.Server.Util.AsyncVar (
    AsyncVar,
    new,
    read,
    write,
    modify
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

import Prelude hiding (read, catch)

data AsyncVar state = AsyncVar !(TChan (Either (state -> state) state))
                               !(MVar state)

new :: (state -> ()) -> state -> IO (AsyncVar state)
new force initial = do
    () <- evaluate (force initial)
    parent <- myThreadId
    inChan <- atomically $ newTChan
    outVar <- newMVar initial
    let loop = do
        avail   <- readAllAvailable inChan
        current <- readMVar outVar
        -- we have a series of updates, either incremental or replacement
        let value = foldl accum current avail -- note: not foldl' on purpose!
            accum x (Left  f)  = f x
            accum _ (Right x') = x'
        res <- try $ evaluate (force value)
        case res of
          Left  e -> do throwTo parent (e :: SomeException)
                        loop
          Right _ -> do modifyMVar_ outVar (\_ -> return value)
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

read :: AsyncVar state -> IO state
read (AsyncVar _ outVar) = readMVar outVar

write :: AsyncVar state -> state -> IO ()
write (AsyncVar inChan _) value = atomically $ writeTChan inChan (Right value)

modify :: AsyncVar state -> (state -> state) -> IO ()
modify (AsyncVar inChan _) func = atomically $ writeTChan inChan (Left func)
