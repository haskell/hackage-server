module Distribution.Server.Util.AsyncVar (
  AsyncVar,
  new,
  read,
  write,
  ) where

import Control.Concurrent
import Control.Exception

import Prelude hiding (read)

-- idea: partition the chan into Either (state -> state) state?
data AsyncVar state = AsyncVar !(Chan state) !(MVar state)

new :: (state -> ()) -> state -> IO (AsyncVar state)
new force initial = do
  evaluate (force initial)
  inChan  <- newChan
  outVar <- newMVar initial
  let loop = do
        value <- readChan inChan
        evaluate (force value) -- TODO: catch exceptions.. also accept 'poison' pills
        _ <- takeMVar outVar
        putMVar outVar value
        loop
  forkIO loop
  return (AsyncVar inChan outVar)

read :: AsyncVar state -> IO state
read (AsyncVar _ outVar) = readMVar outVar

write :: AsyncVar state -> state -> IO ()
write (AsyncVar inChan _) value = writeChan inChan value
