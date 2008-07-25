module Distribution.Server.AsyncVar (
  AsyncVar,
  new,
  read,
  write,
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Exception

import Prelude hiding (read)

data AsyncVar state = AsyncVar !(Chan state) !(MVar state)

new :: (state -> ()) -> state -> IO (AsyncVar state)
new force initial = do
  evaluate (force initial)
  inChan  <- newChan
  outVar <- newMVar initial
  let loop = do
        value <- readChan inChan
        evaluate (force value) -- TODO: catch exceptions
        takeMVar outVar
        putMVar outVar value
        loop
  forkIO loop
  return (AsyncVar inChan outVar)

read :: AsyncVar state -> IO state
read (AsyncVar _ outVar) = readMVar outVar

write :: AsyncVar state -> state -> IO ()
write (AsyncVar inChan _) value = writeChan inChan value
