module Distribution.Server.Util.AsyncVar (
    AsyncVar,
    new,
    read,
    write,
    modify
  ) where

import Control.Concurrent
import Control.Exception

import Prelude hiding (read)

data AsyncVar state = AsyncVar !(Chan (Either (state -> state) state)) !(MVar state)

new :: (state -> ()) -> state -> IO (AsyncVar state)
new force initial = do
    evaluate (force initial)
    inChan  <- newChan
    outVar <- newMVar initial
    let loop = do
        -- TODO: catch exceptions.. perhaps..
        next <- readChan inChan
        current <- takeMVar outVar
        let value = case next of
                Left func -> func current
                Right val -> val
        evaluate (force value)
        putMVar outVar value
        loop
    forkIO loop
    return (AsyncVar inChan outVar)

read :: AsyncVar state -> IO state
read (AsyncVar _ outVar) = readMVar outVar

write :: AsyncVar state -> state -> IO ()
write (AsyncVar inChan _) value = writeChan inChan (Right value)

modify :: AsyncVar state -> (state -> state) -> IO ()
modify (AsyncVar inChan _) func = writeChan inChan (Left func)

