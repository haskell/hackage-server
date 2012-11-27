module Distribution.Server.Framework.MemState (
    MemState,
    newMemStateNF,
    newMemStateWHNF,
    readMemState,
    writeMemState,
    modifyMemState,
  ) where

import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad.Trans (MonadIO(liftIO))
import Control.DeepSeq (NFData, rnf)

-- | General-purpose in-memory ephemeral state.
data MemState a = MemState !(MVar a) (a -> ())

newMemStateWHNF :: a -> IO (MemState a)
newMemStateWHNF state = do
  var <- newMVar state
  return (MemState var (\x -> seq x ()))

newMemStateNF :: NFData a => a -> IO (MemState a)
newMemStateNF state = do
  var <- newMVar state
  return (MemState var rnf)

readMemState :: MonadIO m => MemState a -> m a
readMemState (MemState var _) = liftIO $ readMVar var

writeMemState :: MonadIO m => MemState a -> a -> m ()
writeMemState (MemState var force) x =
  liftIO $ modifyMVar_ var $ \_ -> do
    evaluate (force x)
    return x

modifyMemState :: MonadIO m => MemState a -> (a -> a) -> m ()
modifyMemState (MemState var force) f =
  liftIO $ modifyMVar_ var $ \x -> do
    let x' = f x
    evaluate (force x')
    return x'

