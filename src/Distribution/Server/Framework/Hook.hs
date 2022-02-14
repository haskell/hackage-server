module Distribution.Server.Framework.Hook (
    Hook,
    newHook,
    registerHook,
    registerHookJust,

    runHook,
    runHook_,
  ) where

import Data.IORef
import Control.Monad.Trans (MonadIO, liftIO)

-- | A list of actions accociated with an event.
--
newtype Hook a b = Hook (IORef [a -> IO b])

newHook :: IO (Hook a b)
newHook = fmap Hook $ newIORef []

-- registers a hook to be run *before* all of the previously registered hooks.
-- is this the best strategy? relying on ordering rules of any kind can introduce
-- nasty bugs.
registerHook :: Hook a b -> (a -> IO b) -> IO ()
registerHook (Hook ref) action =
  atomicModifyIORef ref (\actions -> (action:actions, ()))

registerHookJust :: Hook a () -> (a -> Maybe b) -> (b -> IO ()) -> IO ()
registerHookJust (Hook ref) predicate action =
    atomicModifyIORef ref (\actions -> (action':actions, ()))
  where
    action' x = maybe (return ()) action (predicate x)

runHook :: MonadIO m => Hook a b -> a -> m [b]
runHook (Hook ref) x = liftIO $ do
  actions <- readIORef ref
  sequence [ action x | action <- actions ]

runHook_ :: MonadIO m => Hook a () -> a -> m ()
runHook_ (Hook ref) x = liftIO $ do
  actions <- readIORef ref
  sequence_ [ action x | action <- actions ]

