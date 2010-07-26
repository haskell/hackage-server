module Distribution.Server.Hook (
    Hook,
    newHook,
    registerHook,
    registerHooks,

    runHooks,
    runHook,
    runHook',
    runHook'',

    runFilters,
    runFilter,
    runFilter',
    runFilter''
  ) where

import Data.IORef
import Control.Monad.Trans (MonadIO, liftIO)

-- local IORef, nicer than MVar for this task
data Hook a = Hook (IORef [a])

newHook :: IO (Hook a)
newHook = fmap Hook $ newIORef []

registerHook :: Hook a -> a -> IO ()
registerHook (Hook list) hook = modifyIORef list (hook:)

registerHooks :: Hook a -> [a] -> IO ()
registerHooks hlist hooks = mapM_ (registerHook hlist) hooks

runHooks :: MonadIO m => Hook a -> (a -> IO b) -> m ()
runHooks (Hook vlist) func = liftIO $ readIORef vlist >>= mapM_ func

runFilters :: MonadIO m => Hook a -> (a -> IO b) -> m [b]
runFilters (Hook vlist) func = liftIO $ readIORef vlist >>= mapM func

-- boilerplate code, maybe replaceable by insane typeclass magic
runHook :: MonadIO m => Hook (IO ()) -> m ()
runHook list = runHooks list id

runHook' :: MonadIO m => Hook (a -> IO ()) -> a -> m ()
runHook' list a = runHooks list (\f -> f a)

runHook'' :: MonadIO m => Hook (a -> b -> IO ()) -> a -> b -> m ()
runHook'' list a b = runHooks list (\f -> f a b)

runFilter :: MonadIO m => Hook (IO a) -> m [a]
runFilter list = runFilters list id

runFilter' :: MonadIO m => Hook (a -> IO b) -> a -> m [b]
runFilter' list a = runFilters list (\f -> f a)

runFilter'' :: MonadIO m => Hook (a -> b -> IO c) -> a -> b -> m [c]
runFilter'' list a b = runFilters list (\f -> f a b)

