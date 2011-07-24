module Distribution.Server.Framework.Hook (
    Hook,
    Filter,
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

-- | A list of hooks, usually IO actions.
--
-- A local IORef is nicer than MVar for this task, although TVar might be even nicer
data Hook a = Hook (IORef [a])

-- another name for Hook, used when the result is important
type Filter a = Hook a

newHook :: IO (Hook a)
newHook = fmap Hook $ newIORef []

-- registers a hook to be run *before* all of the previously registered hooks.
-- is this the best strategy? relying on ordering rules of any kind can introduce
-- nasty bugs.
registerHook :: Hook a -> a -> IO ()
registerHook (Hook list) hook = modifyIORef list (hook:)

registerHooks :: Hook a -> [a] -> IO ()
registerHooks hlist hooks = mapM_ (registerHook hlist) hooks

-- TODO: catch errors
runHooks :: MonadIO m => Hook a -> (a -> IO b) -> m ()
runHooks (Hook vlist) func = liftIO $ readIORef vlist >>= mapM_ func

runFilters :: MonadIO m => Filter a -> (a -> IO b) -> m [b]
runFilters (Hook vlist) func = liftIO $ readIORef vlist >>= mapM func

-- boilerplate code, maybe replaceable by insane typeclass magic
runHook :: MonadIO m => Hook (IO ()) -> m ()
runHook list = runHooks list id

runHook' :: MonadIO m => Hook (a -> IO ()) -> a -> m ()
runHook' list a = runHooks list (\f -> f a)

runHook'' :: MonadIO m => Hook (a -> b -> IO ()) -> a -> b -> m ()
runHook'' list a b = runHooks list (\f -> f a b)

runFilter :: MonadIO m => Filter (IO a) -> m [a]
runFilter list = runFilters list id

runFilter' :: MonadIO m => Filter (a -> IO b) -> a -> m [b]
runFilter' list a = runFilters list (\f -> f a)

runFilter'' :: MonadIO m => Filter (a -> b -> IO c) -> a -> b -> m [c]
runFilter'' list a b = runFilters list (\f -> f a b)

