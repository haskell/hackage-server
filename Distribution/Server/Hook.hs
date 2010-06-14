module Distribution.Server.Hook (
    HookList,
    newHookList,
    registerHook,
    registerHooks,
    runHooks,
    runZeroHook,
    runOneHook,
    runTwoHook
  ) where

import Data.IORef
import Control.Monad

-- local IORef, nicer than MVar for this task
data HookList a = HookList (IORef [a])

newHookList :: IO (HookList a)
newHookList = fmap HookList $ newIORef []

registerHook :: HookList a -> a -> IO ()
registerHook (HookList list) hook = modifyIORef list (hook:)

registerHooks :: HookList a -> [a] -> IO ()
registerHooks hlist hooks = mapM_ (registerHook hlist) hooks

runHooks :: HookList a -> (a -> IO ()) -> IO ()
runHooks (HookList vlist) func = readIORef vlist >>= mapM_ func

-- boilerplate code, maybe replaceable by insane typeclass magic
runZeroHook :: HookList (IO ()) -> IO ()
runZeroHook list = runHooks list id

runOneHook :: HookList (a -> IO ()) -> a -> IO ()
runOneHook list a = runHooks list (\f -> f a)

runTwoHook :: HookList (a -> b -> IO ()) -> a -> b -> IO ()
runTwoHook list a b = runHooks list (\f -> f a b)
