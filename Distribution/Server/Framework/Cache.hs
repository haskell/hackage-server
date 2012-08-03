module Distribution.Server.Framework.Cache (
    Cache(..),
    newCache,
    newCacheable,
    getCache,
    putCache,
    modifyCache,
    respondCache,

    CacheableAction,
    newCacheableAction,
    getCacheableAction,
    refreshCacheableAction,
  ) where

import qualified Distribution.Server.Util.AsyncVar as AsyncVar
import Distribution.Server.Util.AsyncVar (AsyncVar)

import Happstack.Server

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans (MonadIO(liftIO))
import Control.DeepSeq

-- | A general-purpose in-memory cache.
newtype Cache a = Cache { cacheState :: AsyncVar a }

newCache :: a -> (a -> b) -> IO (Cache a)
newCache state forceFunc = Cache `fmap` AsyncVar.new (\a -> forceFunc a `seq` ()) state

-- How necessary is it to use deepseq to fully evaluate the cache? Too low-level?
newCacheable :: NFData a => a -> IO (Cache a)
newCacheable emptyValue = newCache emptyValue rnf

getCache :: MonadIO m => Cache a -> m a
getCache (Cache avar) = liftIO $ AsyncVar.read avar

putCache :: MonadIO m => Cache a -> a -> m ()
putCache (Cache avar) state = liftIO $ AsyncVar.write avar state

modifyCache :: MonadIO m => Cache a -> (a -> a) -> m ()
modifyCache (Cache avar) func = liftIO $ AsyncVar.modify avar func

-----------------------------------------------------------------------
-- usually b = DynamicPath. This saves on code nodes (elsewhere) and imports (here)
respondCache :: ToMessage r => Cache a -> (a -> r) -> b -> ServerPart Response
respondCache cache func _ = return . toResponse . func =<< getCache cache

-----------------------------------------------------------------------
data CacheableAction a = CachedAction {
                             ca_current :: MVar a,
                             ca_needsUpdate :: TVar Bool
                         }

newCacheableAction :: NFData a => IO a -> IO (CacheableAction a)
newCacheableAction action
    = do parent <- myThreadId
         -- We compute the initial value in the current thread.
         -- This means that by the time anyone tries to read the value,
         -- one has definitely been successfully computed.
         -- If we let the worker thread compute the first value then
         -- we'd somehow have to handle the initial computation raising
         -- an exception.
         let compute = do val <- action
                          evaluate $ force val
         val <- compute
         current <- newMVar val
         needsUpdateVar <- newTVarIO False
         void $ forkIO $ forever $
             do atomically $ do needsUpdate <- readTVar needsUpdateVar
                                if needsUpdate
                                    then writeTVar needsUpdateVar False
                                    else retry
                res <- try compute
                -- TODO: Handle asynchronous exceptions better
                -- (same in Distribution.Server.Util.AsyncVar.new)
                case res of
                    Left  e -> throwTo parent (e :: SomeException)
                    Right v -> modifyMVar_ current $ const $ return v
         return $ CachedAction {
                      ca_current = current,
                      ca_needsUpdate = needsUpdateVar
                  }

getCacheableAction :: MonadIO m => CacheableAction a -> m a
getCacheableAction ca = liftIO $ readMVar $ ca_current ca

refreshCacheableAction :: MonadIO m => CacheableAction a -> m ()
refreshCacheableAction ca
    = liftIO $ atomically $ writeTVar (ca_needsUpdate ca) True

