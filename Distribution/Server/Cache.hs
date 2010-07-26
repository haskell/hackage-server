module Distribution.Server.Cache (
    Cache(..),
    newCache,
    newCacheable,
    getCache,
    putCache,
    respondCache
  ) where

import qualified Distribution.Server.Util.AsyncVar as AsyncVar
import Distribution.Server.Util.AsyncVar (AsyncVar)

import Happstack.Server

import Control.Monad.Trans (MonadIO(liftIO))
import Control.Parallel.Strategies

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)

-------------------------- this should replace Cache eventually
-- though it's an extremely loose wrapper around AsyncVar, so why not use that directly?
newtype Cache a = Cache {
    cacheState :: AsyncVar a
}

newCache :: a -> (a -> b) -> IO (Cache a)
newCache state forceFunc = Cache `fmap` AsyncVar.new (\a -> forceFunc a `seq` ()) state

newCacheable :: NFData a => a -> IO (Cache a)
newCacheable emptyValue = newCache emptyValue rnf

getCache :: MonadIO m => Cache a -> m a
getCache (Cache avar) = liftIO $ AsyncVar.read avar

putCache :: MonadIO m => Cache a -> a -> m ()
putCache (Cache avar) state = liftIO $ AsyncVar.write avar state

-----------------------------------------------------------------------
-- usually b = DynamicPath. This saves on code nodes (elsewhere) and imports (here)
respondCache :: ToMessage r => Cache a -> (a -> r) -> b -> ServerPart Response
respondCache cache func _ = return . toResponse . func =<< getCache cache

-- rough versions of RNF for these
instance NFData ByteString where
    rnf bs = BS.length bs `seq` ()

instance NFData Response where
    rnf = rnf . rsBody --ByteString

