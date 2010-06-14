module Distribution.Server.Cache (
    Cache,
    State(..),
    new,
    get,
    put,
    GenCache(..),
    newCache,
    newCacheable,
    getCache,
    putCache,
    Cacheable(..),
    respondCache
  ) where

import qualified Distribution.Server.Util.AsyncVar as AsyncVar
import Distribution.Server.Util.AsyncVar (AsyncVar)

import Happstack.Server (ServerPart, Response(rsBody), ToMessage, toResponse, result, ok)

import qualified Data.ByteString.Lazy as ByteString
import Data.ByteString.Lazy (ByteString)
import Control.Monad.Trans (MonadIO(liftIO))

newtype Cache = Cache (AsyncVar State)

data State = State {
    packagesPage :: Response,    -- Rendered HTML page
    indexTarball :: ByteString,  -- GZipped tarball
    recentChanges :: Response,   -- Rendered HTML page
    packagesFeed  :: Response    -- Rendered RSS feed
  }

force :: State -> ()
force state =
  let a = ByteString.length (rsBody $ packagesPage state)
      b = ByteString.length (indexTarball state)
      c = ByteString.length (rsBody $ recentChanges state)
      d = ByteString.length (rsBody $ packagesFeed state)
   in foldr seq () [a,b,c,d] --FIXME: do in parallel

new :: State -> IO Cache
new state = Cache `fmap` AsyncVar.new force state

get :: MonadIO m => Cache -> m State
get (Cache avar) = liftIO $ AsyncVar.read avar

put :: Cache -> State -> IO ()
put (Cache avar) state = AsyncVar.write avar state

-------------------------- this should replace Cache eventually
-- though it's an extremely loose wrapper around AsyncVar, so why not use that directly?
newtype GenCache a = GenCache {
    cacheState :: AsyncVar a
}

newCache :: a -> (a -> b) -> IO (GenCache a)
newCache state force = GenCache `fmap` AsyncVar.new (\a -> force a `seq` ()) state

newCacheable :: Cacheable a => IO (GenCache a)
newCacheable = newCache emptyValue forceValue

getCache :: MonadIO m => GenCache a -> m a
getCache (GenCache avar) = liftIO $ AsyncVar.read avar

putCache :: MonadIO m => GenCache a -> a -> m ()
putCache (GenCache avar) state = liftIO $ AsyncVar.write avar state

-----------------------------------------------------------------------
class Cacheable x where
    forceValue :: x -> ()  --aka NFData, but less rigorous
    emptyValue :: x

instance Cacheable Response where
    forceValue res = ByteString.length (rsBody res) `seq` ()
    emptyValue = result 404 ""

instance Cacheable ByteString where
    forceValue res = ByteString.length res `seq` ()
    emptyValue = ByteString.empty

instance (Cacheable a, Cacheable b) => Cacheable (a, b) where
    forceValue (a, b) = forceValue a `seq` forceValue b
    emptyValue = (emptyValue, emptyValue)

-- usually b = Config and c = DynamicPath. This saves on code nodes (elsewhere) and imports (here)
respondCache :: ToMessage r => GenCache a -> (a -> r) -> b -> c -> ServerPart Response
respondCache cache func _ _ = return . toResponse . func =<< getCache cache

