module Distribution.Server.Cache (
    Cache,
    State(..),
    new,
    get,
    put,
  ) where

import qualified Distribution.Server.AsyncVar as AsyncVar
import Distribution.Server.AsyncVar (AsyncVar)

import HAppS.Server (Response(rsBody))

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
