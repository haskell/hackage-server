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
    packagesPage :: Response,
    indexTarball :: ByteString
  }

force :: State -> ()
force state =
  let a = ByteString.length (rsBody $ packagesPage state)
      b = ByteString.length (indexTarball state)
   in a `seq` b `seq` () --FIXME: do in parallel

new :: State -> IO Cache
new state = Cache `fmap` AsyncVar.new force state

get :: MonadIO m => Cache -> m State
get (Cache avar) = liftIO $ AsyncVar.read avar

put :: Cache -> State -> IO ()
put (Cache avar) state = AsyncVar.write avar state
