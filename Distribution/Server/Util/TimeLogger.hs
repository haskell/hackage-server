module Distribution.Server.Util.TimeLogger where

import Control.Concurrent
import Happstack.Server
import Control.Monad.Trans (liftIO)
import System.IO
import Control.Monad (forever, when)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Happstack.Util.Concurrent as HappsLoad
import qualified Data.ByteString.Lazy as BS

-- Logging to determine the performance of various pages
setUpLog :: IO (Chan String)
setUpLog = do
    ch <- newChan
    HappsLoad.fork $ withFile "times" AppendMode $ \h -> do 
        hSetBuffering h LineBuffering
        forever $ hPutStrLn h =<< readChan ch
    return ch

-- An adaptor for impl in Distribution.Server. It evaluates the response
-- and returns it, adding a log entry in the progress. By "evaluate"
-- I mean that it forces the length of the response string, which
-- should at least also force the computation tree of the response.
timeLog :: Chan String -> ServerPart Response -> ServerPart Response
timeLog ch sres = do
    t <- liftIO $ getCurrentTime
    res <- sres
    case res of
        Response{} -> do
            when (rsfContentLength $ rsFlags res) $ do
                let resl = BS.length $ rsBody res
                t2 <- resl `seq` liftIO getCurrentTime
                uri <- fmap rqUri askRq
                let str = unwords [uri, show resl, show $ diffUTCTime t2 t]
                liftIO (writeChan ch str)
            return res
        _ -> return res


