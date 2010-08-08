module Distribution.Server.Util.TimeLogger where

import Control.Concurrent
import Happstack.Server
import Control.Monad.Trans (liftIO)
import System.IO
import Control.Monad (forever)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.ByteString.Lazy as BS

-- Logging to determine the performance of various pages
setUpLog :: IO (Chan String)
setUpLog = do
    ch <- newChan
    forkIO $ withFile "times" AppendMode $ \h -> do 
        hSetBuffering h LineBuffering
        forever $ hPutStrLn h =<< readChan ch
    return ch

timeLog :: Chan String -> ServerPart Response -> ServerPart Response
timeLog ch sres = do
    t <- liftIO $ getCurrentTime
    res <- sres
    case res of
        Response{} -> do
            t2 <- liftIO $ getCurrentTime
            uri <- fmap rqUri askRq
            let resl = BS.length $ rsBody res
                str = unwords [uri, show resl, show $ diffUTCTime t2 t]
            liftIO $ writeChan ch str
            liftIO $ putStrLn str
            return res
        _ -> return res

