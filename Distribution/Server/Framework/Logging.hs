module Distribution.Server.Framework.Logging (
    Verbosity,
    lognotice,
    loginfo,
    logdebug,
    logTiming,
  ) where

import Distribution.Verbosity
import System.IO
import qualified Data.ByteString.Char8 as BS -- No UTF8 in log messages
import System.Environment
import Control.Monad (when)
import Data.Time.Clock (getCurrentTime, diffUTCTime)


lognotice :: Verbosity -> String -> IO ()
lognotice verbosity msg =
  when (verbosity >= normal) $ do
    pname <- getProgName
    BS.hPutStrLn stdout (BS.pack $ pname ++ ": " ++ msg)
    hFlush stdout

loginfo :: Verbosity -> String -> IO ()
loginfo verbosity msg =
  when (verbosity >= verbose) $ do
    BS.hPutStrLn stderr (BS.pack msg)
    hFlush stderr

logdebug :: Verbosity -> String -> IO ()
logdebug verbosity msg =
  when (verbosity >= deafening) $ do
    BS.hPutStrLn stderr (BS.pack msg)
    hFlush stderr

logTiming :: Verbosity -> String -> IO a -> IO a
logTiming verbosity msg action = do
    t   <- getCurrentTime
    res <- action
    t'  <- getCurrentTime
    loginfo verbosity (msg ++ ". time: " ++ show (diffUTCTime t' t))
    return res

