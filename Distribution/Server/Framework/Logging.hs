module Distribution.Server.Framework.Logging (
    Verbosity,
    lognotice,
    loginfo,
    logdebug
  ) where

import Distribution.Verbosity
import System.IO
import qualified Data.ByteString.Char8 as BS
import System.Environment
import Control.Monad (when)

lognotice :: Verbosity -> String -> IO ()
lognotice verbosity msg =
  when (verbosity >= normal) $ do
    pname <- getProgName
    BS.hPutStrLn stdout (BS.pack $ pname ++ ": " ++ msg)
    hFlush stdout

loginfo :: Verbosity -> String -> IO ()
loginfo verboisty msg =
  when (verboisty >= verbose) $ do
    BS.hPutStrLn stderr (BS.pack msg)
    hFlush stderr

logdebug :: Verbosity -> String -> IO ()
logdebug verbosity msg =
  when (verbosity >= deafening) $ do
    BS.hPutStrLn stderr (BS.pack msg)
    hFlush stderr

