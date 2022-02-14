module Distribution.Server.Framework.Logging (
    Verbosity,
    lognotice,
    loginfo,
    logdebug,
    logTiming,
  ) where

import Prelude ()
import Distribution.Server.Prelude

import Distribution.Verbosity
import System.IO
import qualified Data.ByteString.Char8 as BS -- No UTF8 in log messages
import System.Environment
import Data.Time.Clock (getCurrentTime, diffUTCTime)

{-# INLINEABLE lognotice #-}
{-# SPECIALISE lognotice :: Verbosity -> String -> IO () #-}
lognotice :: MonadIO m => Verbosity -> String -> m ()
lognotice verbosity msg =
  when (verbosity >= normal) $ liftIO $ do
    pname <- getProgName
    BS.hPutStrLn stdout (BS.pack $ pname ++ ": " ++ msg)
    hFlush stdout

{-# INLINEABLE loginfo #-}
{-# SPECIALISE loginfo :: Verbosity -> String -> IO () #-}
loginfo :: MonadIO m => Verbosity -> String -> m ()
loginfo verbosity msg =
  when (verbosity >= verbose) $ liftIO $ do
    BS.hPutStrLn stderr (BS.pack msg)
    hFlush stderr

{-# INLINEABLE logdebug #-}
{-# SPECIALISE logdebug :: Verbosity -> String -> IO () #-}
logdebug :: MonadIO m => Verbosity -> String -> m ()
logdebug verbosity msg =
  when (verbosity >= deafening) $ liftIO $ do
    BS.hPutStrLn stderr (BS.pack msg)
    hFlush stderr

{-# INLINEABLE logTiming #-}
{-# SPECIALISE logTiming :: Verbosity -> String -> IO a -> IO a #-}
logTiming :: MonadIO m => Verbosity -> String -> m a -> m a
logTiming verbosity msg action = do
    t   <- liftIO getCurrentTime
    res <- action
    t'  <- liftIO getCurrentTime
    loginfo verbosity (msg ++ ". time: " ++ show (diffUTCTime t' t))
    return res
