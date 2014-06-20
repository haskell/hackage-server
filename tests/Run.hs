
module Run (run) where

import Control.Concurrent
import Control.Exception as Exception
import Control.Monad
import System.Exit
import System.IO
import System.IO.Error
import System.Posix

run :: FilePath -> [String] -> IO (Maybe ExitCode)
run cmd args = do
        ei <- tryIOError $ do pid <- systemSession cmd args
                              return pid
        case ei of
            Left _ -> return Nothing
            Right pid ->
                do mv <- newEmptyMVar
                   void $ forkIO $ do r <- getProcessStatus True False pid
                                      putMVar mv r
                   r <- takeMVar mv
                   case r of
                     Nothing -> do
                           putStrLn "getProcessStatus Nothing, so killing"
                           killProcessGroup pid
                           return Nothing
                     Just (Exited ec)    -> return (Just ec)
                     Just (Terminated _ _) -> return Nothing
                     Just _              -> return Nothing
                `Exception.catch` \e ->
                    do putStrLn ("Got " ++ show (e :: SomeException) ++ ", so killing")
                       killProcessGroup pid
                       return Nothing

systemSession :: FilePath -> [String] -> IO ProcessID
systemSession cmd args =
 forkProcess $ do
   void createSession
   executeFile cmd False args Nothing
   -- need to use exec() directly here, rather than something like
   -- System.Process.system, because we are in a forked child and some
   -- pthread libraries get all upset if you start doing certain
   -- things in a forked child of a pthread process, such as forking
   -- more threads.

killProcessGroup :: ProcessID -> IO ()
killProcessGroup pid = do
  ignoreIOExceptions (signalProcessGroup sigTERM pid)
  checkReallyDead 10
  where
    checkReallyDead :: Integer -> IO ()
    checkReallyDead 0 = hPutStrLn stderr "checkReallyDead: Giving up"
    checkReallyDead n =
      do threadDelay (3*100000) -- 3/10 sec
         m <- tryJust (guard . isDoesNotExistError) $
                 getProcessStatus False False pid
         case m of
            Right Nothing -> return ()
            Left _ -> return ()
            _ -> do
              ignoreIOExceptions (signalProcessGroup sigKILL pid)
              checkReallyDead (n - 1)

ignoreIOExceptions :: IO () -> IO ()
ignoreIOExceptions io = io `catchIOError` ((\_ -> return ()))

