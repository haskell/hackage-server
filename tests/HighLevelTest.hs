
{-
This is a very high-level test of the hackage server. It forks a fresh
server instance, and then uses HTTP to run various requests on that
server.
-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Network.HTTP
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error

import Run

-- A random port, that hopefully won't clash with anything else
testPort :: Int
testPort = 8392

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          info "Initialising"
          root <- getCurrentDirectory
          let testDir = "tests/HighLevelTestTemp"
          info "Setting up test directory"
          exists <- doesDirectoryExist testDir
          when exists $ removeDirectoryRecursive testDir
          createDirectory testDir
          (setCurrentDirectory testDir >> doit root)
              `finally` removeDirectoryRecursive (root </> testDir)

doit :: FilePath -> IO ()
doit root
    = do mec <- runServer root ["init"]
         case mec of
             Just ExitSuccess -> return ()
             _ -> do info "init failed"
                     exitFailure
         info "Forking server thread"
         mv <- newEmptyMVar
         bracket (forkIO (do info "Server thread started"
                             void $ runServer root ["run",
                                                    "--ip", "127.0.0.1",
                                                    "--port", show testPort]
                          `finally` putMVar mv ()))
                 (\t -> do killThread t
                           takeMVar mv)
                 (\_ -> do waitForServer
                           info "Server running"
                           runTests
                           info "Finished")

runTests :: IO ()
runTests = do do info "Getting user list"
                 xs <- getUrlStrings "/users/"
                 unless (xs == ["Hackage users","admin"]) $
                     error ("Bad user list: " ++ show xs)
              do info "Getting admin user list"
                 xs <- getUrlStrings "/users/admins/"
                 unless (xs == ["Hackage admins","[","edit","]","admin"]) $
                     error ("Bad admin user list: " ++ show xs)
              do info "Creating user"
                 postToUrl "/users/" [("username", "testuser"),
                                      ("password", "testpass"),
                                      ("repeat-password", "testpass")]
              do info "Getting user list"
                 xs <- getUrlStrings "/users/"
                 unless (xs == ["Hackage users","admin","testuser"]) $
                     error ("Bad user list: " ++ show xs)

-- This is a simple HTML screen-scraping function. It skips down to
-- a div with class "content", and then returns the list of strings
-- that are in the following HTML.
getUrlStrings :: String -> IO [String]
getUrlStrings url
    = do res <- simpleHTTP (getRequest (mkUrl url))
         case res of
             Left e ->
                 error ("Request failed: " ++ show e)
             Right rsp
              | rspCode rsp == (2, 0, 0) ->
                 do let xs0 = lines $ rspBody rsp
                        xs1 = dropWhile (not . ("<div class=\"content\"" `isSuffixOf`)) xs0
                        trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
                    return $ filter (not . null) $ map trim $ getStrings 1 $ unlines xs1
              | otherwise ->
                 error ("Bad response code: " ++ show (rspCode rsp))
    where isAngleBracket '<' = True
          isAngleBracket '>' = True
          isAngleBracket _   = False
          getStrings :: Integer -> String -> [String]
          getStrings 0 xs = case break isAngleBracket xs of
                            (_,    '>' : _)   -> error "Bad HTML?"
                            (pref, '<' : xs') -> pref : getStrings 1 xs'
                            _                 -> [xs]
          getStrings n xs = case break isAngleBracket xs of
                            (_, '>' : xs') -> getStrings (n - 1) xs'
                            (_, '<' : xs') -> getStrings (n + 1) xs'
                            _              -> error "Bad HTML?"

postToUrl :: String -> [(String, String)] -> IO ()
postToUrl url vals
    = do let req = setRequestBody (postRequest (mkUrl url))
                                  ("application/x-www-form-urlencoded",
                                   urlEncodeVars vals)
         res <- simpleHTTP req
         case res of
             Left e ->
                 error ("Request failed: " ++ show e)
             Right rsp
              | rspCode rsp == (3, 0, 3) ->
                 return ()
              | otherwise ->
                 error ("Bad response code: " ++ show (rspCode rsp))

mkUrl :: String -> String
mkUrl path = "http://127.0.0.1:" ++ show testPort ++ path

waitForServer :: IO ()
waitForServer = f 10
    where f :: Int -> IO ()
          f n = do info "Making a request to see if server is up"
                   res <- tryIOError $ simpleHTTP (getRequest (mkUrl "/"))
                   case res of
                       Right (Right rsp)
                        | rspCode rsp == (2, 0, 0) ->
                           info "Server is up"
                       _ ->
                           if n == 0
                           then error "Server didn't come up"
                           else do info "Server not up yet; will try again shortly"
                                   info ("(result was " ++ show res ++ ")")
                                   threadDelay 100000
                                   f (n - 1)

runServer :: FilePath -> [String] -> IO (Maybe ExitCode)
runServer root args = run server args'
    where server = root </> "dist/build/hackage-server/hackage-server"
          args' = ("--static-dir=" ++ root </> "static/")
                : args

info :: String -> IO ()
info str = putStrLn ("= " ++ str)

