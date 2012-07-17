
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
import Data.Maybe
import Network.HTTP
import Network.HTTP.Auth
import Network.URI
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
runTests = do runUserTests
              runPackageTests

runUserTests :: IO ()
runUserTests = do
    do info "Getting user list"
       xs <- getUrlStrings "/users/"
       unless (xs == ["Hackage users","admin"]) $
           die ("Bad user list: " ++ show xs)
    do info "Getting admin user list"
       xs <- getUrlStrings "/users/admins/"
       unless (xs == ["Hackage admins","[","edit","]","admin"]) $
           die ("Bad admin user list: " ++ show xs)
    do info "Creating user"
       postToUrl "/users/" [("username", "testuser"),
                            ("password", "testpass"),
                            ("repeat-password", "testpass")]
    do info "Creating user2"
       postToUrl "/users/" [("username", "testuser2"),
                            ("password", "testpass2"),
                            ("repeat-password", "testpass2")]
    do info "Checking new users are now in user list"
       xs <- getUrlStrings "/users/"
       unless (xs == ["Hackage users","admin","testuser","testuser2"]) $
           die ("Bad user list: " ++ show xs)
    do info "Checking new users are not in admin list"
       xs <- getUrlStrings "/users/admins/"
       unless (xs == ["Hackage admins","[","edit","]","admin"]) $
           die ("Bad admin user list: " ++ show xs)
    do info "Getting password change page for testuser"
       void $ getAuthUrlStrings "testuser" "testpass" "/user/testuser/password"
    do info "Getting password change page for testuser as an admin"
       void $ getAuthUrlStrings "admin" "admin" "/user/testuser/password"
    do info "Getting password change page for testuser as another user"
       checkForbiddenUrl "testuser2" "testpass2" "/user/testuser/password"
    do info "Getting password change page for testuser with bad password"
       checkUnauthedUrl "testuser" "badpass" "/user/testuser/password"
    do info "Getting password change page for testuser with bad username"
       checkUnauthedUrl "baduser" "testpass" "/user/testuser/password"
    do info "Changing password for testuser2"
       void $ postAuthToUrl "testuser2" "testpass2" "/user/testuser2/password"
                  [("password", "newtestpass2"),
                   ("repeat-password", "newtestpass2"),
                   ("_method", "PUT")]
    do info "Checking password has changed"
       void $ getAuthUrlStrings "testuser2" "newtestpass2"
                                "/user/testuser2/password"
       checkUnauthedUrl "testuser2" "testpass2" "/user/testuser2/password"
    do info "Trying to delete testuser2 as testuser2"
       deleteUrlRes ((4, 0, 3) ==) "testuser2" "newtestpass2" "/user/testuser2"
       xs <- getUrlStrings "/users/"
       unless (xs == ["Hackage users","admin","testuser","testuser2"]) $
           die ("Bad user list: " ++ show xs)
    do info "Deleting testuser2 as admin"
       deleteUrlRes ((2, 0, 0) ==) "admin" "admin" "/user/testuser2"
       xs <- getUrlStrings "/users/"
       unless (xs == ["Hackage users","admin","testuser"]) $
           die ("Bad user list: " ++ show xs)
    do info "Getting user info for testuser"
       xs <- getUrlStrings "/user/testuser"
       unless (xs == ["testuser"]) $
           die ("Bad user info: " ++ show xs)

runPackageTests :: IO ()
runPackageTests = do
    do info "Getting package list"
       xs <- getUrlStrings "/packages/"
       unless (xs == ["Packages by category","Categories:","."]) $
           die ("Bad package list: " ++ show xs)

getUrlStrings :: String -> IO [String]
getUrlStrings url = getUrlStrings' (getRequest (mkUrl url))

getAuthUrlStrings :: String -> String -> String -> IO [String]
getAuthUrlStrings u p url
    = withAuth u p (getRequest (mkUrl url)) getUrlStrings'

checkForbiddenUrl :: String -> String -> String -> IO ()
checkForbiddenUrl u p url = withAuth u p (getRequest (mkUrl url)) $
                            checkReqGivesResponse ((4, 0, 3) ==)

checkUnauthedUrl :: String -> String -> String -> IO ()
checkUnauthedUrl u p url = withAuth u p (getRequest (mkUrl url)) $
                           checkReqGivesResponse ((4, 0, 1) ==)

deleteUrlRes :: ((Int, Int, Int) -> Bool) -> String -> String -> String
             -> IO ()
deleteUrlRes wantedCode u p url = case parseURI (mkUrl url) of
                                  Nothing -> die "Bad URL"
                                  Just uri ->
                                      withAuth u p (mkRequest DELETE uri) $
                                      checkReqGivesResponse wantedCode

withAuth :: String -> String -> Request_String -> (Request_String -> IO a)
         -> IO a
withAuth u p req f = do
    res <- simpleHTTP req
    case res of
        Left e ->
            die ("Request failed: " ++ show e)
        Right rsp
         | rspCode rsp == (4, 0, 1) ->
            do let uri = rqURI req
                   hdrs = retrieveHeaders HdrWWWAuthenticate rsp
                   challenges = catMaybes $ map (headerToChallenge uri) hdrs
               auth <- case challenges of
                       [] -> die "No challenges"
                       c : _ ->
                           case c of
                           ChalBasic r ->
                               return $ AuthBasic {
                                            auSite = uri,
                                            auRealm = r,
                                            auUsername = u,
                                            auPassword = p
                                        }
                           ChalDigest r d n o _stale a q ->
                               return $ AuthDigest {
                                            auRealm = r,
                                            auUsername = u,
                                            auPassword = p,
                                            auDomain = d,
                                            auNonce = n,
                                            auOpaque = o,
                                            auAlgorithm = a,
                                            auQop = q
                                        }
               let req' = insertHeader HdrAuthorization
                                       (withAuthority auth req)
                                       req
               f req'
         | otherwise ->
            badResponse rsp

-- This is a simple HTML screen-scraping function. It skips down to
-- a div with class "content", and then returns the list of strings
-- that are in the following HTML.
getUrlStrings' :: Request_String -> IO [String]
getUrlStrings' req
    = do res <- simpleHTTP req
         case res of
             Left e ->
                 die ("Request failed: " ++ show e)
             Right rsp
              | rspCode rsp == (2, 0, 0) ->
                 do let xs0 = lines $ rspBody rsp
                        contentStart = ("<div class=\"content\"" `isSuffixOf`)
                        xs1 = dropWhile (not . contentStart) xs0
                        trim = dropWhile isSpace . reverse
                             . dropWhile isSpace . reverse
                        tidy = filter (not . null) . map trim
                    case getStrings 1 $ unlines xs1 of
                        Nothing -> die "Bad HTML?"
                        Just strings -> return $ tidy strings
              | otherwise ->
                 badResponse rsp
    where isAngleBracket '<' = True
          isAngleBracket '>' = True
          isAngleBracket _   = False
          getStrings :: Integer -> String -> Maybe [String]
          getStrings 0 xs = case break isAngleBracket xs of
                            (_,    '>' : _)   -> Nothing
                            (pref, '<' : xs') -> fmap (pref :)
                                               $ getStrings 1 xs'
                            _                 -> Just [xs]
          getStrings n xs = case break isAngleBracket xs of
                            (_, '>' : xs') -> getStrings (n - 1) xs'
                            (_, '<' : xs') -> getStrings (n + 1) xs'
                            _              -> Nothing

mkPostReq :: String -> [(String, String)] -> Request_String
mkPostReq url vals = setRequestBody (postRequest (mkUrl url))
                                    ("application/x-www-form-urlencoded",
                                     urlEncodeVars vals)

postToUrl :: String -> [(String, String)] -> IO ()
postToUrl url vals = checkReqGivesResponse ((3, 0, 3) ==) $ mkPostReq url vals

postAuthToUrl :: String -> String -> String -> [(String, String)] -> IO ()
postAuthToUrl u p url vals
    = withAuth u p (mkPostReq url vals) (checkReqGivesResponse goodCode)
    where goodCode (3, 0, 3) = True
          goodCode (2, 0, 0) = True
          goodCode _         = False

checkReqGivesResponse :: ((Int, Int, Int) -> Bool) -> Request_String -> IO ()
checkReqGivesResponse wantedCode req
    = do res <- simpleHTTP req
         case res of
             Left e ->
                 die ("Request failed: " ++ show e)
             Right rsp
              | wantedCode (rspCode rsp) ->
                 return ()
              | otherwise ->
                 badResponse rsp

mkUrl :: String -> String
mkUrl relPath = "http://127.0.0.1:" ++ show testPort ++ relPath

badResponse :: Response String -> IO a
badResponse rsp = die ("Bad response code: " ++ show (rspCode rsp) ++ "\n\n"
                    ++ show rsp)

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
                           do when (n == 0) $ die "Server didn't come up"
                              info "Server not up yet; will try again shortly"
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

die :: String -> IO a
die err = do hPutStrLn stderr err
             exitFailure

