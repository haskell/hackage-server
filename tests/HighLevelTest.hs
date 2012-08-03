
{-
This is a very high-level test of the hackage server. It forks a fresh
server instance, and then uses HTTP to run various requests on that
server.
-}

module Main (main) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy.Char8 as LBS
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

import Package
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
    = do info "initialising hackage database"
         runServerChecked True root ["init"]
         withServerRunning root $ do runUserTests
                                     runPackageUploadTests
                                     runPackageTests
         withServerRunning root $ runPackageTests
         info "Making database backup"
         runServerChecked False root ["backup", "-o", "hlb.tar"]
         info "Removing old state"
         removeDirectoryRecursive "state"
         info "Checking server doesn't work"
         mec <- runServer True root serverRunningArgs
         case mec of
             Just (ExitFailure 1) -> return ()
             Just (ExitFailure _) -> die "Server failed with wrong exit code"
             Just ExitSuccess     -> die "Server worked unexpectedly"
             Nothing              -> die "Got a signal?"
         info "Restoring database"
         runServerChecked False root ["restore", "hlb.tar"]
         info "Making another database backup"
         runServerChecked False root ["backup", "-o", "hlb2.tar"]
         info "Checking databases match"
         db1 <- LBS.readFile "hlb.tar"
         db2 <- LBS.readFile "hlb2.tar"
         unless (db1 == db2) $ die "Databases don't match"
         info "Checking server still works, and data is intact"
         withServerRunning root $ runPackageTests

withServerRunning :: FilePath -> IO () -> IO ()
withServerRunning root f
    = do info "Forking server thread"
         mv <- newEmptyMVar
         bracket (forkIO (do info "Server thread started"
                             void $ runServer True root serverRunningArgs
                          `finally` putMVar mv ()))
                 (\t -> do killThread t
                           takeMVar mv
                           info "Server terminated")
                 (\_ -> do waitForServer
                           info "Server running"
                           f
                           info "Finished with server")

serverRunningArgs :: [String]
serverRunningArgs = ["run", "--disable-caches", "--ip", "127.0.0.1", "--port", show testPort]

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

runPackageUploadTests :: IO ()
runPackageUploadTests = do
    do info "Getting package list"
       xs <- getUrlStrings "/packages/"
       unless (xs == ["Packages by category","Categories:","."]) $
           die ("Bad package list: " ++ show xs)
    do info "Trying to upload testpackage"
       void $ postFileAuthToUrlRes ((4, 0, 3) ==)
                  "testuser" "testpass" "/packages/" "package"
                  (testpackageTarFilename, testpackageTarFileContent)
    do info "Adding testuser to uploaders"
       void $ postAuthToUrl "admin" "admin" "/packages/uploaders/"
                  [("user", "testuser")]
    do info "Uploading testpackage"
       void $ postFileAuthToUrlRes ((2, 0, 0) ==)
                  "testuser" "testpass" "/packages/" "package"
                  (testpackageTarFilename, testpackageTarFileContent)
    where (testpackageTarFilename, testpackageTarFileContent, _, _, _, _)
              = testpackage

runPackageTests :: IO ()
runPackageTests = do
    do info "Getting package list"
       xs <- getUrlStrings "/packages/"
       unless (xs == ["Packages by category",
                      "Categories:","MyCategory","&nbsp;(1).",
                      "MyCategory",
                      "testpackage","library: test package testpackage"]) $
           die ("Bad package list: " ++ show xs)
    do info "Getting package index"
       targz <- getUrl "/packages/index.tar.gz"
       let tar = GZip.decompress $ LBS.pack targz
           entries = Tar.foldEntries (:) [] (error . show) $ Tar.read tar
           entryFilenames = map Tar.entryPath    entries
           entryContents  = map Tar.entryContent entries
       unless (entryFilenames == [testpackageCabalIndexFilename]) $
           die ("Bad index filenames: " ++ show entryFilenames)
       case entryContents of
           [Tar.NormalFile bs _]
            | LBS.unpack bs == testpackageCabalFile ->
               return ()
           _ ->
               die "Bad index contents"
    do info "Getting testpackage info"
       xs <- getUrlStrings "/package/testpackage"
       unless (take 1 xs == ["The testpackage package"]) $
           die ("Bad package info: " ++ show xs)
    do info "Getting testpackage-1.0.0.0 info"
       xs <- getUrlStrings "/package/testpackage-1.0.0.0"
       unless (take 1 xs == ["The testpackage package"]) $
           die ("Bad package info: " ++ show xs)
    do info "Getting testpackage Cabal file"
       cabalFile <- getUrl "/package/testpackage-1.0.0.0/testpackage.cabal"
       unless (cabalFile == testpackageCabalFile) $
           die "Bad Cabal file"
    do info "Getting testpackage tar file"
       tarFile <- getUrl "/package/testpackage/testpackage-1.0.0.0.tar.gz"
       unless (tarFile == testpackageTarFileContent) $
           die "Bad tar file"
    do info "Getting testpackage source"
       hsFile <- getUrl ("/package/testpackage/src"
                     </> testpackageHaskellFilename)
       unless (hsFile == testpackageHaskellFileContent) $
           die "Bad Haskell file"
    do info "Getting testpackage maintainer info"
       xs <- getUrlStrings "/package/testpackage/maintainers/"
       unless (xs == ["Maintainers for", "testpackage",
                      "Maintainers for a package can upload new versions and adjust other attributes in the package database.",
                      "[","edit","]",
                      "testuser"]) $
           die "Bad maintainers list"
    where (_,                             testpackageTarFileContent,
           testpackageCabalIndexFilename, testpackageCabalFile,
           testpackageHaskellFilename,    testpackageHaskellFileContent)
              = testpackage

testpackage :: (FilePath, String, FilePath, String, FilePath, String)
testpackage = mkPackage "testpackage"

getUrl :: String -> IO String
getUrl url = getReq (getRequest (mkUrl url))

getUrlStrings :: String -> IO [String]
getUrlStrings url = getReqStrings (getRequest (mkUrl url))

getAuthUrlStrings :: String -> String -> String -> IO [String]
getAuthUrlStrings u p url
    = withAuth u p (getRequest (mkUrl url)) getReqStrings

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
getReqStrings :: Request_String -> IO [String]
getReqStrings req
    = do body <- getReq req
         let xs0 = lines body
             contentStart = ("<div id=\"content\"" `isSuffixOf`)
             xs1 = dropWhile (not . contentStart) xs0
             trim = dropWhile isSpace . reverse
                  . dropWhile isSpace . reverse
             tidy = filter (not . null) . map trim
         case getStrings 1 $ unlines xs1 of
             Nothing -> die ("Bad HTML?\n\n" ++ body)
             Just strings -> return $ tidy strings
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

getReq :: Request_String -> IO String
getReq req
    = do res <- simpleHTTP req
         case res of
             Left e ->
                 die ("Request failed: " ++ show e)
             Right rsp
              | rspCode rsp == (2, 0, 0) ->
                 return $ rspBody rsp
              | otherwise ->
                 badResponse rsp

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

postFileAuthToUrlRes :: ((Int, Int, Int) -> Bool)
                     -> String -> String -> String -> String
                     -> (FilePath, String)
                     -> IO ()
postFileAuthToUrlRes wantedCode u p url field file
    = postFileAuthToReqRes wantedCode u p (postRequest (mkUrl url)) field file

postFileAuthToReqRes :: ((Int, Int, Int) -> Bool)
                     -> String -> String -> Request_String -> String
                     -> (FilePath, String)
                     -> IO ()
postFileAuthToReqRes wantedCode u p req field (filename, fileContents)
    = let boundary = "--BOUNDARY"
          req' = setRequestBody req
                                ("multipart/form-data; boundary=" ++ boundary,
                                 body)
          unlines' = concat . map (++ "\r\n")
          body = unlines' ["--" ++ boundary,
                           "Content-Disposition: form-data; name=" ++ show field ++ "; filename=" ++ show filename,
                           "Content-Type: application/gzip",
                           -- Base64 encoding avoids any possibility of
                           -- the boundary clashing with the file data
                           "Content-Transfer-Encoding: base64",
                           "",
                           BS.unpack $ Base64.encode $ BS.pack fileContents,
                           "--" ++ boundary ++ "--",
                           ""]

      in withAuth u p req' (checkReqGivesResponse wantedCode)

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
                    ++ show rsp ++ "\n\n"
                    ++ rspBody rsp)

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

runServerChecked :: Bool -> FilePath -> [String] -> IO ()
runServerChecked withStatic root args
    = do mec <- runServer withStatic root args
         case mec of
             Just ExitSuccess -> return ()
             _ -> die "Bad exit code from server"

runServer :: Bool -> FilePath -> [String] -> IO (Maybe ExitCode)
runServer withStatic root args = run server args'
    where server = root </> "dist/build/hackage-server/hackage-server"
          args' = if withStatic
                  then ("--static-dir=" ++ root </> "static/") : args
                  else                                           args

info :: String -> IO ()
info str = putStrLn ("= " ++ str)

die :: String -> IO a
die err = do hPutStrLn stderr err
             exitFailure

