{-
  This is a very high-level test of the hackage server. It forks a fresh server
  instance, and then uses HTTP to run various requests on that server.

  System requirements:

  1. Port `testPort` (currently 8392) must be available on localhost
  2. You must have sendmail configured so that it can send emails to external
     domains (for user registration) -- currently we use mailinator.com accounts
  3. You must allow for outgoing HTTP traffic, as we POST to html5.validator.nu
     for HTML validation.
-}

{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (isInfixOf, isSuffixOf)
import Data.Maybe
import Data.String ()
import Data.Aeson (FromJSON(..), Value(..), (.:))
import Network.HTTP hiding (user)
import Network.URI
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Random

import Package
import Run
import MailUtils
import Util
import HttpUtils ( ExpectedCode
                 , isOk
                 , isAccepted
                 , isSeeOther
                 , isUnauthorized
                 , isForbidden
                 , Authorization(..)
                 )
import qualified HttpUtils as Http

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          info "Initialising"
          root <- getCurrentDirectory
          info "Setting up test directory"
          exists <- doesDirectoryExist (testDir root)
          when exists $ removeDirectoryRecursive (testDir root)
          createDirectory (testDir root)
          (setCurrentDirectory (testDir root) >> doit root)
              `finally` removeDirectoryRecursive (testDir root)

testDir :: FilePath -> FilePath
testDir root = root </> "tests" </> "HighLevelTestTemp"

doit :: FilePath -> IO ()
doit root
    = do info "initialising hackage database"
         runServerChecked root ["init"]
         withServerRunning root $ do void $ validate NoAuth "/"
                                     void $ validate NoAuth "/accounts"
                                     void $ validate NoAuth "/admin"
                                     void $ validate NoAuth "/upload"
                                     runUserTests
                                     runPackageUploadTests
                                     runPackageTests
         withServerRunning root $ runPackageTests
         info "Making database backup"
         tarGz1 <- createBackup root "1"
         info "Removing old state"
         removeDirectoryRecursive "state"
         info "Checking server doesn't work"
         mec <- runServer root serverRunningArgs
         case mec of
             Just (ExitFailure 1) -> return ()
             Just (ExitFailure _) -> die "Server failed with wrong exit code"
             Just ExitSuccess     -> die "Server worked unexpectedly"
             Nothing              -> die "Got a signal?"
         info $ "Restoring database from " ++ tarGz1
         runServerChecked root ["restore", tarGz1]
         info "Making another database backup"
         tarGz2 <- createBackup root "2"
         info "Checking databases match"
         db1 <- LBS.readFile tarGz1
         db2 <- LBS.readFile tarGz2
         unless (db1 == db2) $ die "Databases don't match"
         info "Checking server still works, and data is intact"
         withServerRunning root $ runPackageTests

find :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
find dir p = (map (dir </>) . filter p) `liftM` getDirectoryContents dir

withServerRunning :: FilePath -> IO () -> IO ()
withServerRunning root f
    = do info "Forking server thread"
         mv <- newEmptyMVar
         bracket (forkIO (do info "Server thread started"
                             void $ runServer root serverRunningArgs
                          `finally` putMVar mv ()))
                 (\t -> do killThread t
                           takeMVar mv
                           info "Server terminated")
                 (\_ -> do waitForServer
                           info "Server running"
                           f
                           info "Finished with server")

serverRunningArgs :: [String]
serverRunningArgs = ["run", "--ip", "127.0.0.1", "--port", show testPort]

runUserTests :: IO ()
runUserTests = do
    do info "Getting user list"
       xs <- getUsers
       unless (xs == ["admin"]) $
           die ("Bad user list: " ++ show xs)
    do info "Getting admin user list"
       xs <- getAdmins
       unless (groupMembers xs == ["admin"]) $
           die ("Bad admin user list: " ++ show xs)
    do -- Create random test email addresses so that we don't confuse
       -- confirmation emails from other sessions
       testEmail1 <- mkTestEmail `liftM` randomIO
       testEmail2 <- mkTestEmail `liftM` randomIO
       createUser "HackageTestUser1" "Test User 1" testEmail1
       createUser "HackageTestUser2" "Test User 2" testEmail2
       confirmUser testEmail1 "testpass1"
       confirmUser testEmail2 "testpass2"
    do info "Checking new users are now in user list"
       xs <- getUsers
       unless (xs == ["admin","HackageTestUser1","HackageTestUser2"]) $
           die ("Bad user list: " ++ show xs)
    do info "Checking new users are not in admin list"
       xs <- getAdmins
       unless (groupMembers xs == ["admin"]) $
           die ("Bad admin user list: " ++ show xs)
    do info "Getting password change page for HackageTestUser1"
       void $ validate (Auth "HackageTestUser1" "testpass1") "/user/HackageTestUser1/password"
    do info "Getting password change page for HackageTestUser1 as an admin"
       void $ validate (Auth "admin" "admin") "/user/HackageTestUser1/password"
    do info "Getting password change page for HackageTestUser1 as another user"
       checkIsForbidden (Auth "HackageTestUser2" "testpass2") "/user/HackageTestUser1/password"
    do info "Getting password change page for HackageTestUser1 with bad password"
       checkIsUnauthorized (Auth "HackageTestUser1" "badpass") "/user/HackageTestUser1/password"
    do info "Getting password change page for HackageTestUser1 with bad username"
       checkIsUnauthorized (Auth "baduser" "testpass1") "/user/HackageTestUser1/password"
    do info "Changing password for HackageTestUser2"
       post (Auth "HackageTestUser2" "testpass2") "/user/HackageTestUser2/password" [
           ("password",        "newtestpass2")
         , ("repeat-password", "newtestpass2")
         , ("_method",         "PUT")
         ]
    do info "Checking password has changed"
       void $ validate (Auth "HackageTestUser2" "newtestpass2") "/user/HackageTestUser2/password"
       checkIsUnauthorized (Auth "HackageTestUser2" "testpass2") "/user/HackageTestUser2/password"
    do info "Trying to delete HackageTestUser2 as HackageTestUser2"
       delete isForbidden (Auth "HackageTestUser2" "newtestpass2") "/user/HackageTestUser2"
       xs <- getUsers
       unless (xs == ["admin","HackageTestUser1","HackageTestUser2"]) $
           die ("Bad user list: " ++ show xs)
    do info "Deleting HackageTestUser2 as admin"
       delete isOk (Auth "admin" "admin") "/user/HackageTestUser2"
       xs <- getUsers
       unless (xs == ["admin","HackageTestUser1"]) $
           die ("Bad user list: " ++ show xs)
    do info "Getting user info for HackageTestUser1"
       xs <- validate NoAuth "/user/HackageTestUser1"
       unless ("Test User 1" `isInfixOf` xs) $
           die ("Bad user info: " ++ show xs)
  where
    mkTestEmail :: Int -> String
    mkTestEmail n = "HackageTestUser" ++ show n

runPackageUploadTests :: IO ()
runPackageUploadTests = do
    do info "Getting package list"
       xs <- map packageName `liftM` getPackages
       unless (xs == []) $
           die ("Bad package list: " ++ show xs)
    do info "Trying to upload testpackage"
       postFile isForbidden
                (Auth "HackageTestUser1" "testpass1")
                "/packages/" "package"
                (testpackageTarFilename, testpackageTarFileContent)
    do info "Adding HackageTestUser1 to uploaders"
       post (Auth "admin" "admin") "/packages/uploaders/" [
           ("user", "HackageTestUser1")
         ]
    do info "Uploading testpackage"
       postFile isOk
                (Auth "HackageTestUser1" "testpass1")
                "/packages/" "package"
                (testpackageTarFilename, testpackageTarFileContent)
  where
    (testpackageTarFilename, testpackageTarFileContent, _, _, _, _) =
      testpackage

runPackageTests :: IO ()
runPackageTests = do
    do info "Getting package list"
       xs <- map packageName `liftM` getPackages
       unless (xs == ["testpackage"]) $
           die ("Bad package list: " ++ show xs)
    do info "Getting package index"
       targz <- getUrl NoAuth "/packages/index.tar.gz"
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
       xs <- validate NoAuth "/package/testpackage"
       unless ("The testpackage package" `isInfixOf` xs) $
           die ("Bad package info: " ++ show xs)
    do info "Getting testpackage-1.0.0.0 info"
       xs <- validate NoAuth "/package/testpackage-1.0.0.0"
       unless ("The testpackage package" `isInfixOf` xs) $
           die ("Bad package info: " ++ show xs)
    do info "Getting testpackage Cabal file"
       cabalFile <- getUrl NoAuth "/package/testpackage-1.0.0.0/testpackage.cabal"
       unless (cabalFile == testpackageCabalFile) $
           die "Bad Cabal file"
    do info "Getting testpackage tar file"
       tarFile <- getUrl NoAuth "/package/testpackage/testpackage-1.0.0.0.tar.gz"
       unless (tarFile == testpackageTarFileContent) $
           die "Bad tar file"
    do info "Getting testpackage source"
       hsFile <- getUrl NoAuth ("/package/testpackage/src" </> testpackageHaskellFilename)
       unless (hsFile == testpackageHaskellFileContent) $
           die "Bad Haskell file"
    do info "Getting testpackage maintainer info"
       xs <- getGroup "/package/testpackage/maintainers/.json"
       unless (groupMembers xs == ["HackageTestUser1"]) $
           die "Bad maintainers list"
  where
    (_,                             testpackageTarFileContent,
     testpackageCabalIndexFilename, testpackageCabalFile,
     testpackageHaskellFilename,    testpackageHaskellFileContent)
       = testpackage

testpackage :: (FilePath, String, FilePath, String, FilePath, String)
testpackage = mkPackage "testpackage"

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

createBackup :: FilePath -> FilePath -> IO FilePath
createBackup root suffix = do
    runServerChecked root ["backup", "-o", testDir root </> suffix]
    findTarGz (testDir root </> suffix)
  where
    findTarGz :: FilePath -> IO FilePath
    findTarGz dir = do
      [tarGz] <- find dir (".tar.gz" `isSuffixOf`)
      return tarGz

runServerChecked :: FilePath -> [String] -> IO ()
runServerChecked root args = do
    mec <- runServer root args
    case mec of
      Just ExitSuccess -> return ()
      _                -> die "Bad exit code from server"

runServer :: FilePath -> [String] -> IO (Maybe ExitCode)
runServer root args = run server args'
  where
    server = root </> "dist/build/hackage-server/hackage-server"
    args'  = ("--static-dir=" ++ root </> "datafiles/") : args

{------------------------------------------------------------------------------
  Access to individual Hackage features
------------------------------------------------------------------------------}

type User  = String
data Group = Group { groupMembers     :: [User]
                   , groupTitle       :: String
                   , groupDescription :: String
                   }
  deriving Show

instance FromJSON Group where
  parseJSON (Object obj) = do
    members <- obj .: "members"
    title   <- obj .: "title"
    descr   <- obj .: "description"
    return Group { groupMembers     = members
                 , groupTitle       = title
                 , groupDescription = descr
                 }
  parseJSON _ = fail "Expected object"

getUsers :: IO [User]
getUsers = getJSONStrings "/users/.json"

getAdmins :: IO Group
getAdmins = getGroup "/users/admins/.json"

getGroup :: String -> IO Group
getGroup url = getUrl NoAuth url >>= decodeJSON

createUser :: User -> String -> String -> IO ()
createUser user real email = do
  info $ "Creating user " ++ real ++ " with email address " ++ testEmailAddress email
  post NoAuth "/users/register-request" [
      ("username", user)
    , ("realname", real)
    , ("email",    testEmailAddress email)
    ]

confirmUser :: String -> String -> IO ()
confirmUser email pass = do
  confirmation <- waitForEmailWithSubject email "Hackage account confirmation"
  emailText    <- getEmail confirmation
  let [urlWithNonce] = map (uriPath . fromJust . parseURI . trim)
                     . filter ("users/register-request" `isInfixOf`)
                     . lines
                     $ emailText
  info $ "Confirming new user at " ++ urlWithNonce
  post NoAuth urlWithNonce [
      ("password",        pass)
    , ("repeat-password", pass)
    ]

data PackageInfo = PackageInfo { packageName :: String }
  deriving Show

instance FromJSON PackageInfo where
  parseJSON (Object obj) = do
    name <- obj .: "packageName"
    return PackageInfo { packageName = name }
  parseJSON _ = fail "Expected object"

getPackages :: IO [PackageInfo]
getPackages = getUrl NoAuth "/packages/.json" >>= decodeJSON

{------------------------------------------------------------------------------
  Small layer on top of HttpUtils, specialized to our test server
------------------------------------------------------------------------------}

type RelativeURL = String
type AbsoluteURL = String

-- A random port, that hopefully won't clash with anything else
testPort :: Int
testPort = 8392

mkUrl :: RelativeURL -> AbsoluteURL
mkUrl relPath = "http://127.0.0.1:" ++ show testPort ++ relPath

mkGetReq :: RelativeURL -> Request_String
mkGetReq url = getRequest (mkUrl url)

mkPostReq :: RelativeURL -> [(String, String)] -> Request_String
mkPostReq url vals =
  setRequestBody (postRequest (mkUrl url))
                 ("application/x-www-form-urlencoded", urlEncodeVars vals)

getUrl :: Authorization -> RelativeURL -> IO String
getUrl auth url = Http.execRequest auth (mkGetReq url)

getJSONStrings :: RelativeURL -> IO [String]
getJSONStrings url = getUrl NoAuth url >>= decodeJSON

checkIsForbidden :: Authorization -> RelativeURL -> IO ()
checkIsForbidden auth url = void $
  Http.execRequest' auth (mkGetReq url) isForbidden

checkIsUnauthorized :: Authorization -> RelativeURL -> IO ()
checkIsUnauthorized auth url = void $
  Http.execRequest' auth (mkGetReq url) isUnauthorized

delete :: ExpectedCode -> Authorization -> RelativeURL -> IO ()
delete expectedCode auth url = void $
  case parseURI (mkUrl url) of
    Nothing  -> die "Bad URL"
    Just uri -> Http.execRequest' auth (mkRequest DELETE uri) expectedCode

post :: Authorization -> RelativeURL -> [(String, String)] -> IO ()
post auth url vals = void $
    Http.execRequest' auth (mkPostReq url vals) expectedCode
  where
    expectedCode code = isOk code || isSeeOther code || isAccepted code

postFile :: ExpectedCode
         -> Authorization -> RelativeURL
         -> String -> (FilePath, String)
         -> IO ()
postFile expectedCode auth url field file =
    Http.execPostFile expectedCode auth (postRequest (mkUrl url)) field file

validate :: Authorization -> RelativeURL -> IO String
validate auth url = do
  putStr ("= Validating " ++ show url ++ ": ") ; hFlush stdout
  (body, errs) <- Http.validate auth (mkUrl url)
  if null errs
    then do putStrLn "ok"
    else do putStrLn $ "failed: " ++ show (length errs) ++ " error(s)"
            forM_ (zip [1..] errs) $ \(i, err) ->
              putStrLn $ show (i :: Int) ++ ".\t" ++ err
  return body
