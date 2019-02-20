{-
  This is a very high-level test of the hackage server. It forks a fresh server
  instance, and then uses HTTP to run various requests on that server.

  System requirements:

  1. Port `testPort` (currently 8392) must be available on localhost
  2. You must allow for outgoing HTTP traffic, as we POST to validator.w3.org
     for HTML validation.
-}

module Main (main) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (isInfixOf)
import Data.String ()
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO

import Package
import Util
import HttpUtils ( isOk
                 , isNoContent
                 , isForbidden
                 , Authorization(..)
                 )
import HackageClientUtils


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

testName :: FilePath
testName = "HighLevelTestTemp"

testDir :: FilePath -> FilePath
testDir root = root </> "tests" </> testName

doit :: FilePath -> IO ()
doit root
    = do info "initialising hackage database"
         runServerChecked root ["init"]
         withServerRunning root $ do void $ validate NoAuth "/"
                                     void $ validate NoAuth "/accounts"
                                     void $ validate (Auth "admin" "admin") "/admin"
                                     void $ validate NoAuth "/upload"
                                     runUserTests
                                     runPackageUploadTests
                                     runPackageTests
         withServerRunning root $ runPackageTests
         info "Making database backup"
         tarGz1 <- createBackup testName root "1"
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
         tarGz2 <- createBackup testName root "2"
         info "Checking databases match"
         db1 <- LBS.readFile tarGz1
         db2 <- LBS.readFile tarGz2
         unless (db1 == db2) $ die "Databases don't match"
         info "Checking server still works, and data is intact"
         withServerRunning root $ runPackageTests


runUserTests :: IO ()
runUserTests = do
    do info "Getting user list"
       xs <- fmap (map userName) getUsers
       unless (xs == ["admin"]) $
           die ("Bad user list: " ++ show xs)
    do info "Getting admin user list"
       xs <- getAdmins
       unless (map userName (groupMembers xs) == ["admin"]) $
           die ("Bad admin user list: " ++ show xs)
    do -- For this test we just create the users directly using the admin
       -- interface, there's a separate test that tests the self-signup.
       createUserDirect (Auth "admin" "admin") "HackageTestUser1" "testpass1"
       createUserDirect (Auth "admin" "admin") "HackageTestUser2" "testpass2"
    do info "Checking new users are now in user list"
       xs <- fmap (map userName) getUsers
       unless (xs == ["admin","HackageTestUser1","HackageTestUser2"]) $
           die ("Bad user list: " ++ show xs)
    do info "Checking new users are not in admin list"
       xs <- getAdmins
       unless (map userName (groupMembers xs) == ["admin"]) $
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
       xs <- fmap (map userName) getUsers
       unless (xs == ["admin","HackageTestUser1","HackageTestUser2"]) $
           die ("Bad user list: " ++ show xs)
    do info "Deleting HackageTestUser2 as admin"
       delete isNoContent (Auth "admin" "admin") "/user/HackageTestUser2"
       xs <- fmap (map userName) getUsers
       unless (xs == ["admin","HackageTestUser1"]) $
           die ("Bad user list: " ++ show xs)
    do info "Getting user info for HackageTestUser1"
       xs <- validate NoAuth "/user/HackageTestUser1"
       --TODO: set the user's real name, and then look for that here
       unless ("HackageTestUser1" `isInfixOf` xs) $
           die ("Bad user info: " ++ show xs)

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
    do info "Getting package index with etag"
       validateETagHandling "/packages/index.tar.gz"
    do info "Getting testpackage info"
       xs <- validate NoAuth "/package/testpackage.html"
       unless (">testpackage</a>: <small>test package testpackage</small></h1>" `isInfixOf` xs) $
           die ("Bad package info: " ++ show xs)
    do info "Getting testpackage-1.0.0.0 info"
       xs <- validate NoAuth "/package/testpackage-1.0.0.0.html"
       unless (">testpackage</a>: <small>test package testpackage</small></h1>" `isInfixOf` xs) $
           die ("Bad package info: " ++ show xs)

    do info "Getting testpackage-1.0.0.0 info (JSON)"
       xs <- validate NoAuth "/package/testpackage-1.0.0.0.json"
       unless ("\"synopsis\":\"test package testpackage\"" `isInfixOf` xs) $
           die ("Bad package info: " ++ show xs)

    do info "Getting testpackage version info (JSON)"
       xs <- validate NoAuth "/package/testpackage.json"
       unless ("\"1.0.0.0\":\"normal\"" `isInfixOf` xs) $
           die ("Bad package version info: " ++ show xs)

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
    do info "Getting testpackage source with etag"
       validateETagHandling ("/package/testpackage/src" </> testpackageHaskellFilename)
    do info "Getting testpackage maintainer info"
       xs <- getGroup "/package/testpackage/maintainers/.json"
       unless (map userName (groupMembers xs) == ["HackageTestUser1"]) $
           die "Bad maintainers list"
  where
    (_,                             testpackageTarFileContent,
     testpackageCabalIndexFilename, testpackageCabalFile,
     testpackageHaskellFilename,    testpackageHaskellFileContent)
       = testpackage

testpackage :: (FilePath, String, FilePath, String, FilePath, String)
testpackage = mkPackage "testpackage"

