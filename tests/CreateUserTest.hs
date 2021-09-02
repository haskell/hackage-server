{-
  This a separate part of the high-level test of the hackage server
  (see HighLevelTest.hs). This set of tests check that the user self
  registration wors. This test needs local outgoing email, which isn't
  available on all hosts, so we keep it as a separate test.

  System requirements:

  1. Port `testPort` (currently 8392) must be available on localhost
  2. You must have sendmail configured so that it can send emails to external
     domains (for user registration) -- currently we use mailinator.com accounts
  3. You must allow for outgoing HTTP traffic, as we POST to validator.w3.org
     for HTML validation.
-}

module Main (main) where

import Control.Exception
import Control.Monad
import Data.List (isInfixOf)
import Data.String ()
import System.Directory
import System.FilePath
import System.IO
import System.Random

import MailUtils
import Util
import HttpUtils (Authorization(..))
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
testName = "CreateUserTestTemp"

testDir :: FilePath -> FilePath
testDir root = root </> "tests" </> testName

doit :: FilePath -> IO ()
doit root
    = do info "initialising hackage database"
         runServerChecked root ["init"]
         withServerRunning root runUserTests

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

    testEmail1 <- do
       -- Create random test email addresses so that we don't confuse
       -- confirmation emails from other sessions
       testEmail1 <- mkTestEmail `liftM` randomIO
       testEmail2 <- mkTestEmail `liftM` randomIO
       createUserSelfRegister "HackageTestUser1" "Test User 1" testEmail1
       createUserSelfRegister "HackageTestUser2" "Test User 2" testEmail2
       confirmUser testEmail1 "testpass1"
       confirmUser testEmail2 "testpass2"
       return (testEmailAddress testEmail1)
    do info "Checking new users are now in user list"
       xs <- getUsers
       unless (xs == ["admin","HackageTestUser1","HackageTestUser2"]) $
           die ("Bad user list: " ++ show xs)
    do info "Checking new users are not in admin list"
       xs <- getAdmins
       unless (groupMembers xs == ["admin"]) $
           die ("Bad admin user list: " ++ show xs)
    do info "Checking new users name & contact info"
       ncinf <- getNameContactInfo (Auth "HackageTestUser1" "testpass1")
                                   "/user/HackageTestUser1/name-contact.json"
       unless (realName ncinf == "Test User 1") $
           die ("Bad user real name: " ++ realName ncinf)
       unless (contactEmailAddress ncinf == testEmail1) $
           die ("Bad user email: " ++ contactEmailAddress ncinf)
    do info "Checking new users admin info"
       uainf <- getUserAdminInfo (Auth "admin" "admin") "/user/HackageTestUser1/admin-info.json"
       unless (accountKind uainf == Just "AccountKindRealUser") $
           die ("Bad user account kind: " ++ show (accountKind uainf))
       unless ("self-registration" `isInfixOf` accountNotes uainf) $
           die ("Bad user notes: " ++ accountNotes uainf)
       
  where
    mkTestEmail :: Int -> String
    mkTestEmail n = "HackageTestUser" ++ show n

