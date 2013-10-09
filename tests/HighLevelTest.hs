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

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
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
import Network.HTTP hiding (user)
import Network.HTTP.Auth
import Network.URI
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error

import Package
import Run

-- For the html5.validator.nu API
import Data.String ()
import Data.Aeson (FromJSON(..), Value(..), (.:))
import qualified Data.Aeson as Aeson

-- For mail
import qualified Text.XML.Light as XML
import System.Random (randomIO)

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
         withServerRunning root $ do validate (mkUrl "/")
                                     validate (mkUrl "/accounts")
                                     validate (mkUrl "/admin")
                                     validate (mkUrl "/upload")
                                     runUserTests
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
       void $ getAuthUrlStrings "HackageTestUser1" "testpass" "/user/HackageTestUser1/password"
    do info "Getting password change page for HackageTestUser1 as an admin"
       void $ getAuthUrlStrings "admin" "admin" "/user/HackageTestUser1/password"
    do info "Getting password change page for HackageTestUser1 as another user"
       checkForbiddenUrl "HackageTestUser2" "testpass2" "/user/HackageTestUser1/password"
    do info "Getting password change page for HackageTestUser1 with bad password"
       checkUnauthedUrl "HackageTestUser1" "badpass" "/user/HackageTestUser1/password"
    do info "Getting password change page for HackageTestUser1 with bad username"
       checkUnauthedUrl "baduser" "testpass" "/user/HackageTestUser1/password"
    do info "Changing password for HackageTestUser2"
       void $ postAuthToUrl "HackageTestUser2" "testpass2" "/user/HackageTestUser2/password"
                  [("password", "newtestpass2"),
                   ("repeat-password", "newtestpass2"),
                   ("_method", "PUT")]
    do info "Checking password has changed"
       void $ getAuthUrlStrings "HackageTestUser2" "newtestpass2"
                                "/user/HackageTestUser2/password"
       checkUnauthedUrl "HackageTestUser2" "testpass2" "/user/HackageTestUser2/password"
    do info "Trying to delete HackageTestUser2 as HackageTestUser2"
       deleteUrlRes ((4, 0, 3) ==) "HackageTestUser2" "newtestpass2" "/user/HackageTestUser2"
       xs <- getUrlStrings "/users/"
       unless (xs == ["Hackage users","admin","HackageTestUser1","HackageTestUser2"]) $
           die ("Bad user list: " ++ show xs)
    do info "Deleting HackageTestUser2 as admin"
       deleteUrlRes ((2, 0, 0) ==) "admin" "admin" "/user/HackageTestUser2"
       xs <- getUrlStrings "/users/"
       unless (xs == ["Hackage users","admin","HackageTestUser1"]) $
           die ("Bad user list: " ++ show xs)
    do info "Getting user info for HackageTestUser1"
       xs <- getUrlStrings "/user/HackageTestUser1"
       unless (xs == ["HackageTestUser1"]) $
           die ("Bad user info: " ++ show xs)
  where
    mkTestEmail :: Int -> String
    mkTestEmail n = "HackageTestUser" ++ show n

runPackageUploadTests :: IO ()
runPackageUploadTests = do
    do info "Getting package list"
       xs <- getUrlStrings "/packages/"
       unless (xs == ["Packages by category","Categories:","."]) $
           die ("Bad package list: " ++ show xs)
    do info "Trying to upload testpackage"
       void $ postFileAuthToUrlRes ((4, 0, 3) ==)
                  "HackageTestUser1" "testpass" "/packages/" "package"
                  (testpackageTarFilename, testpackageTarFileContent)
    do info "Adding HackageTestUser1 to uploaders"
       void $ postAuthToUrl "admin" "admin" "/packages/uploaders/"
                  [("user", "HackageTestUser1")]
    do info "Uploading testpackage"
       void $ postFileAuthToUrlRes ((2, 0, 0) ==)
                  "HackageTestUser1" "testpass" "/packages/" "package"
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
                      "HackageTestUser1"]) $
           die "Bad maintainers list"
    where (_,                             testpackageTarFileContent,
           testpackageCabalIndexFilename, testpackageCabalFile,
           testpackageHaskellFilename,    testpackageHaskellFileContent)
              = testpackage

testpackage :: (FilePath, String, FilePath, String, FilePath, String)
testpackage = mkPackage "testpackage"

getUrl :: String -> IO String
getUrl relPath = getReq (getRequest (mkUrl relPath))

getUrlStrings :: String -> IO [String]
getUrlStrings relPath = do validate url
                           getReqStrings (getRequest url)
    where url = mkUrl relPath

getAuthUrlStrings :: String -> String -> String -> IO [String]
getAuthUrlStrings u p relPath
    = do validate $ mkAuthUrl u p relPath
         withAuth u p (getRequest (mkUrl relPath)) getReqStrings

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

postToUrl :: ((Int, Int, Int) -> Bool) -> String -> [(String, String)] -> IO ()
postToUrl expectedResponse url vals = checkReqGivesResponse expectedResponse $
  mkPostReq url vals

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

mkAuthUrl :: String -> String -> String -> String
mkAuthUrl u p relPath = "http://" ++ u ++ ":" ++ p ++ "@127.0.0.1:" ++ show testPort ++ relPath

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
                  then ("--static-dir=" ++ root </> "datafiles/") : args
                  else                                              args

info :: String -> IO ()
info str = putStrLn ("= " ++ str)

die :: String -> IO a
die err = do hPutStrLn stderr err
             exitFailure

{------------------------------------------------------------------------------
  Hackage-specific API
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
getGroup url = getReq (getRequest (mkUrl url)) >>= decodeJSON

createUser :: User -> String -> String -> IO ()
createUser user real email = do
  info $ "Creating user " ++ real ++ " with email address " ++ testEmailAddress email
  postToUrl expectAccepted "/users/register-request" [
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
  postToUrl expectSeeOther urlWithNonce [
      ("password",        pass)
    , ("repeat-password", pass)
    ]

{------------------------------------------------------------------------------
  JSON utils
------------------------------------------------------------------------------}

getJSONStrings :: String -> IO [String]
getJSONStrings url = getReq (getRequest (mkUrl url)) >>= decodeJSON

decodeJSON :: FromJSON a => String -> IO a
decodeJSON json =
  case Aeson.decode (LBS.pack json) of
    Nothing     -> fail "Could not decode JSON"
    Just result -> return result

{------------------------------------------------------------------------------
  Interface to html5.validator.nu

  NOTE: We only parse bits of the information returned by html5.validator.nu
------------------------------------------------------------------------------}

data ValidateResult = ValidateResult {
    validateMessages :: [ValidateMessage]
  }
  deriving Show

data ValidateMessage = ValidateInfo {
      validateMessage :: String
    }
  | ValidateError {
      validateMessage :: String
  }
  deriving Show

instance FromJSON ValidateResult where
  parseJSON (Object obj) = do
    msgs <- obj .: "messages"
    return ValidateResult {
        validateMessages = msgs
      }
  parseJSON _ = fail "Expected object"

instance FromJSON ValidateMessage where
  parseJSON (Object obj) = do
    msgType <- obj .: "type"
    msgCtnt <- obj .: "message"
    case msgType of
      "info"  -> return ValidateInfo {
                     validateMessage = msgCtnt
                   }
      "error" -> return ValidateError {
                     validateMessage = msgCtnt
                   }
      _       -> fail $ "Unknown message type " ++ msgType
  parseJSON _ = fail "Expected object"

getValidateResult :: String -> IO ValidateResult
getValidateResult url = do
    body <- getReq (getRequest url)
    json <- getReq (postRequestWithBody validatorURL "text/html" body)
    decodeJSON json
  where
    validatorURL = "http://html5.validator.nu?out=json&charset=UTF-8"

validationErrors :: ValidateResult -> [String]
validationErrors = aux [] . validateMessages
  where
    aux :: [String] -> [ValidateMessage] -> [String]
    aux acc [] = reverse acc
    aux acc (_msg@(ValidateInfo {})  : msgs) = aux acc msgs
    aux acc ( msg@(ValidateError {}) : msgs) = aux (validateMessage msg : acc) msgs

validate :: String -> IO ()
validate url = do
  putStr ("Validating " ++ show url ++ ": ") ; hFlush stdout
  errs <- validationErrors `liftM` getValidateResult url
  if null errs
    then putStrLn "ok"
    else do putStrLn $ "failed: " ++ show (length errs) ++ " error(s)"
            forM_ (zip [1..] errs) $ \(i, err) ->
              putStrLn $ show (i :: Int) ++ ".\t" ++ err

{------------------------------------------------------------------------------
  Interface to mailinator.com

  This is setup in such a way so that we can easily change what service we
  use for temp email (or indeed non-temp email)
------------------------------------------------------------------------------}

testEmailAddress :: String -> String
testEmailAddress user = user ++ "@mailinator.com"

data Email = Email {
    emailTitle  :: String
  , emailLink   :: URI
  , emailSender :: String
  , emailDate   :: String
  }
  deriving Show

checkEmail :: String -> IO [Email]
checkEmail user = do
    rss <- (XML.onlyElems . XML.parseXML) `liftM` getReq (getRequest rssUrl)
    let items = concatMap (XML.filterElementsName $ simpleName "item") rss
    return (map parseEmail items)
  where
    rssUrl = "http://www.mailinator.com/feed?to=" ++ user

    parseEmail :: XML.Element -> Email
    parseEmail e =
      let [title]  = XML.filterElementsName (simpleName "title")   e
          [link]   = XML.filterElementsName (simpleName "link")    e
          [sender] = XML.filterElementsName (simpleName "creator") e
          [date]   = XML.filterElementsName (simpleName "date")    e
      in Email { emailTitle  = XML.strContent title
               , emailLink   = fromJust . parseURI . XML.strContent $ link
               , emailSender = XML.strContent sender
               , emailDate   = XML.strContent date
               }

    simpleName :: String -> XML.QName -> Bool
    simpleName n = (== n) . XML.qName

emailWithSubject :: String -> String -> IO (Maybe Email)
emailWithSubject user subject = do
  emails <- checkEmail user
  return . listToMaybe . filter ((== subject) . emailTitle) $ emails

waitForEmailWithSubject :: String -> String -> IO Email
waitForEmailWithSubject user subject = f 10
  where
    f :: Int -> IO Email
    f n = do
      info $ "Waiting for confirmation email at " ++ testEmailAddress user
      mEmail <- emailWithSubject user subject
      case mEmail of
        Just email          -> return email
        Nothing | n == 0    -> die "Didn't get confirmation email"
                | otherwise -> do
          info "No confirmation email yet; will try again in 30 sec"
          threadDelay (30 * 1000000)
          f (n - 1)

getEmail :: Email -> IO String
getEmail email = getReq (getRequest url)
  where
    msgid = fromJust . lookup "msgid" . parseQuery . uriQuery . emailLink $ email
    url   = "http://mailinator.com/rendermail.jsp?msgid=" ++ msgid ++ "&text=true"

{------------------------------------------------------------------------------
  HTTP utils
------------------------------------------------------------------------------}

expectAccepted :: (Int, Int, Int) -> Bool
expectAccepted = (== (2, 0, 2))

expectSeeOther :: (Int, Int, Int) -> Bool
expectSeeOther = (== (3, 0, 3))

parseQuery :: String -> [(String, String)]
parseQuery = map parseAssignment . explode '&'
  where
    parseAssignment :: String -> (String, String)
    parseAssignment a = let [var, val] = explode '=' a
                        in (var, val)

{------------------------------------------------------------------------------
  Misc util
------------------------------------------------------------------------------}

-- | > explode ',' "abc,def,ghi" == ["abc", "def", "ghi"]
explode :: Eq a => a -> [a] -> [[a]]
explode x xs = let (xs1, xs2) = span (/= x) xs
               in xs1 : case xs2 of
                          []        -> []
                          (_ : xs') -> explode x xs'

-- | Remove leading and trailing whitespace
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
