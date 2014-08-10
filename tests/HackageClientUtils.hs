{-# LANGUAGE OverloadedStrings #-}
module HackageClientUtils where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Applicative
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

import Run
import MailUtils
import Util
import HttpUtils ( ExpectedCode
                 , isOk
                 , isAccepted
                 , isSeeOther
                 , isNotModified
                 , isUnauthorized
                 , isForbidden
                 , Authorization(..)
                 )
import qualified HttpUtils as Http

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
                              threadDelay 5000000
                              f (n - 1)

createBackup :: FilePath -> FilePath -> FilePath -> IO FilePath
createBackup testName root suffix = do
    runServerChecked root ["backup", "-o", root </> "tests" </> testName </> suffix]
    findTarGz (root </> "tests" </> testName </> suffix)
  where
    findTarGz :: FilePath -> IO FilePath
    findTarGz dir = do
      [tarGz] <- find dir (".tar.gz" `isSuffixOf`)
      return tarGz

find :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
find dir p = (map (dir </>) . filter p) `liftM` getDirectoryContents dir

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

data UserInfo = UserInfo { userName :: User
                         , userId :: Int
                         }
              deriving Show

instance FromJSON UserInfo where
  parseJSON (Object obj) = do
    name <- obj .: "username"
    uid  <- obj .: "userid"
    return UserInfo { userName = name
                    , userId   = uid
                    }
  parseJSON _ = fail "Expected object"

data Group = Group { groupMembers     :: [UserInfo]
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

getUsers :: IO [UserInfo]
getUsers = getUrl NoAuth "/users/.json" >>= decodeJSON

getAdmins :: IO Group
getAdmins = getGroup "/users/admins/.json"

getGroup :: String -> IO Group
getGroup url = getUrl NoAuth url >>= decodeJSON

createUserDirect :: Authorization -> User -> String -> IO ()
createUserDirect auth user pass = do
  info $ "Creating user " ++ user
  post auth "/users/" [
      ("username",        user)
    , ("password",        pass)
    , ("repeat-password", pass)
    ]

createUserSelfRegister :: User -> String -> String -> IO ()
createUserSelfRegister user real email = do
  info $ "Requesting registration for user " ++ real
      ++ " with email address " ++ testEmailAddress email
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

data NameContactInfo = NameContactInfo { realName :: String
                                       , contactEmailAddress :: String }
  deriving Show

instance FromJSON NameContactInfo where
  parseJSON (Object obj) = do
    name  <- obj .: "name"
    email <- obj .: "contactEmailAddress"
    return (NameContactInfo name email)
  parseJSON _ = fail "Expected object"

getNameContactInfo :: Authorization -> String -> IO NameContactInfo
getNameContactInfo auth url = getUrl auth url >>= decodeJSON


data UserAdminInfo = UserAdminInfo { accountKind :: Maybe String
                                   , accountNotes :: String }
  deriving Show

instance FromJSON UserAdminInfo where
  parseJSON (Object obj) = do
    kind_ <- obj .: "accountKind"
    kind  <- case kind_ of
               Null -> return Nothing
               Object kobj -> (do Array _ <- kobj .: "AccountKindRealUser"
                                  return (Just "AccountKindRealUser"))
                          <|> (return (Just ""))
               _ -> fail "unexpected accountKind"
    notes <- obj .: "notes"
    return (UserAdminInfo kind notes)
  parseJSON _ = fail "Expected object"

getUserAdminInfo :: Authorization -> String -> IO UserAdminInfo
getUserAdminInfo auth url = getUrl auth url >>= decodeJSON

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

getETag :: RelativeURL -> IO String
getETag url = Http.responseHeader HdrETag (mkGetReq url)

mkGetReqWithETag :: String -> RelativeURL -> Request_String
mkGetReqWithETag url etag =
    Request (fromJust $ parseURI $ mkUrl url) GET hdrs ""
  where
    hdrs = [mkHeader HdrIfNoneMatch etag]

validateETagHandling :: RelativeURL -> IO ()
validateETagHandling url = void $ do
    etag <- getETag url
    checkETag etag
    checkETagMismatch (etag ++ "garbled123")
  where
    checkETag etag = void $ Http.execRequest' NoAuth (mkGetReqWithETag url etag) isNotModified
    checkETagMismatch etag = void $ Http.execRequest NoAuth (mkGetReqWithETag url etag)

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

