-- | Generic HTTP utilities
{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module HttpUtils (
  -- * Stateless functions
    ExpectedCode
  , isOk
  , isAccepted
  , isNoContent
  , isSeeOther
  , isNotModified
  , isUnauthorized
  , isForbidden
  , parseQuery
  -- * Stateful functions
  , Authorization(..)
  , execRequest
  , execRequest'
  , responseHeader
  , execPostFile
  -- * Interface to html5.validator.nu
  , validate
) where

import Control.Exception
import Control.Monad
import Data.Maybe
import Network.HTTP hiding (user)
import Network.HTTP.Auth
import Data.Aeson (Value(..), FromJSON(..), (.:))

import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Base64 as Base64

import Util

{------------------------------------------------------------------------------
  Stateless functions
------------------------------------------------------------------------------}

type ExpectedCode = (Int, Int, Int) -> Bool

isOk, isAccepted, isNoContent, isSeeOther :: ExpectedCode
isNotModified, isUnauthorized, isForbidden :: ExpectedCode
isOk           = (== (2, 0, 0))
isAccepted     = (== (2, 0, 2))
isNoContent    = (== (2, 0, 4))
isSeeOther     = (== (3, 0, 3))
isNotModified  = (== (3, 0, 4))
isUnauthorized = (== (4, 0, 1))
isForbidden    = (== (4, 0, 3))

parseQuery :: String -> [(String, String)]
parseQuery = map parseAssignment . explode '&'
  where
    parseAssignment :: String -> (String, String)
    parseAssignment a = let [var, val] = explode '=' a
                        in (var, val)

{------------------------------------------------------------------------------
  Stateful functions
------------------------------------------------------------------------------}

data Authorization = NoAuth | Auth String String

withAuth :: Authorization -> Request_String -> (Request_String -> IO a) -> IO a
withAuth NoAuth           req f = f req
withAuth (Auth user pass) req f = do
  res <- simpleHTTP req
  case res of
    Left e -> die ("Request failed: " ++ show e)
    Right rsp
      | rspCode rsp == (4, 0, 1) -> do
          let uri        = rqURI req
              hdrs       = retrieveHeaders HdrWWWAuthenticate rsp
              challenges = catMaybes $ map (headerToChallenge uri) hdrs
          auth <- case challenges of
                    [] ->
                      die "No challenges"
                    ChalBasic realm : _ ->
                      return AuthBasic {
                          auSite     = uri
                        , auRealm    = realm
                        , auUsername = user
                        , auPassword = pass
                        }
                    ChalDigest realm domain nonce opaq _stale alg qop : _ ->
                      return AuthDigest {
                          auRealm     = realm
                        , auUsername  = user
                        , auPassword  = pass
                        , auDomain    = domain
                        , auNonce     = nonce
                        , auOpaque    = opaq
                        , auAlgorithm = alg
                        , auQop       = qop
                        }
          f $ insertHeader HdrAuthorization (withAuthority auth req) req
      | otherwise ->
         badResponse rsp

execRequest :: Authorization -> Request_String -> IO String
execRequest auth req = execRequest' auth req isOk

execRequest' :: Authorization -> Request_String -> ExpectedCode -> IO String
execRequest' auth req' expectedCode = withAuth auth req' $ \req -> do
    res <- simpleHTTP req
    case res of
      Left e -> die ("Request failed: " ++ show e)
      Right rsp | expectedCode (rspCode rsp) -> return $ rspBody rsp
                | otherwise                  -> badResponse rsp

responseHeader :: HeaderName -> Request_String -> IO String
responseHeader h req = do
    res <- simpleHTTP req
    case res of
      Left e -> die ("Request failed: " ++ show e)
      Right rsp -> case lookupHeader h $ getHeaders rsp of
                     Just v -> return v
                     _ -> die ("Header missing: " ++ show h)

execPostFile :: ExpectedCode
             -> Authorization -> Request_String -> String
             -> (FilePath, String)
             -> IO ()
execPostFile expectedCode auth req field (filename, fileContents) =
    void $ execRequest' auth req' expectedCode
  where
    boundary = "--BOUNDARY"
    req'     = setRequestBody req
                              ("multipart/form-data; boundary=" ++ boundary,
                               body)
    unlines' = concat . map (++ "\r\n")
    body     = unlines' [
        "--" ++ boundary
      , "Content-Disposition: form-data; name=" ++ show field ++ "; filename=" ++ show filename
      , "Content-Type: application/gzip"
        -- Base64 encoding avoids any possibility of
        -- the boundary clashing with the file data
      , "Content-Transfer-Encoding: base64"
      , ""
      , BS.unpack $ Base64.encode $ BS.pack fileContents
      , "--" ++ boundary ++ "--"
      , ""
      ]

badResponse :: Response String -> IO a
badResponse rsp =
     die $ "Bad response code: " ++ show (rspCode rsp) ++ "\n\n"
        ++ show rsp ++ "\n\n"
        ++ rspBody rsp

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

getValidateResult :: Authorization -> String
                  -> IO (String, Maybe ValidateResult)
getValidateResult auth url = do
          body <- execRequest auth (getRequest url)
          result <-
            catch
            (do
                json <- execRequest NoAuth
                        (postRequestWithBody validatorURL "text/html" body)
                result <- decodeJSON json
                return $ Just result)
            handler
          return (body, result)
  where handler :: IOException -> IO (Maybe ValidateResult)
        handler _ = return Nothing
        validatorURL = "http://html5.validator.nu?out=json&charset=UTF-8"

validationErrors :: ValidateResult -> [String]
validationErrors = aux [] . validateMessages
  where
    aux :: [String] -> [ValidateMessage] -> [String]
    aux acc [] = reverse acc
    aux acc (_msg@(ValidateInfo {})  : msgs) = aux acc msgs
    aux acc ( msg@(ValidateError {}) : msgs) = aux (validateMessage msg : acc) msgs

-- Validate and return the list of errors
validate :: Authorization -> String -> IO (String, [String])
validate auth url = do
  (body, mres) <- getValidateResult auth url
  case mres of
    Just result -> return (body, validationErrors result)
    Nothing -> return (body, ["Couldn't connect to validation service"])
