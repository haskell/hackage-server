-- | Error handling style for the hackage server.
--
-- The point is to be able to abort and return appropriate HTTP errors, plus
-- human readable messages.
--
-- We use a standard error monad / exception style so that we can hide some of
-- the error checking plumbing.
--
-- We use a custom error type that enables us to render the error in an
-- appropriate way, ie themed html or plain text, depending on the context.
--
module Distribution.Server.Framework.Error (

    -- * Server error monad
    ServerPartE,

    -- * Generating errors
    MessageSpan(..),
    errBadRequest,
    errForbidden,
    errNotFound,
    errBadMediaType,
    errInternalError,
    throwError,

    -- * Handling errors
    ErrorResponse(..),
    runServerPartE,
    handleErrorResponse,
    messageToText,

    -- * Handy error message operator
    (?!)
  ) where

import Happstack.Server
import Control.Monad.Error

import qualified Data.ByteString.Lazy.Char8 as BS

-- | The \"oh noes?!\" operator
--
(?!) :: Maybe a -> e -> Either e a
ma ?! e = maybe (Left e) Right ma


-- | A derivative of the 'ServerPartT' monad with an extra error monad layer.
--
-- So we can use the standard 'MonadError' methods like 'throwError'.
--
type ServerPartE a = ServerPartT (ErrorT ErrorResponse IO) a

-- | A type for generic error reporting that should be sufficient for
-- most purposes.
--
data ErrorResponse = ErrorResponse {
    errorCode   :: Int,
    errorHeaders:: [(String, String)],
    errorTitle  :: String,
    errorDetail :: [MessageSpan]
} deriving (Eq, Show)

instance ToMessage ErrorResponse where
  toResponse (ErrorResponse code hdrs title detail) =
    let rspbody = title ++ ": " ++ messageToText detail ++ "\n"
     in Response {
          rsCode      = code,
          rsHeaders   = mkHeaders (("Content-Type",  "text/plain") : reverse hdrs),
          rsFlags     = nullRsFlags { rsfLength = ContentLength },
          rsBody      = BS.pack rspbody,
          rsValidator = Nothing
        }

-- | A message possibly including hypertext links.
--
-- The point is to be able to render error messages either as text or as html.
--
data MessageSpan = MLink String String | MText String
  deriving (Eq, Show)

-- | Format a message as simple text.
--
-- For html or other formats you'll have to write your own function!
--
messageToText :: [MessageSpan] -> String
messageToText []             = ""
messageToText (MLink x _:xs) = x ++ messageToText xs
messageToText (MText x  :xs) = x ++ messageToText xs

-- We don't want to use these methods directly anyway.
instance Error ErrorResponse where
    noMsg      = ErrorResponse 500 [] "Internal server error" []
    strMsg str = ErrorResponse 500 [] "Internal server error" [MText str]


errBadRequest    :: String -> [MessageSpan] -> ServerPartE a
errBadRequest    title message = throwError (ErrorResponse 400 [] title message)

-- note: errUnauthorized is deliberately not provided because exceptions thrown
-- in this way bypass the FilterMonad stuff and so setHeaderM etc are ignored
-- but setHeaderM are usually needed for responding to auth errors.

errForbidden     :: String -> [MessageSpan] -> ServerPartE a
errForbidden     title message = throwError (ErrorResponse 403 [] title message)

errNotFound      :: String -> [MessageSpan] -> ServerPartE a
errNotFound      title message = throwError (ErrorResponse 404 [] title message)

errBadMediaType  :: String -> [MessageSpan] -> ServerPartE a
errBadMediaType  title message = throwError (ErrorResponse 415 [] title message)

errInternalError :: [MessageSpan] -> ServerPartE a
errInternalError       message = throwError (ErrorResponse 500 [] title message)
  where
    title = "Internal server error"

-- | Run a 'ServerPartE', including a top-level fallback error handler.
--
-- Any 'ErrorResponse' exceptions are turned into a simple error response with
-- a \"text/plain\" formated body.
--
-- To use a nicer custom formatted error response, use 'handleErrorResponse'.
--
runServerPartE :: ServerPartE a -> ServerPart a
runServerPartE = mapServerPartT' (spUnwrapErrorT fallbackHandler)
  where
    fallbackHandler :: ErrorResponse -> ServerPart a
    fallbackHandler err = finishWith (toResponse err)

handleErrorResponse :: (ErrorResponse -> ServerPartE Response)
                    -> ServerPartE a -> ServerPartE a
handleErrorResponse handler action =
    catchError action (\errResp -> handler errResp >>= finishWith)
