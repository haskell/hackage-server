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
module Distribution.Server.Error (

    -- * Server error monad
    ServerPartE,

    -- * Generating errors
    MessageSpan(..),
    errBadRequest,
    errForbidden,
    errNotFound,
    errInternalError,
    throwError,

    -- * Handling errors
    ErrorResponse(..),
    runServerPartE,
    handleErrorResponse,
    messageToText,
  ) where

import Happstack.Server
import Control.Monad.Error

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
    errorTitle  :: String,
    errorDetail :: [MessageSpan]
}

-- | A message possibly including hypertext links.
--
-- The point is to be able to render error messages either as text or as html.
--
data MessageSpan = MLink String String | MText String

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
    noMsg      = ErrorResponse 500 "Internal server error" []
    strMsg str = ErrorResponse 500 "Internal server error" [MText str]


errBadRequest    :: String -> [MessageSpan] -> ServerPartE a
errBadRequest    title message = throwError (ErrorResponse 400 title message)

-- note: errUnauthorized is deliberately not provided because exceptions thrown
-- in this way bypass the FilterMonad stuff and so setHeaderM etc are ignored
-- but setHeaderM are usually needed for responding to auth errors.

errForbidden     :: String -> [MessageSpan] -> ServerPartE a
errForbidden     title message = throwError (ErrorResponse 403 title message)

errNotFound      :: String -> [MessageSpan] -> ServerPartE a
errNotFound      title message = throwError (ErrorResponse 404 title message)

errInternalError :: [MessageSpan] -> ServerPartE a
errInternalError       message = throwError (ErrorResponse 500 title message)
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
    fallbackHandler err = finishWith (result (errorCode err) message)
      where
        message = errorTitle err ++ ": " ++ messageToText (errorDetail err)

handleErrorResponse :: (ErrorResponse -> ServerPartE Response)
                    -> ServerPartE a -> ServerPartE a
handleErrorResponse handler action =
    catchError action (\errResp -> handler errResp >>= finishWith)
