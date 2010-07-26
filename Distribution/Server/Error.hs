module Distribution.Server.Error where

import Control.Monad.Error
import Happstack.Server

-- | A type for generic error reporting that should be sufficient for
-- most purposes.
data ErrorResponse = ErrorResponse {
    errorCode  :: Int,
    errorTitle :: String,
    errorMessage :: [Message]
}

-- | A message with hypertext. (MLink str href) will be taken
-- as a pointer to a relevant page, and (MText str) merely
-- as text. In formats which don't have hypertext mixed with
-- regular text, the links and text can be obtained separately
-- with unzipError.
data Message = MLink String String | MText String

messageToString :: [Message] -> String
messageToString [] = ""
messageToString (MLink x _:xs) = x ++ messageToString xs
messageToString (MText x  :xs) = x ++ messageToString xs

unzipMessage :: [Message] -> (String, [String])
unzipMessage = foldr go ("", [])
  where go m ~(text, links) = case m of
            MLink x k -> (x++text, k:links)
            MText x   -> (x++text, links)

instance Error ErrorResponse where 
    strMsg str = ErrorResponse 400 "Server error" [MText str]

-- | Simple synonym to indicate possible format-generic failure.
type MServerPart a = ServerPart (Either ErrorResponse a)

-- | Creates a server part with an OK response code.
returnOk :: a -> MServerPart a
returnOk = ok . Right

-- | Creates a server part with the error and sets the HTTP response code.
returnError :: Int -> String -> [Message] -> MServerPart a
returnError errCode title message = returnError' $ ErrorResponse errCode title message

-- | Creates an error ServerPart directly from an ErrorResponse object.
returnError' :: ErrorResponse -> MServerPart a
returnError' res = resp (errorCode res) $ Left res
------------------------------------------------------------------------
textResponse :: MServerPart Response -> ServerPart Response
textResponse mpart = mpart >>= \mres -> case mres of
    Left  err -> makeTextError err
    Right res -> return res

-- | This is the monadic bind operator for MServerPart. It is still useful to
-- pass an MServerPart a into a function which expects a ServerPart a, but at
-- other times the Either needs to be abstracted over. This function should be
-- useful in a few odd cases.
responseWith :: MServerPart a -> (a -> MServerPart b) -> MServerPart b
responseWith mpart func = mpart >>= \mres -> case mres of
    Left  err -> return $ Left err
    Right res -> func res

makeTextError :: ErrorResponse -> ServerPart Response
makeTextError (ErrorResponse errCode _ message) = resp errCode $ toResponse (messageToString message ++ "\n")

