module Distribution.Server.Error where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent.MVar
import Data.Maybe (fromMaybe)
import Happstack.Server

data ErrorResponse = ErrorResponse String Int [ErrorMessage]
data ErrorMessage = ELink String String | EText String

type MaybeResponse = Either ErrorResponse Response

textMaybeResponse :: ServerPart MaybeResponse -> ServerPart Response
textMaybeResponse mresp = case mresp of
    Left  err -> defaultError err
    Right res -> return res

defaultError (ErrorResponse _ errCode message) = resp errCode $ toResponse (errorToString message)

errorToString :: [ErrorMessage] -> String
errorToString [] = ""
errorToString (ELink x _:xs) = x ++ errorToString xs
errorToString (EText x  :xs) = x ++ errorToString xs

