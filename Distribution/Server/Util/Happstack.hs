
{-|

Functions and combinators to expose functioanlity buiding
on happstack bit is not really specific to any one area
of Hackage.

-}

module Distribution.Server.Util.Happstack
    ( remainingPath
    , mime
    ) where

import Happstack.Server
import qualified Data.Map as Map
import System.FilePath.Posix (takeExtension)

-- |Passes a list of remaining path segments in the URL. Does not
-- include the query string. This call only fails if the passed in
-- handler fails. 
remainingPath :: ([String] -> ServerPart a) -> ServerPart a
remainingPath handle = do
  rq <- askRq
  localRq (\newRq -> newRq{rqPaths=[]}) $ handle (rqPaths rq)

-- |Returns a mime-type string based on the extension of the passed in
-- file.
mime :: FilePath -> String
mime x  = Map.findWithDefault "text/plain" (drop 1 (takeExtension x)) mimeTypes
