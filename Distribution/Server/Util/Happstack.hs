
{-|

Functions and combinators to expose functioanlity buiding
on happstack bit is not really specific to any one area
of Hackage.

-}

module Distribution.Server.Util.Happstack (
    remainingPath,
    remainingPathString,
    mime
  ) where

import Happstack.Server
import qualified Happstack.Server.SURI as SURI
import qualified Data.Map as Map
import System.FilePath.Posix (takeExtension, (</>))
import Control.Monad (liftM)

-- |Passes a list of remaining path segments in the URL. Does not
-- include the query string. This call only fails if the passed in
-- handler fails. 
remainingPath :: Monad m => ([String] -> ServerPartT m a) -> ServerPartT m a
remainingPath handle = do
    rq <- askRq
    localRq (\newRq -> newRq{rqPaths=[]}) $ handle (rqPaths rq)

-- | Gets the string without altering the request.
remainingPathString :: Monad m => ServerPartT m String
remainingPathString = do
    strs <- liftM rqPaths askRq
    return $ if null strs then "" else foldr1 (</>) . map SURI.escape $ strs

-- |Returns a mime-type string based on the extension of the passed in
-- file.
mime :: FilePath -> String
mime x  = Map.findWithDefault "text/plain" (drop 1 (takeExtension x)) mimeTypes
