
{-|

Functions and combinators to expose functioanlity buiding
on happstack bit is not really specific to any one area
of Hackage.

-}

module Distribution.Server.Framework.HappstackUtils (
    remainingPath,
    remainingPathString,
    mime,
    consumeRequestBody,

    uriEscape,
    showContentType
  ) where

import Happstack.Server.Types
import Happstack.Server.Monads
import Happstack.Server.Response
import Happstack.Server.FileServe

import qualified Data.Map as Map
import System.FilePath.Posix (takeExtension, (</>))
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import qualified Network.URI as URI


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
    return $ if null strs then "" else foldr1 (</>) . map uriEscape $ strs

-- This disappeared from happstack in 7.1.7
uriEscape :: String -> String
uriEscape = URI.escapeURIString URI.isAllowedInURI

-- |Returns a mime-type string based on the extension of the passed in
-- file.
mime :: FilePath -> String
mime x  = Map.findWithDefault "text/plain; charset=utf-8" (drop 1 (takeExtension x)) mimeTypes'
  where
    mimeTypes' = customMimeTypes `Map.union` mimeTypes
    customMimeTypes = Map.fromList
      [ ("xhtml", "application/xhtml+xml; charset=utf-8")
      , ("html" , "text/html; charset=utf-8")
      , ("cabal", "text/plain; charset=utf-8")
      , ("hs",  "text/plain; charset=utf-8")
      , ("lhs", "text/plain; charset=utf-8")
      , ("hsc", "text/plain; charset=utf-8")
      , ("chs", "text/plain; charset=utf-8")
      , ("c",  " text/plain; charset=utf-8")
      , ("h",  " text/plain; charset=utf-8")
      ]



-- | Get the raw body of a PUT or POST request.
--
-- Note that for performance reasons, this consumes the data and it cannot be
-- called twice.
--
consumeRequestBody :: Happstack m => m BS.ByteString
consumeRequestBody = do
    mRq <- takeRequestBody =<< askRq
    case mRq of
      Nothing -> escape $ internalServerError $ toResponse
                   "consumeRequestBody cannot be called more than once."
      Just (Body b) -> return b

-- The following functions are in happstack-server, but not exported. So we
-- copy them here.

-- | Produce the standard string representation of a content-type,
--   e.g. \"text\/html; charset=ISO-8859-1\".
showContentType :: ContentType -> String
showContentType (ContentType x y ps) = x ++ "/" ++ y ++ showParameters ps

-- | Helper for 'showContentType'.
showParameters :: [(String,String)] -> String
showParameters = concatMap f
    where f (n,v) = "; " ++ n ++ "=\"" ++ concatMap esc v ++ "\""
          esc '\\' = "\\\\"
          esc '"'  = "\\\""
          esc c | c `elem` ['\\','"'] = '\\':[c]
                | otherwise = [c]
