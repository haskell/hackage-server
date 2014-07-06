
{-|

Functions and combinators to expose functioanlity buiding
on happstack bit is not really specific to any one area
of Hackage.

-}

module Distribution.Server.Util.Happstack (
    remainingPath,
    remainingPathString,
    mime,
    consumeRequestBody,

    ETag(..),
    formatETag,
    useETag,

    uriEscape
  ) where

import Happstack.Server
import qualified Data.Map as Map
import System.FilePath.Posix (takeExtension, (</>))
import Control.Monad
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BS8
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
mime x  = Map.findWithDefault "text/plain" (drop 1 (takeExtension x)) mimeTypes


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


newtype ETag = ETag String
  deriving (Eq, Ord, Show)

formatETag :: ETag -> String
formatETag (ETag etag) = '"' : etag ++ ['"']


-- | Adds an ETag to the response, returns 304 if the request ETag matches.
useETag :: Monad m => ETag -> ServerPartT m ()
useETag expectedtag = do
    -- Set the ETag field on the response.
    composeFilter $ setHeader "ETag" (formatETag expectedtag)
    -- Check the request for a matching ETag, return 304 if found.
    rq <- askRq
    case getHeader "if-none-match" rq of
      Just etag -> checkETag (BS8.unpack etag)
      _ -> return ()
    where checkETag actualtag =
            when ((formatETag expectedtag) == actualtag) $
                finishWith (noContentLength . result 304 $ "")
