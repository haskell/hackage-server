{-# LANGUAGE FlexibleContexts #-}
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
    showContentType,

    -- * Working with headers
    alterResponseHeader,
    removeResponseHeader,

    -- * Response filters
    enableGZip,
    enableGZip',
    enableRange
  ) where

import Happstack.Server.FileServe
import Happstack.Server.Internal.Compression (encodings)
import Happstack.Server.Monads
import Happstack.Server.Response
import Happstack.Server.Types

import Control.Monad
import Data.Char (toLower)
import Data.Int (Int64)
import System.FilePath.Posix (takeExtension, (</>))
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BS.C8
import qualified Data.ByteString.Lazy   as BS.Lazy
import qualified Data.Map               as Map
import qualified Network.URI            as URI
import qualified Text.Parsec            as Parsec
import qualified Text.Parsec.ByteString as Parsec

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
mime x =
    Map.findWithDefault thedefault (drop 1 (takeExtension x)) mimeTypes'
  where
    thedefault      = "text/plain; charset=utf-8"
    mimeTypes'      = customMimeTypes `Map.union` mimeTypes
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
consumeRequestBody :: Happstack m => m BS.Lazy.ByteString
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

{-------------------------------------------------------------------------------
  Working with headers
-------------------------------------------------------------------------------}

-- | Alter a response header
--
-- If Happstack exported it's @HasHeaders@ type classes we could give a more
-- general implementation of this, but sadly it doesn't.
alterResponseHeader :: String
                    -> ([BS.ByteString] -> [BS.ByteString])
                    -> Response -> Response
alterResponseHeader nm f r = r { rsHeaders = aux (rsHeaders r) }
  where
    aux :: Headers -> Headers
    aux = Map.alter f' $ BS.C8.pack (map toLower nm)

    f' :: Maybe HeaderPair -> Maybe HeaderPair
    f' (Just (HeaderPair nm' vals)) = f'' nm'             $ f vals
    f' Nothing                      = f'' (BS.C8.pack nm) $ f []

    f'' :: BS.ByteString -> [BS.ByteString] -> Maybe HeaderPair
    f'' _   []   = Nothing
    f'' nm' vals = Just $ HeaderPair nm' vals

-- | Remove a response header
removeResponseHeader :: String -> Response -> Response
removeResponseHeader nm = alterResponseHeader nm (const [])

{-------------------------------------------------------------------------------
  Response filters
-------------------------------------------------------------------------------}

-- | Enable GZip content compression if the client requests it
--
-- This differs from 'compressedResponseFilter' in a number of ways:
--
-- 1. We only allow GZip compression
-- 2. We make the GZip compression level configurable
-- 3. We remove the Content-Length and Content-MD5 headers from the response
-- 4. We change the ETag header and set the corresponding Vary header
enableGZip :: (FilterMonad Response m, WebMonad Response m, ServerMonad m)
           => GZip.CompressParams
           -> (BS.ByteString -> BS.ByteString) -- ^ ETag modifier
           -> m ()
enableGZip compressParams modifyETag = do
    mHeader <- getHeaderM "Accept-Encoding"
    case fmap (Parsec.parse encodings "" . BS.C8.unpack) mHeader of
      Nothing           -> return ()
      Just (Left _)     -> finishWith $ result 400 "Invalid Accept-Encodings"
      Just (Right encs) -> do
        if "gzip" `elem` map fst encs
          then composeFilter gzipFilter
          else return ()
  where
    gzipFilter :: Response -> Response
    gzipFilter r@SendFile{} = r -- Leave files alone
    gzipFilter r@Response{} =
        chunked
      . setHeader "Content-Encoding" "gzip"
      . setHeader "Vary" "Accept-Encoding"
      . removeResponseHeader "Content-Length"
      . removeResponseHeader "Content-MD5"
      . alterResponseHeader "ETag" (map modifyETag)
      $ r { rsBody = GZip.compressWith compressParams $ rsBody r }

-- | Variation on 'enableGZip' with sensible defaults
--
-- We use the default compression parameters (with the exception of the c
-- compression level), and simply change any existing etag @"foo"@ to
-- @"foo-gzip"@.
enableGZip' :: (FilterMonad Response m, WebMonad Response m, ServerMonad m)
            => Int   -- ^ Compression level
            -> m ()
enableGZip' compressLevel =
    enableGZip compressParams modifyETag
  where
    modifyETag etag = BS.init etag `BS.append` BS.C8.pack "-gzip\""
    compressParams  = GZip.defaultCompressParams {
        GZip.compressLevel = GZip.compressionLevel compressLevel
      }

-- | Enable range requests for this resource
--
-- IMPORTANT: If using both 'enableGZip' and 'enableRange', 'enableRange'
-- MUST be called first.
enableRange :: (FilterMonad Response m, WebMonad Response m, ServerMonad m)
            => m ()
enableRange = do
     mRange <- getHeaderM "Range"
     case fmap (Parsec.parse parseRange "Range header") mRange of
       Nothing        -> composeFilter indicateAcceptRanges
       Just (Right r) -> composeFilter (rangeFilter r)
       Just (Left  _) -> finishWith $ result 400 "Invalid Range"
  where
    -- TODO: We don't check for ill-formed range requests (past the end of the
    -- file, 'to' before 'fr', negative, etc.). Checking this here is a little
    -- awkward; we'd have to parse the original Content-Length header to find
    -- out the original length.
    rangeFilter :: (Int64, Int64) -> Response -> Response
    rangeFilter (fr, to) r =
        setHeader "Content-Length" (show rangeLen)
      . setHeaderBS (BS.C8.pack "Content-Range") (contentRange fr to fullLen)
      . removeResponseHeader "Content-MD5"
      $ r { rsBody = BS.Lazy.take rangeLen $ BS.Lazy.drop fr $ rsBody r
          , rsCode = 206
          }
      where
        rangeLen = to - fr + 1
        fullLen  = getHeader "Content-Length" r

    contentRange :: Int64 -> Int64 -> Maybe BS.ByteString -> BS.ByteString
    contentRange fr to mFullLen = BS.concat [
        BS.C8.pack ("bytes " ++ show fr ++ "-" ++ show to ++ "/")
      , case mFullLen of
          Nothing      -> BS.C8.pack "*"
          Just fullLen -> fullLen
      ]

    indicateAcceptRanges :: Response -> Response
    indicateAcceptRanges = setHeader "Accept-Ranges" "bytes"

    -- TODO: We might have to make this parser more flexible (allow for
    -- whitespace, different capitalization, what have you).
    parseRange :: Parsec.Parser (Int64, Int64)
    parseRange = do
      Parsec.string "bytes="
      fr <- Parsec.many1 Parsec.digit
      Parsec.char '-'
      to <- Parsec.many1 Parsec.digit
      return (read fr, read to)
