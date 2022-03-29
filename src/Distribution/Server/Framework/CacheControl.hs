
-- | Handler helpers to use HTTP cache headers: @Cache-Control@ and @ETag@.
--
module Distribution.Server.Framework.CacheControl (
    cacheControl,
    cacheControlWithoutETag,
    CacheControl(..),
    ETag(..),
    etagFromHash,
    maxAgeSeconds, maxAgeMinutes, maxAgeHours, maxAgeDays, maxAgeMonths,
  ) where

import Prelude ()
import Distribution.Server.Prelude

import Happstack.Server.Types
import Happstack.Server.Monads

import Data.List
import qualified Data.ByteString.Char8 as BS8
import Data.Hashable
import Numeric

data CacheControl = MaxAge Int | Public | Private | NoCache | NoTransform

maxAgeSeconds, maxAgeMinutes, maxAgeHours,
  maxAgeDays, maxAgeMonths :: Int -> CacheControl
maxAgeSeconds n = MaxAge n
maxAgeMinutes n = MaxAge (n * 60)
maxAgeHours   n = MaxAge (n * 60 * 60)
maxAgeDays    n = MaxAge (n * 60 * 60 * 24)
maxAgeMonths  n = MaxAge (n * 60 * 60 * 24 * 30)

formatCacheControl :: CacheControl -> String
formatCacheControl (MaxAge n)  = "max-age=" ++ show n
formatCacheControl Public      = "public"
formatCacheControl Private     = "private"
formatCacheControl NoCache     = "no-cache"
formatCacheControl NoTransform = "no-transform"

-- | Adds a @Cache-Control@ and @ETag@ header to the response. Also handles the
-- @if-none-match@ request header by returning 304 if the request ETag matches.
cacheControl :: Monad m => [CacheControl] -> ETag -> ServerPartT m ()
cacheControl ctls etag = do
    setCacheControl ctls
    handleETag etag

-- | Just adds a @Cache-Control@. Whenever possible you should use
-- 'cacheControl' instead and supply an ETag.
cacheControlWithoutETag :: Monad m => [CacheControl] -> ServerPartT m ()
cacheControlWithoutETag ctls =
  setCacheControl ctls

-- | Set the Cache-Control header on the response
setCacheControl :: Monad m => [CacheControl] -> ServerPartT m ()
setCacheControl ctls =
    let hdr = intercalate ", " (map formatCacheControl ctls) in
    setHeaderM "Cache-Control" hdr

newtype ETag = ETag String
  deriving (Eq, Ord, Show)

formatETag :: ETag -> String
formatETag (ETag etag) = '"' : etag ++ ['"']

handleETag :: Monad m => ETag -> ServerPartT m ()
handleETag expectedtag = do
    -- Set the ETag header on the response.
    setHeaderM "ETag" (formatETag expectedtag)

    -- Check the request for a matching ETag, return 304 if found.
    rq <- askRq
    case getHeader "if-none-match" rq of
      Just actualtag | formatETag expectedtag == BS8.unpack actualtag
        -> finishWith (noContentLength . result 304 $ "")
      _ -> return ()

etagFromHash :: Hashable a => a -> ETag
etagFromHash x = ETag (showHex (fromIntegral (hash x) :: Word) "")

