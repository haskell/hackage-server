
-- | Handler helpers to use HTTP cache headers: @Cache-Control@ and @ETag@.
--
module Distribution.Server.Framework.CacheControl (
    ETag(..),
    useETag,
  ) where

import Happstack.Server.Types
import Happstack.Server.Monads

import Control.Monad
import qualified Data.ByteString.Char8 as BS8


newtype ETag = ETag String
  deriving (Eq, Ord, Show)

formatETag :: ETag -> String
formatETag (ETag etag) = '"' : etag ++ ['"']


-- | Adds an ETag to the response, returns 304 if the request ETag matches.
useETag :: Monad m => ETag -> ServerPartT m ()
useETag expectedtag = do
    -- Set the ETag field on the response.
    setHeaderM "ETag" (formatETag expectedtag)
    -- Check the request for a matching ETag, return 304 if found.
    rq <- askRq
    case getHeader "if-none-match" rq of
      Just etag -> checkETag (BS8.unpack etag)
      _ -> return ()
    where checkETag actualtag =
            when ((formatETag expectedtag) == actualtag) $
                finishWith (noContentLength . result 304 $ "")

