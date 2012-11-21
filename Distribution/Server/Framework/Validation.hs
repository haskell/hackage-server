-- | Simple common utilities for request validation
--
module Distribution.Server.Framework.Validation (

    -- * checking mime types
    expectMimeType,
    expectTextPlain,
    expectTarball,

    -- * checking content
    expectJsonContent

  ) where

import Happstack.Server
import Distribution.Server.Util.Happstack
import Distribution.Server.Framework.Error
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Text.JSON

-- | Expect the request body to have the given mime type (exact match),
-- and to have no content-encoding.
--
expectMimeType :: BS.ByteString -> ServerPartE ()
expectMimeType expected = do
    req <- askRq
    let contentType     = getHeader "Content-Type" req
        contentEncoding = getHeader "Content-Encoding"  req
    case (contentType, contentEncoding) of
      (Just actual, Nothing) | actual == expected -> return ()
      _ -> finishWith =<< resp 415 (toResponse errMessage)
  where
    errMessage = "expected mime type " ++ BS.unpack expected

-- | Expect the request body to have mime type @text/plain@ and no
-- content-encoding.
--
expectTextPlain :: ServerPartE ()
expectTextPlain = expectMimeType (BS.pack "text/plain")


-- | Expect a compressed @.tar.gz@ file.
--
-- Compressed tarballs are a little odd in that some clients send them
-- as mime type @application/x-gzip@ with no content-encoding, while others
-- use @application/x-tar@ with a @gzip@ content-encoding. This check allows
-- either.
--
expectTarball :: ServerPartE ()
expectTarball = do
  req <- askRq
  let contentType     = fmap BS.unpack (getHeader "Content-Type" req)
      contentEncoding = fmap BS.unpack (getHeader "Content-Encoding" req)
  case (contentType, contentEncoding) of
    (Just "application/x-tar", Just "gzip") -> return ()
    (Just "application/x-gzip", Nothing)    -> return ()
    _ -> finishWith =<< resp 415 (toResponse "expected application/x-tar or x-gzip")

expectJsonContent :: ServerPartE JSValue
expectJsonContent = do
  expectMimeType (BS.pack "application/json")
  Body contents <- consumeRequestBody
  case decodeStrict (LBS.unpack contents) of
    Ok a      -> return a
    Error msg -> finishWith =<< resp 400 (toResponse $ "malformed JSON: " ++ msg)

