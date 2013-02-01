{-# LANGUAGE OverloadedStrings, PatternGuards #-}
-- | 
--
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.Framework.RequestContentTypes
-- Copyright   :  (c) Duncan Coutts 2012-2013
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for validating and consuming the body of requests for various
-- content types like plain text, tarballs, JSON etc.
-----------------------------------------------------------------------------
module Distribution.Server.Framework.RequestContentTypes (

    -- * checking mime types
    expectContentType,

    -- * various specific content types
    expectTextPlain,
    expectUncompressedTarball,
    expectCompressedTarball,
    expectJsonContent,
    expectAesonContent,

  ) where

import Happstack.Server
import Distribution.Server.Util.Happstack
import Distribution.Server.Framework.Error
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Codec.Compression.Zlib.Internal as GZip
import qualified Text.JSON  as JSON
import qualified Data.Aeson as Aeson

-- | Expect the request body to have the given mime type (exact match),
-- and to have either no content-encoding, or gzip encoding
-- (which is transparently decoded).
--
expectContentType :: BS.ByteString -> ServerPartE LBS.ByteString
expectContentType expected = do
    req <- askRq
    let contentType     = getHeader "Content-Type" req
        contentEncoding = getHeader "Content-Encoding" req
    case contentType of
      Just actual
        | actual == expected -> case contentEncoding of
           Nothing           -> consumeRequestBody
           Just enc
             | enc == "gzip" -> consumeRequestBody >>= gzipDecompress
           _                 -> wrongContentEncoding
        | otherwise          -> wrongContentType actual
      Nothing                -> missingContentType
  where
    missingContentType =
      errBadMediaType "Missing content-type"
        [MText "An HTTP content-type header was expected."]
    wrongContentType actual =
      errBadMediaType "Unexpected content-type"
        [MText $ "For this resource the content-type was expected to be "
              ++ BS.unpack expected ++ ", rather than " ++ BS.unpack actual]
    wrongContentEncoding =
      errBadMediaType "Unexpected content-encoding"
        [MText $ "The only content-encodings supported are gzip, or none at all."]

gzipDecompress :: LBS.ByteString -> ServerPartE LBS.ByteString
gzipDecompress content =
    case GZip.decompressWithErrors
           GZip.gzipFormat GZip.defaultDecompressParams content of
      GZip.StreamError errkind _ -> errDecompress errkind
      stream                     -> return (GZip.fromDecompressStream stream)
  where
    errDecompress GZip.TruncatedInput =
      errBadRequest "Truncated data upload"
        [MText $ "The uploaded data (gzip-compressed) is truncated. Check "
              ++ "your gzip data is complete, and try again."]
    errDecompress _ =
      errBadRequest "Corrupted upload"
        [MText $ "There is an error in the gzip encoding of the uploaded "
              ++ "data. Check that the uploaded data is compressed using "
              ++ "the gzip format."]

-- | Expect the request body to have mime type @text/plain@ and no
-- content-encoding.
--
expectTextPlain :: ServerPartE LBS.ByteString
expectTextPlain = expectContentType "text/plain"

-- | Expect an uncompressed @.tar@ file.
--
-- The tar file is not validated.
--
-- A content-encoding of \"gzip\" is handled transparently.
--
expectUncompressedTarball :: ServerPartE LBS.ByteString
expectUncompressedTarball = expectContentType "application/x-tar"

-- | Expect a compressed @.tar.gz@ file.
--
-- Neither the gzip encoding nor the tar format are validated.
--
-- Compressed tarballs are a little odd in that some clients send them
-- as mime type @application/x-gzip@ with no content-encoding, while others
-- use @application/x-tar@ with a @gzip@ content-encoding. We allow either.
--
expectCompressedTarball :: ServerPartE LBS.ByteString
expectCompressedTarball = do
    req <- askRq
    let contentType     = getHeader "Content-Type" req
        contentEncoding = getHeader "Content-Encoding" req
    case contentType of
      Just actual 
        | actual == "application/x-tar"
        , contentEncoding == Just "gzip" -> consumeRequestBody
        | actual == "application/x-gzip"
        , contentEncoding == Nothing     -> consumeRequestBody
      _                                  -> errExpectedTarball
  where
    errExpectedTarball =
      errBadMediaType "Expected a compressed tarball"
        [MText $ "Expected either content-type application/x-tar "
              ++ " (with a content-encoding of gzip) or application/x-gzip."]

expectJsonContent :: JSON.JSON a => ServerPartE a
expectJsonContent = do
  content <- expectContentType "application/json"
  case JSON.decodeStrict (LBS.unpack content) of
    JSON.Ok a      -> return a
    JSON.Error msg -> errBadRequest "Malformed request"
                        [MText $ "The JSON is malformed: " ++ msg]

expectAesonContent :: Aeson.FromJSON a => ServerPartE a
expectAesonContent = do
  content <- expectContentType "application/json"
  case Aeson.eitherDecode' content of
    Right a  -> return a
    Left msg -> errBadRequest "Malformed request"
                   [MText $ "The JSON data is not in the expected form: " ++ msg]
