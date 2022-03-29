{-# LANGUAGE OverloadedStrings, PatternGuards #-}
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
    expectAesonContent,
    expectCSV,

  ) where

import Happstack.Server
import Distribution.Server.Framework.HappstackUtils
import Distribution.Server.Framework.Error
import qualified Data.ByteString.Char8 as BS (ByteString, empty, unpack) -- Used for content-type headers only
import qualified Data.ByteString.Lazy as LBS (ByteString, empty)
import qualified Data.ByteString.Lazy.Internal as LBS (ByteString (..))
import qualified Codec.Compression.Zlib.Internal as GZip
import Control.Monad.IO.Class (liftIO)
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
gzipDecompress content = go content decompressor
  where
    decompressor :: GZip.DecompressStream IO
    decompressor = GZip.decompressIO GZip.gzipFormat GZip.defaultDecompressParams

    go :: LBS.ByteString -> GZip.DecompressStream IO -> ServerPartE LBS.ByteString
    go cs (GZip.DecompressOutputAvailable bs k) = do
        stream <- liftIO k
        LBS.Chunk bs `fmap` go cs stream
    go _  (GZip.DecompressStreamEnd      _bs)   = return LBS.empty
    go _  (GZip.DecompressStreamError err)      = errDecompress err
    go cs (GZip.DecompressInputRequired      k) = do
        let ~(c, cs') = uncons cs
        liftIO (k c) >>= go cs'

    uncons :: LBS.ByteString -> (BS.ByteString, LBS.ByteString)
    uncons LBS.Empty        = (BS.empty, LBS.Empty)
    uncons (LBS.Chunk c cs) = (c, cs)

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

expectAesonContent :: Aeson.FromJSON a => ServerPartE a
expectAesonContent = do
  content <- expectContentType "application/json"
  case Aeson.eitherDecode' content of
    Right a  -> return a
    Left msg -> errBadRequest "Malformed request"
                   [MText $ "The JSON data is not in the expected form: " ++ msg]

expectCSV :: ServerPartE LBS.ByteString
expectCSV = expectContentType "text/csv"
