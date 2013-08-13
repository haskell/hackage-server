{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, StandaloneDeriving, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.Framework.ResponseContentTypes
-- Copyright   :  (c) David Himmelstrup 2008
--                    Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Types for various kinds of resources we serve, xml, package tarballs etc.
-----------------------------------------------------------------------------
module Distribution.Server.Framework.ResponseContentTypes where

import Distribution.Server.Framework.BlobStorage
         ( BlobId, blobMd5 )

import Happstack.Server
         ( ToMessage(..), Response(..), RsFlags(..), Length(NoContentLength), nullRsFlags, mkHeaders
         , noContentLength )

import qualified Data.ByteString.Lazy as BS.Lazy
import Text.RSS (RSS)
import qualified Text.RSS as RSS (rssToXML, showXML)
import qualified Text.XHtml.Strict as XHtml (Html, renderHtml)
import qualified Data.Aeson as Aeson (Value, encode)
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Format as Time (formatTime)
import System.Locale (defaultTimeLocale)
import Text.CSV (printCSV, CSV)

import qualified Data.Text.Lazy          as Text
import qualified Data.Text.Lazy.Encoding as Text

newtype ETag = ETag String
  deriving (Eq, Ord, Show)

blobETag :: BlobId -> ETag
blobETag = ETag . blobMd5

formatETag :: ETag -> String
formatETag (ETag etag) = '"' : etag ++ ['"']

data IndexTarball = IndexTarball BS.Lazy.ByteString

instance ToMessage IndexTarball where
  toContentType _ = "application/x-gzip"
  toMessage (IndexTarball bs) = bs


data PackageTarball = PackageTarball BS.Lazy.ByteString BlobId UTCTime

instance ToMessage PackageTarball where
  toResponse (PackageTarball bs blobid time) = mkResponse bs
    [ ("Content-Type",  "application/x-gzip")
    , ("Content-MD5",   md5sum)
    , ("ETag",          formatETag (ETag md5sum))
    , ("Last-modified", formatTime time)
    ]
    where md5sum = blobMd5 blobid

data DocTarball = DocTarball BS.Lazy.ByteString BlobId

instance ToMessage DocTarball where
  toResponse (DocTarball bs blobid) = mkResponse bs
    [ ("Content-Type",  "application/x-tar")
    , ("Content-MD5",   md5sum)
    , ("ETag",          formatETag (ETag md5sum))
    ]
    where md5sum = blobMd5 blobid

formatTime :: UTCTime -> String
formatTime = Time.formatTime defaultTimeLocale rfc822DateFormat
  where
    -- HACK! we're using UTC but http requires GMT
    -- hopefully it's ok to just say it's GMT
    rfc822DateFormat = "%a, %d %b %Y %H:%M:%S GMT"

newtype OpenSearchXml = OpenSearchXml BS.Lazy.ByteString

instance ToMessage OpenSearchXml where
    toContentType _ = "application/opensearchdescription+xml"
    toMessage (OpenSearchXml bs) = bs

instance ToMessage Aeson.Value where
    toContentType _ = "application/json; charset=utf-8"
    toMessage val = Aeson.encode val

newtype CabalFile = CabalFile BS.Lazy.ByteString

instance ToMessage CabalFile where
    toContentType _ = "text/plain; charset=utf-8"
    toMessage (CabalFile bs) = bs

newtype BuildLog = BuildLog BS.Lazy.ByteString

instance ToMessage BuildLog where
    toContentType _ = "text/plain"
    toMessage (BuildLog bs) = bs

instance ToMessage RSS where
    toContentType _ = "application/rss+xml"
    toMessage = stringToBytes . RSS.showXML . RSS.rssToXML

newtype XHtml = XHtml XHtml.Html

instance ToMessage XHtml where
    toContentType _ = "text/html; charset=utf-8"
    toMessage (XHtml xhtml) = stringToBytes (XHtml.renderHtml xhtml)

-- Like XHtml, but don't bother calculating length
newtype LongXHtml = LongXHtml XHtml.Html

instance ToMessage LongXHtml where
    toResponse (LongXHtml xhtml) = noContentLength $ mkResponse
        (stringToBytes (XHtml.renderHtml xhtml))
        [("Content-Type", "text/html")]

newtype ExportTarball = ExportTarball BS.Lazy.ByteString

instance ToMessage ExportTarball where
    toResponse (ExportTarball bs)
        = noContentLength $ mkResponse bs
          [("Content-Type",  "application/gzip")]

newtype CSVFile = CSVFile CSV

instance ToMessage CSVFile where
    toContentType _ = "text/csv"
    toMessage (CSVFile csv) = stringToBytes (printCSV csv)

mkResponse :: BS.Lazy.ByteString -> [(String, String)] -> Response
mkResponse bs headers = Response {
    rsCode    = 200,
    rsHeaders = mkHeaders headers,
    rsFlags   = nullRsFlags,
    rsBody    = bs,
    rsValidator = Nothing
  }

mkResponseLen :: BS.Lazy.ByteString -> Int -> [(String, String)] -> Response
mkResponseLen bs len headers = Response {
    rsCode    = 200,
    rsHeaders = mkHeaders (("Content-Length", show len) : headers),
    rsFlags   = nullRsFlags { rsfLength = NoContentLength },
    rsBody    = bs,
    rsValidator = Nothing
  }

stringToBytes :: String -> BS.Lazy.ByteString
stringToBytes = Text.encodeUtf8 . Text.pack
