{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
         ( BlobId, blobMd5Digest )
import Distribution.Server.Framework.MemSize
import Distribution.Server.Framework.Instances ()
import Distribution.Server.Util.Parse (packUTF8)
import Distribution.Server.Features.Security.MD5
import Distribution.Server.Features.Security.SHA256

import Happstack.Server
         ( ToMessage(..), Response(..), RsFlags(..), Length(NoContentLength), nullRsFlags, mkHeaders
         , noContentLength )

import qualified Data.ByteString.Lazy  as BS.Lazy
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Base64 as Base64
import Text.RSS (RSS)
import qualified Text.RSS as RSS (rssToXML, showXML)
import qualified Text.XHtml.Strict as XHtml (Html, showHtml)
import qualified Data.Aeson as Aeson (Value, encode)
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Format as Time (formatTime)
import Data.Time.Format (defaultTimeLocale)
import Text.CSV (printCSV, CSV)
import Control.DeepSeq

-- | The index tarball
--
-- We cache the compressed and uncompressed incremental tarball, as well as
-- the compressed legacy tarball for cabal's benefit
data IndexTarballInfo = IndexTarballInfo {
      indexTarballLegacyGz :: !TarballCompressed
    , indexTarballIncremUn :: !TarballUncompressed
    , indexTarballIncremGz :: !TarballCompressed
    }

data TarballUncompressed = TarballUncompressed {
      tarContent    :: !BS.Lazy.ByteString
    , tarLength     :: !Int
    , tarHashMD5    :: !MD5Digest
    , tarHashSHA256 :: !SHA256Digest
    , tarModified   :: !UTCTime
    }

data TarballCompressed = TarballCompressed {
      tarGzContent    :: !BS.Lazy.ByteString
    , tarGzLength     :: !Int
    , tarGzHashMD5    :: !MD5Digest
    , tarGzHashSHA256 :: !SHA256Digest
    , tarGzModified   :: !UTCTime
    }

mkTarballUncompressed :: UTCTime -> BS.Lazy.ByteString -> TarballUncompressed
mkTarballUncompressed time indexTarball = TarballUncompressed {
      tarContent    = indexTarball
    , tarLength     = indexTarballLen
    , tarHashMD5    = indexTarballMD5
    , tarHashSHA256 = indexTarballSHA256
    , tarModified   = time
    }
  where
    indexTarballLen    = fromIntegral $ BS.Lazy.length indexTarball
    indexTarballMD5    = md5 indexTarball
    indexTarballSHA256 = sha256 indexTarball

mkTarballCompressed :: UTCTime -> BS.Lazy.ByteString -> TarballCompressed
mkTarballCompressed time indexTarballGz = TarballCompressed {
      tarGzContent    = indexTarballGz
    , tarGzLength     = indexTarballGzLen
    , tarGzHashMD5    = indexTarballGzMD5
    , tarGzHashSHA256 = indexTarballGzSHA256
    , tarGzModified   = time
    }
  where
    indexTarballGzLen    = fromIntegral $ BS.Lazy.length indexTarballGz
    indexTarballGzMD5    = md5 indexTarballGz
    indexTarballGzSHA256 = sha256 indexTarballGz

instance NFData IndexTarballInfo where
  rnf (IndexTarballInfo a b c) = rnf (a, b, c)

instance NFData TarballUncompressed where
  rnf (TarballUncompressed a b c d e) = rnf (a, b, c, d, e)

instance NFData TarballCompressed where
  rnf (TarballCompressed a b c d e) = rnf (a, b, c, d, e)

instance MemSize IndexTarballInfo where
  memSize (IndexTarballInfo a b c) = memSize3 a b c

instance MemSize TarballUncompressed where
  memSize (TarballUncompressed a b c d e) = memSize5 a b c d e

instance MemSize TarballCompressed where
  memSize (TarballCompressed a b c d e) = memSize5 a b c d e

instance ToMessage TarballCompressed where
  toResponse TarballCompressed{..} =
    mkResponseLen tarGzContent tarGzLength
      [ ("Content-Type", "application/x-gzip")
      , ("Content-MD5",   formatMD5Digest tarGzHashMD5)
      , ("Last-modified", formatLastModifiedTime tarGzModified)
      ]

instance ToMessage TarballUncompressed where
  toResponse TarballUncompressed{..} =
    mkResponseLen tarContent tarLength
      [ ("Content-Type", "application/x-tar")
      , ("Content-MD5",   formatMD5Digest tarHashMD5)
      , ("Last-modified", formatLastModifiedTime tarModified)
      ]

data PackageTarball = PackageTarball BS.Lazy.ByteString BlobId UTCTime

instance ToMessage PackageTarball where
  toResponse (PackageTarball bs blobid time) = mkResponse bs
    [ ("Content-Type",  "application/x-gzip")
    , ("Content-MD5",   formatMD5Digest (blobMd5Digest blobid))
    , ("Last-modified", formatLastModifiedTime time)
    ]

data DocTarball = DocTarball BS.Lazy.ByteString BlobId

instance ToMessage DocTarball where
  toResponse (DocTarball bs blobid) = mkResponse bs
    [ ("Content-Type",  "application/x-tar")
    , ("Content-MD5",   formatMD5Digest (blobMd5Digest blobid))
    ]

-- | Format an 'MD5Digest' in Base64 as required for the \"Content-MD5\" header.
formatMD5Digest :: MD5Digest -> String
formatMD5Digest = BS.Char8.unpack . Base64.encode . md5DigestBytes

formatLastModifiedTime :: UTCTime -> String
formatLastModifiedTime = Time.formatTime defaultTimeLocale rfc822DateFormat
  where
    -- HACK! we're using UTC but http requires GMT
    -- hopefully it's ok to just say it's GMT
    rfc822DateFormat = "%a, %d %b %Y %H:%M:%S GMT"

newtype OpenSearchXml = OpenSearchXml BS.Lazy.ByteString

instance ToMessage OpenSearchXml where
    toContentType _ = "application/opensearchdescription+xml"
    toMessage (OpenSearchXml bs) = bs

instance ToMessage Aeson.Value where
    toContentType _ = "application/json"
    toMessage val = Aeson.encode val

data CabalFile = CabalFile !BS.Lazy.ByteString !UTCTime

instance ToMessage CabalFile where
    toResponse (CabalFile bs time) = mkResponse bs
      [ ("Content-Type",  "text/plain; charset=utf-8")
      , ("Last-modified", formatLastModifiedTime time)
      ]

newtype BuildLog = BuildLog BS.Lazy.ByteString

instance ToMessage BuildLog where
    toContentType _ = "text/plain"
    toMessage (BuildLog bs) = bs

newtype BuildCovg = BuildCovg BS.Lazy.ByteString

instance ToMessage BuildCovg where
    toContentType _ = "text/plain"
    toMessage (BuildCovg bs) = bs

instance ToMessage RSS where
    toContentType _ = "application/rss+xml"
    toMessage = packUTF8 . RSS.showXML . RSS.rssToXML

newtype XHtml = XHtml XHtml.Html

instance ToMessage XHtml where
    toContentType _ = "text/html; charset=utf-8"
    toMessage (XHtml xhtml) = packUTF8 (XHtml.showHtml xhtml)

-- Like XHtml, but don't bother calculating length
newtype LongXHtml = LongXHtml XHtml.Html

instance ToMessage LongXHtml where
    toResponse (LongXHtml xhtml) = noContentLength $ mkResponse
        (packUTF8 (XHtml.showHtml xhtml))
        [("Content-Type", "text/html")]

newtype ExportTarball = ExportTarball BS.Lazy.ByteString

instance ToMessage ExportTarball where
    toResponse (ExportTarball bs)
        = noContentLength $ mkResponse bs
          [("Content-Type",  "application/gzip")]

newtype CSVFile = CSVFile CSV

instance ToMessage CSVFile where
    toContentType _ = "text/csv"
    toMessage (CSVFile csv) = packUTF8 (printCSV csv)

newtype XMLResponse = XMLResponse BS.Lazy.ByteString
  deriving (MemSize, NFData)

instance ToMessage XMLResponse where
  toContentType _ = "application/xml"
  toMessage (XMLResponse bs) = bs

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
