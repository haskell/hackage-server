{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
-- | Provides several ways to construct an XML Sitemap.
--
--   Generally speaking, it is the user's responsibility to
--   construct a list of sitemap entries (i.e., [SitemapEntry]) and to call
--   'renderSitemap' on that list.
--
--   This can be done by manually populating a list using
--   the 'SitemapEntry' data type, and passing them to 'renderSitemap'.
--
--   If you have a variety of paths with common values for last modification,
--   change frequency, and priority, apply 'sitemapEntryToXml' to a list of
--   fully qualified URLs.
--   (e.g., ["http://yourhackage.com/package/blah", ...]
--          :: [String] )
--
--   If you have paths with differing dates for the last modification,
--   apply 'pathsAndDatesToSitemapEntries' to a list of tuples consisting of
--   fully qualified paths and UTCTimes.
--   (e.g., [("http://yourhackage.com/foo", "2012-04-30 20:31:07.123485 UTC"), ...]
--          :: [(String, UTCTime)] )
--
module Distribution.Server.Features.Sitemap.Functions (
    SitemapEntry
  , ChangeFreq(..)
  , renderSitemap
  , urlsToSitemapEntries
  , pathsAndDatesToSitemapEntries
  ) where

import Text.XML.Generator
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (showGregorian)
import Network.URI (URI)
import qualified Data.Text as T
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder as B


data SitemapEntry = SitemapEntry {
                 location   ::  String,
                 lastmod    :: !Text,
                 changefreq :: !ChangeFreq,
                 priority   :: !Float
               }

data ChangeFreq = Monthly | Weekly | Daily

-- | Primary function - generates the XML file from a list of Nodes.
renderSitemap :: URI -> [SitemapEntry] -> ByteString
renderSitemap serverBaseURI entries =
    xrender $
      doc defaultDocInfo $
        xelem "urlset" $
              xattr "xmlns" "http://www.sitemaps.org/schemas/sitemap/0.9"
          <#> map (sitemapEntryToXml serverBaseURI') entries
  where
    serverBaseURI' = T.pack (show serverBaseURI)

-- Render a SitemapEntry as an xml element (using xmlgen package)
sitemapEntryToXml :: Text -> SitemapEntry -> Xml Elem
sitemapEntryToXml serverBaseURI SitemapEntry{..} =
    xelem "url" $
      xelems [
        xelem "loc"        (xtext (serverBaseURI <> T.pack location))
      , xelem "lastmod"    (xtext lastmod)
      , xelem "changefreq" (xtext (renderChangeFreq changefreq))
      , xelem "priority"   (xtextRaw (renderPriority priority))
      ]
  where
    renderChangeFreq :: ChangeFreq -> Text
    renderChangeFreq Monthly = "monthly"
    renderChangeFreq Weekly  = "weekly"
    renderChangeFreq Daily   = "daily"
    renderPriority = B.floatDec


-- | Make nodes with differing paths, but common lastmod\/changefreq\/priority
-- values.
urlsToSitemapEntries :: [String] -> Text -> ChangeFreq -> Float
                     -> [SitemapEntry]
urlsToSitemapEntries urls lastModAll changeFreqAll priorityAll =
    [ SitemapEntry {
        location   = url,
        lastmod    = lastModAll,
        changefreq = changeFreqAll,
        priority   = priorityAll
      }
    | url <- urls ]

-- | Make nodes with differeing paths and modification times, but common
-- | change frequencies and priorities.
pathsAndDatesToSitemapEntries :: [(String, UTCTime)] -> ChangeFreq -> Float
                              -> [SitemapEntry]
pathsAndDatesToSitemapEntries urldates changeFreqAll priorityAll =
    [ SitemapEntry {
        location   = url,
        lastmod    = T.pack (showGregorian (utctDay date)),
        changefreq = changeFreqAll,
        priority   = priorityAll
      }
    | (url, date) <- urldates ]

