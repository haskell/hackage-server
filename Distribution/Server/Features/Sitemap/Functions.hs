-- | Provides several ways to construct an XML Sitemap.
--
-- | Generally speaking, it is the user's responsibility to
-- | construct a list of Nodes (i.e., [Node]) and to call
-- | 'nodesToSiteMap' on that list.
--
-- | This can be done by manually populating a list using
-- | the URLNode data type, transforming them into Nodes
-- | with 'urlNodeToXMLElement', and passing them to 'nodesToSiteMap'.
--
-- | If you have a variety of paths with common values for last modification,
-- | change frequency, and priority, apply 'urlsToNodes' to a list of
-- | fully qualified URLs.
-- | (e.g., ["http://yourhackage.com/package/blah", ...]
-- |        :: [String] )
--
-- | If you have paths with differing dates for the last modification,
-- | apply "pathsAndDatesToNodes" to a list of tuples consisting of
-- | fully qualified paths and UTCTimes.
-- | (e.g., [("http://yourhackage.com/foo", "2012-04-30 20:31:07.123485 UTC"), ...]
-- |        :: [(String, UTCTime)] )

module Distribution.Server.Features.Sitemap.Functions
  ( nodesToSiteMap
  , urlsToNodes
  , pathsAndDatesToNodes
  ) where

import Text.XML
import Data.Text

import qualified Data.Map as Map
import Prelude hiding (writeFile)

import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (showGregorian)

import qualified Data.ByteString.Lazy as L

data URLNode = URLNode {
  location :: String
, lastmod :: String
, changefreq :: String
, priority :: String
}

-- | Primary function - generates the XML file from a list of Nodes.
nodesToSiteMap :: [Node] -> L.ByteString
nodesToSiteMap nodes = renderLBS def (Document (Prologue [] Nothing []) root [])
  where
    root = Element (makeName "urlset") schemaAttr nodes

-- | Make nodes with differing paths, but common lastmod/changefreq/priority values.
urlsToNodes :: [String] -> String -> String -> String -> [Node]
urlsToNodes urls lastModAll changeFreqAll priorityAll =
  Prelude.map (createURLNode lastModAll changeFreqAll priorityAll) urls

-- Take url/location as last argument to facilitate mapping over lists.
createURLNode :: String -> String -> String -> String -> Node
createURLNode l cf p url =
  let node = URLNode {
        location = url
      , lastmod = l
      , changefreq = cf
      , priority = p
      } in
  urlNodeToXMLElement node

-- | Make nodes with differeing paths and modification times, but common
-- | change frequencies and priorities.
pathsAndDatesToNodes :: [(String, UTCTime)] -> String -> String -> [Node]
pathsAndDatesToNodes urls changeFreqAll priorityAll =
  Prelude.map (createURLNode' changeFreqAll priorityAll) urls

-- Take url and lastmod time tuple as last argument to facilitate mapping.
createURLNode' :: String -> String -> (String, UTCTime) -> Node
createURLNode' cf p urlAndDate =
  let node = URLNode {
        location = fst urlAndDate
      , lastmod = showGregorian . utctDay . snd $ urlAndDate
      , changefreq = cf
      , priority = p
      } in
      urlNodeToXMLElement node


-- | Local Functions

-- Unwraps a URLNode and creates a NodeElement (from xml-conduit)
urlNodeToXMLElement :: URLNode -> Node
urlNodeToXMLElement node =
          NodeElement $ Element (makeName "url") noAttr
            [ NodeElement $ Element (makeName "loc") noAttr
                (makeNodeValue $ location node)
            , NodeElement $ Element (makeName "lastmod") noAttr
                (makeNodeValue $ lastmod node)
            , NodeElement $ Element (makeName "changefreq") noAttr
                (makeNodeValue $ changefreq node)
            , NodeElement $ Element (makeName "priority") noAttr
                (makeNodeValue $ priority node)
            ]


-- | Helper Functions

-- Empty set of attributes for an XML Element
noAttr :: Map.Map Name Text
noAttr = Map.empty

-- Attributes needed for an XML Sitemap
schemaAttr :: Map.Map Name Text
schemaAttr = Map.fromList [
  ( makeName "xmlns"
  , pack "http://www.sitemaps.org/schemas/sitemap/0.9"
  )]

-- xml-conduit expects the actual value of a given element
-- (i.e., what goes between the element's tags) to be typed as
-- 'NodeContent Text', so this function abstracts that into a String.
makeNodeValue :: String -> [Node]
makeNodeValue val = [NodeContent (pack val)]

-- xml-conduit allows for fully-qualified namespaces of type 'Name'.
-- For a simple sitemap, we only need local names, so this function
-- abstracts the global namespace and prefix away.
makeName :: String -> Name
makeName s = Name
  { nameLocalName = pack s
  , nameNamespace = Nothing
  , namePrefix = Nothing }
