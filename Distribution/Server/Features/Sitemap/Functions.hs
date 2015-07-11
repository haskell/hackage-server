
module Distribution.Server.Features.Sitemap.Functions
  ( nodesToSiteMap
  , makeURLNodes
  ) where

import qualified Data.Map as Map
import Prelude hiding (writeFile)
import Data.Text
import Text.XML
import qualified Data.ByteString.Lazy as L
import qualified Network.URI as URI


nodesToSiteMap :: [Node] -> L.ByteString
nodesToSiteMap nodes = renderLBS def (Document (Prologue [] Nothing []) root [])
  where
    root = Element (makeName "urlset") schemaAttr nodes

makeURLNodes :: [String] -> URI.URI -> String -> [Node]
makeURLNodes xs baseuri priority = Prelude.map (packageToNode baseuri priority) xs

noAttr :: Map.Map Name Text
noAttr = Map.empty

schemaAttr :: Map.Map Name Text
schemaAttr = Map.fromList [ (makeName "xmlns", pack "http://www.sitemaps.org/schemas/sitemap/0.9")]

packageToNode :: URI.URI -> String -> String -> Node
packageToNode baseuri priority pkgname =
  makeNodeURL (show baseuri ++ pkgname) "2012-04-30" "monthly" priority


makeNodeURL :: String -> String -> String -> String -> Node
makeNodeURL loc lastmod changefreq priority =
          NodeElement $ Element (makeName "url") noAttr
            [ NodeElement $ Element (makeName "loc") noAttr
                (makeNodeValue loc)
            , NodeElement $ Element (makeName "lastmod") noAttr
                (makeNodeValue lastmod)
            , NodeElement $ Element (makeName "changefreq") noAttr
                (makeNodeValue changefreq)
            , NodeElement $ Element (makeName "priority") noAttr
                (makeNodeValue priority)
            ]

makeNodeValue :: String -> [Node]
makeNodeValue val = [NodeContent (pack val)]

makeName :: String -> Name
makeName s = Name
  { nameLocalName = pack s
  , nameNamespace = Nothing
  , namePrefix = Nothing }
