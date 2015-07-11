
module Distribution.Server.Features.Sitemap.Functions
  ( nodesToSiteMap
  , makeURLNodes
  , makeURLNodes'
  ) where

import qualified Data.Map as Map
import Prelude hiding (writeFile)
import Data.Text
import Text.XML
import qualified Data.ByteString.Lazy as L
import qualified Network.URI as URI
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (Day(..), showGregorian)

data URLNode = URLNode {
  location :: String
, lastmod :: String
, changefreq :: String
, priority :: String
}


nodesToSiteMap :: [Node] -> L.ByteString
nodesToSiteMap nodes = renderLBS def (Document (Prologue [] Nothing []) root [])
  where
    root = Element (makeName "urlset") schemaAttr nodes

makeURLNodes :: [String] -> String -> [Node]
makeURLNodes urls priorityForAll =
  Prelude.map (packageToNode priorityForAll) urls

makeURLNodes' :: [(String, UTCTime)] -> String -> [Node]
makeURLNodes' urls priorityForAll = Prelude.map (packageToNode' priorityForAll) urls

noAttr :: Map.Map Name Text
noAttr = Map.empty

schemaAttr :: Map.Map Name Text
schemaAttr = Map.fromList [ (makeName "xmlns", pack "http://www.sitemaps.org/schemas/sitemap/0.9")]

packageToNode :: String -> String -> Node
packageToNode priorityForPkg pkgPath =
  let node = URLNode {
        location = pkgPath
      , lastmod = "2012-04-30"
      , changefreq = "monthly"
      , priority = priorityForPkg
      } in
  makeNodeURL node

packageToNode' :: String -> (String, UTCTime) -> Node
packageToNode' priorityForPkg nameAndDate =
  let node = URLNode {
        location = fst nameAndDate
      {-, lastmod = show . toModifiedJulianDay . utctDay . snd . nameAndDate-}
      , lastmod = showGregorian . utctDay . snd $ nameAndDate
      , changefreq = "monthly"
      , priority = priorityForPkg
      } in
      makeNodeURL node


makeNodeURL :: URLNode -> Node
makeNodeURL node =
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

makeNodeValue :: String -> [Node]
makeNodeValue val = [NodeContent (pack val)]

makeName :: String -> Name
makeName s = Name
  { nameLocalName = pack s
  , nameNamespace = Nothing
  , namePrefix = Nothing }
