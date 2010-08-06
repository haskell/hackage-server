module Distribution.Server.Util.TextSearch (
    TextSearch(..),
    constructTextIndex,
    searchText
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Search
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char
import Data.Maybe (catMaybes)

-- Basic full text search. This works best when there are plenty of entries
-- and all of them are short. I'd use something like Hayoo here, but there's
-- no easy way to integrate it into the site.
--
-- At present this uses Bayer-Moore. Something with multiple search keys
-- might be more flexible. Or, even better, a Lucene-like engine.
data TextSearch = TextSearch {
    fullText :: ByteString,
    textIndex :: Map Int (String, String)
} deriving Show

constructTextIndex :: [(String, String)] -> TextSearch
constructTextIndex strs = case go strs 0 of
    (bs, texts) -> TextSearch (BS.concat bs) (Map.fromList texts)
  where
    go :: [(String, String)] -> Int -> ([ByteString], [(Int, (String, String))])
    go [] _ = ([], [])
    go (pair@(_, text):xs) pos = 
        let text' = BS.pack $ "\0" ++ stripText text
        in case go xs (BS.length text' + pos) of
            ~(bs, texts) -> (text':bs, (pos, pair):texts)

stripText :: String -> String
stripText = map toLower . filter (\c -> isSpace c || isAlphaNum c)

searchText :: TextSearch -> String -> [(String, String)]
searchText (TextSearch theText theIndex) str =
    Map.toList . Map.fromAscListWith const
  . catMaybes . map (\i -> getIndexEntry (fromIntegral i) theIndex)
  $ nonOverlappingIndices (BS.pack $ stripText str) theText

-- TODO: offset might be useful for determining whether the match was whole-word
-- or no
getIndexEntry :: Int -> Map Int a -> Maybe a
getIndexEntry index theIndex = case Map.splitLookup index theIndex of
    (_, Just entry, _) -> Just entry
    (beforeMap, _, afterMap) -> case (Map.null beforeMap, Map.null afterMap) of
        (True, True)  -> Nothing
        (True, False) -> Just $ snd $ Map.findMin afterMap
        (False, _) -> Just $ snd $ Map.findMax beforeMap

