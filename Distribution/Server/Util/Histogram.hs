{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distribution.Server.Util.Histogram where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (delete)
import Control.Parallel.Strategies

-- technically a reverse histogram
type Histogram a = Map Count [a]

newtype Count = Count Int deriving (Eq, Show, NFData)
instance Ord Count where
    compare (Count a) (Count b) = compare b a

topCounts :: Eq a => Histogram a -> [(a, Int)]
topCounts = concatMap (\(Count c, es) -> map (flip (,) c) es) . Map.toList

topEntries :: Eq a => Histogram a -> [a]
topEntries = concat . Map.elems

updateHistogram :: Eq a => a -> Int -> Int -> Histogram a -> Histogram a
updateHistogram entry old new = Map.alter putInEntry (Count new)
                              . Map.alter takeOutEntry (Count old)
  where
    takeOutEntry Nothing = Nothing
    takeOutEntry (Just l) = case delete entry l of
        [] -> Nothing
        l' -> Just l'
    putInEntry Nothing  = Just [entry]
    putInEntry (Just l) = Just (entry:l)

constructHistogram :: Eq a => [(a, Int)] -> Histogram a
constructHistogram = Map.fromListWith (++) . map (\(entry, c) -> (Count c, [entry]))

