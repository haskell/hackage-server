{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distribution.Server.Util.Histogram where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (delete, sortBy)
import Data.Ord (comparing)
import Control.DeepSeq

import Distribution.Server.Framework.MemSize


-- | Histograms are intended to keep track of an integer attribute related
-- to a collection of objects.
data Histogram a = Histogram {
    histogram        :: !(Map a Int),
    reverseHistogram :: !(Map Count [a])
}
emptyHistogram :: Histogram a
emptyHistogram = Histogram Map.empty Map.empty

newtype Count = Count Int deriving (Eq, Show, NFData, MemSize)
instance Ord Count where
    compare (Count a) (Count b) = compare b a

instance NFData a => NFData (Histogram a) where
    rnf (Histogram a b) = rnf a `seq` rnf b

instance MemSize a => MemSize (Histogram a) where
    memSize (Histogram a b) = memSize2 a b

topCounts :: Ord a => Histogram a -> [(a, Int)]
topCounts = concatMap (\(Count c, es) -> map (flip (,) c) es) . Map.toList . reverseHistogram

topEntries :: Ord a => Histogram a -> [a]
topEntries = concat . Map.elems . reverseHistogram

getCount :: Ord a => Histogram a -> a -> Int
getCount (Histogram hist _) entry = Map.findWithDefault 0 entry hist

updateHistogram :: Ord a => a -> Int -> Histogram a -> Histogram a
updateHistogram entry new (Histogram hist rev) =
    let old = Map.findWithDefault 0 entry hist
    in Histogram
        (Map.insert entry new hist)
        (Map.alter putInEntry (Count new) . Map.alter takeOutEntry (Count old) $ rev)
  where
    takeOutEntry Nothing = Nothing
    takeOutEntry (Just l) = case delete entry l of
        [] -> Nothing
        l' -> Just l'
    putInEntry Nothing  = Just [entry]
    putInEntry (Just l) = Just (entry:l)

constructHistogram :: Ord a => [(a, Int)] -> Histogram a
constructHistogram assoc = Histogram
    (Map.fromList assoc)
    (Map.fromListWith (++) . map toSingle $ assoc)
  where toSingle (entry, c) = (Count c, [entry])

sortByCounts :: Ord a => (b -> a) -> Histogram a -> [b] -> [(b, Int)]
sortByCounts entryFunc (Histogram hist _) items =
    let modEntry item = (item, Map.findWithDefault 0 (entryFunc item) hist)
    in sortBy (comparing snd) $ map modEntry items

