{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module Distribution.Server.Features.Search.TermBag (
    TermId,
    TermBag,
    size,
    fromList,
    elems,
    termCount,
  ) where

import Distribution.Server.Framework.MemSize

import qualified Data.Vector.Unboxed as Vec
import qualified Data.Map as Map
import Data.Word (Word32)
import Data.Bits

newtype TermId = TermId Word32
  deriving (Eq, Ord, Show, Enum, MemSize)

instance Bounded TermId where
  minBound = TermId 0
  maxBound = TermId 0x00FFFFFF

data TermBag = TermBag !Int !(Vec.Vector TermIdAndCount)
  deriving Show

-- We sneakily stuff both the TermId and the bag count into one 32bit word
type TermIdAndCount = Word32

-- Bottom 24 bits is the TermId, top 8 bits is the bag count
termIdAndCount :: TermId -> Int -> TermIdAndCount
termIdAndCount (TermId termid) freq =
      (min (fromIntegral freq) 255 `shiftL` 24)
  .|. (termid .&. 0x00FFFFFF)

getTermId :: TermIdAndCount -> TermId
getTermId word = TermId (word .&. 0x00FFFFFF)

getTermCount :: TermIdAndCount -> Int
getTermCount word = fromIntegral (word `shiftR` 24)


size :: TermBag -> Int
size (TermBag sz _) = sz

elems :: TermBag -> [TermId]
elems (TermBag _ vec) = map getTermId (Vec.toList vec)

termCount :: TermBag -> TermId -> Int
termCount (TermBag _ vec) =
    binarySearch 0 (Vec.length vec - 1)
  where
    binarySearch :: Int -> Int -> TermId -> Int
    binarySearch !a !b !key
      | a > b     = 0
      | otherwise =
        let mid         = (a + b) `div` 2
            tidAndCount = vec Vec.! mid
         in case compare key (getTermId tidAndCount) of
              LT -> binarySearch a (mid-1) key
              EQ -> getTermCount tidAndCount
              GT -> binarySearch (mid+1) b key

fromList :: [TermId] -> TermBag
fromList termids =
    let bag = Map.fromListWith (+) [ (t, 1) | t <- termids ]
        sz  = Map.foldl' (+) 0 bag
        vec = Vec.fromListN (Map.size bag)
                            [ termIdAndCount termid freq
                            | (termid, freq) <- Map.toAscList bag ]
     in TermBag sz vec

instance MemSize TermBag where
  memSize (TermBag _ vec) = 2 + memSizeUVector 2 vec

