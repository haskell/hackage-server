module Distribution.Server.Util.Merge where

import Data.Map


data MergeResult a b = OnlyInLeft a | InBoth a b | OnlyInRight b

mergeBy :: (a -> b -> Ordering) -> [a] -> [b] -> [MergeResult a b]
mergeBy cmp = merge
  where
    merge []     ys     = [ OnlyInRight y | y <- ys]
    merge xs     []     = [ OnlyInLeft  x | x <- xs]
    merge (x:xs) (y:ys) =
      case x `cmp` y of
        GT -> OnlyInRight   y : merge (x:xs) ys
        EQ -> InBoth      x y : merge xs     ys
        LT -> OnlyInLeft  x   : merge xs  (y:ys)

mergeMaps :: Ord k => Map k a -> Map k b -> Map k (MergeResult a b)
mergeMaps m1 m2 = unionWith (\(OnlyInLeft a) (OnlyInRight b) -> InBoth a b) (fmap OnlyInLeft m1) (fmap OnlyInRight m2)
