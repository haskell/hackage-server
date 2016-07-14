{-# LANGUAGE FlexibleInstances #-}
module Distribution.Server.Framework.MemSize (
  MemSize(..),
  memSizeMb, memSizeKb,
  memSize0, memSize1, memSize2, memSize3, memSize4, memSize5,
  memSize6, memSize7, memSize8, memSize9, memSize10, memSize11, memSize13,
  memSizeUArray, memSizeUVector
  ) where

import Data.Word
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Bimap as Bimap
import Data.Bimap (Bimap)
import qualified Data.Graph as Gr
import Data.Graph (Graph)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.Sequence (Seq)
import qualified Data.Foldable as Foldable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as BSS
import qualified Data.Text as T
import Data.Time (UTCTime, Day)
import Data.Ix
import qualified Data.Array.Unboxed as A
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as V.U

import Distribution.Package  (PackageIdentifier(..), PackageName(..))
import Distribution.PackageDescription (FlagName(..))
import Distribution.Version  (Version(..), VersionRange, foldVersionRange')
import Distribution.System   (Arch(..), OS(..))
import Distribution.Compiler (CompilerFlavor(..), CompilerId(..))

-------------------------------------------------------------------------------
-- Mem size class and instances
--

memSizeMb, memSizeKb :: Int -> Int
memSizeMb w = wordSize * w `div` (1024 * 1024)
memSizeKb w = wordSize * w `div` 1024

wordSize :: Int
wordSize = 8

-- | Size in the heap of values, in words (so multiply by 4 or 8)
class MemSize a where
  memSize :: a -> Int

memSize0 :: Int
memSize1 :: MemSize a => a -> Int
memSize2 :: (MemSize a1, MemSize a) => a -> a1 -> Int
memSize3 :: (MemSize a2, MemSize a1, MemSize a) => a -> a1 -> a2 -> Int
memSize4 :: (MemSize a3, MemSize a2, MemSize a1, MemSize a) => a -> a1 -> a2 -> a3 -> Int
memSize5 :: (MemSize a4, MemSize a3, MemSize a2, MemSize a1, MemSize a) => a -> a1 -> a2 -> a3 -> a4 -> Int
memSize6 :: (MemSize a5, MemSize a4, MemSize a3, MemSize a2, MemSize a1, MemSize a) => a -> a1 -> a2 -> a3 -> a4 -> a5 -> Int
memSize7 :: (MemSize a6, MemSize a5, MemSize a4, MemSize a3, MemSize a2, MemSize a1, MemSize a) => a -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> Int
memSize8 :: (MemSize a7, MemSize a6, MemSize a5, MemSize a4, MemSize a3, MemSize a2, MemSize a1, MemSize a) => a -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> Int
memSize9 :: (MemSize a8, MemSize a7, MemSize a6, MemSize a5, MemSize a4, MemSize a3, MemSize a2, MemSize a1, MemSize a) => a -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> Int
memSize10 :: (MemSize a9, MemSize a8, MemSize a7, MemSize a6, MemSize a5, MemSize a4, MemSize a3, MemSize a2, MemSize a1, MemSize a) => a -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> Int
memSize11 :: (MemSize a10, MemSize a9, MemSize a8, MemSize a7, MemSize a6, MemSize a5, MemSize a4, MemSize a3, MemSize a2, MemSize a1, MemSize a) => a -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> Int
memSize13 :: (MemSize a12, MemSize a11, MemSize a10, MemSize a9, MemSize a8, MemSize a7, MemSize a6, MemSize a5, MemSize a4, MemSize a3, MemSize a2, MemSize a1, MemSize a) => a -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12 ->Int
memSize0             = 0
memSize1 a           = 2 + memSize a
memSize2 a b         = 3 + memSize a + memSize b
memSize3 a b c       = 4 + memSize a + memSize b + memSize c
memSize4 a b c d     = 5 + memSize a + memSize b + memSize c
                         + memSize d
memSize5 a b c d e   = 6 + memSize a + memSize b + memSize c
                         + memSize d + memSize e
memSize6 a b c d e f = 7 + memSize a + memSize b + memSize c
                         + memSize d + memSize e + memSize f
memSize7 a b c d e
         f g         = 8 + memSize a + memSize b + memSize c
                         + memSize d + memSize e + memSize f
                         + memSize g
memSize8 a b c d e
         f g h       = 9 + memSize a + memSize b + memSize c
                         + memSize d + memSize e + memSize f
                         + memSize g + memSize h
memSize9 a b c d e
         f g h i     = 10 + memSize a + memSize b + memSize c
                          + memSize d + memSize e + memSize f
                          + memSize g + memSize h + memSize i
memSize10 a b c d e
          f g h i j  = 11 + memSize a + memSize b + memSize c
                          + memSize d + memSize e + memSize f
                          + memSize g + memSize h + memSize i
                          + memSize j

memSize11 a b c d e
          f g h i j k= 12 + memSize a + memSize b + memSize c
                          + memSize d + memSize e + memSize f
                          + memSize g + memSize h + memSize i
                          + memSize j + memSize k

memSize13 a b c d e f g
          h i j k l m= 14 + memSize a + memSize b + memSize c
                          + memSize d + memSize e + memSize f
                          + memSize g + memSize h + memSize i
                          + memSize j + memSize k + memSize l
                          + memSize m


instance MemSize (a -> b) where
  memSize _ = 0

instance MemSize Int where
  memSize _ = 2

instance MemSize Word where
  memSize _ = 2

instance MemSize Word32 where
  memSize _ = 2

instance MemSize Char where
  memSize _ = 0

instance MemSize Bool where
  memSize _ = 0

instance MemSize Integer where
  memSize _ = 2

instance MemSize Float where
  memSize _ = 2

instance MemSize UTCTime where
  memSize _ = 7

instance MemSize Day where
  memSize _ = 2

instance MemSize a => MemSize [a] where
  memSize []     = memSize0
  memSize (x:xs) = memSize2 x xs
--   memSize xs = 2 + length xs + sum (map memSize xs)

instance (MemSize a, MemSize b) => MemSize (a,b) where
  memSize (a,b) = memSize2 a b

instance (MemSize a, MemSize b, MemSize c) => MemSize (a,b,c) where
  memSize (a,b,c) = memSize3 a b c

instance (MemSize a, MemSize b, MemSize c, MemSize d) => MemSize (a,b,c,d) where
  memSize (a,b,c,d) = memSize4 a b c d

instance MemSize a => MemSize (Maybe a) where
  memSize Nothing  = memSize0
  memSize (Just a) = memSize1 a

instance (MemSize a, MemSize b) => MemSize (Either a b) where
  memSize (Left  a) = memSize1 a
  memSize (Right b) = memSize1 b

instance (MemSize a, MemSize b) => MemSize (Map a b) where
  memSize m = sum [ 6 + memSize k + memSize v | (k,v) <- Map.toList m ]

instance (MemSize a, MemSize b) => MemSize (Bimap a b) where
  memSize m = sum [ 6 + memSize k + memSize v | (k,v) <- Bimap.toList m ]

instance MemSize a => MemSize (IntMap a) where
  memSize m = sum [ 8 + memSize v | v <- IntMap.elems m ]

instance MemSize Graph where
  memSize m = sum [ 6 + memSize v | v <- Gr.edges m ]

instance MemSize a => MemSize (Set a) where
  memSize m = sum [ 5 + memSize v | v <- Set.elems m ]

instance MemSize IntSet where
  memSize s = 4 * IntSet.size s --estimate

instance MemSize a => MemSize (Seq a) where
  memSize s = sum [ 5 + memSize v | v <- Foldable.toList s ] --estimate

instance MemSize BS.ByteString where
  memSize s = let (w,t) = divMod (BS.length s) wordSize
               in 5 + w + signum t

instance MemSize BSS.ShortByteString where
  memSize s = let (w,t) = divMod (BSS.length s) wordSize
               in 1 + w + signum t

instance MemSize LBS.ByteString where
  memSize s = sum [ 1 + memSize c | c <- LBS.toChunks s ]

instance MemSize T.Text where
  memSize s = let (w,t) = divMod (T.length s) (wordSize `div` 2)
               in 5 + w + signum t

memSizeUArray :: (Ix i, A.IArray a e) => Int -> a i e -> Int
memSizeUArray sz a = 13 + (rangeSize (A.bounds a) * sz) `div` wordSize

instance MemSize e => MemSize (V.Vector e) where
  memSize a = 5 + V.length a + V.foldl' (\s e -> s + memSize e) 0 a

memSizeUVector :: V.U.Unbox e => Int -> V.U.Vector e -> Int
memSizeUVector sz a = 5 + (V.U.length a * sz) `div` wordSize


----

instance MemSize PackageName where
    memSize (PackageName n) = memSize n

instance MemSize Version where
    memSize (Version a b) = memSize2 a b

instance MemSize VersionRange where
    memSize =
      foldVersionRange' memSize0                  -- any
                        memSize1                  -- == v
                        memSize1                  -- > v
                        memSize1                  -- < v
                        (\v -> 7 + 2 * memSize v) -- >= v
                        (\v -> 7 + 2 * memSize v) -- <= v
                        (\v _v' -> memSize1 v)    -- == v.*
                        memSize2                  -- _ || _
                        memSize2                  -- _ && _
                        memSize1                  -- (_)

instance MemSize PackageIdentifier where
    memSize (PackageIdentifier a b) = memSize2 a b

instance MemSize Arch where
    memSize _ = memSize0

instance MemSize OS where
    memSize _ = memSize0

instance MemSize FlagName where
    memSize (FlagName n) = memSize n

instance MemSize CompilerFlavor where
    memSize _ = memSize0

instance MemSize CompilerId where
    memSize (CompilerId a b) = memSize2 a b
