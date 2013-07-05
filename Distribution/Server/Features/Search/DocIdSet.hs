{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module Distribution.Server.Features.Search.DocIdSet (
    DocId,  
    DocIdSet,
    null,
    size,
    empty,
    singleton,
    fromList,
    toList,
    toSet,
    insert,
    delete,
    union,
    invariant,
  ) where

import Distribution.Server.Framework.MemSize

import Data.Word
import qualified Data.Vector.Unboxed         as Vec
import qualified Data.Vector.Unboxed.Mutable as MVec
import qualified Data.Vector.Generic.Base    as VecGen
import qualified Data.Vector.Unboxed.Base    as VecBase
import qualified Data.Vector.Generic.Mutable as VecMut
import Control.Monad.ST
import Data.Set (Set)
import qualified Data.Set as Set

import Prelude hiding (null)

--import Test.QuickCheck
--import qualified Data.List as List

newtype DocId = DocId Word32
  deriving (Eq, Ord, Show, Enum, Bounded, MemSize, Vec.Unbox,
            VecGen.Vector VecBase.Vector,
            VecMut.MVector VecBase.MVector)

newtype DocIdSet = DocIdSet (Vec.Vector DocId)
  deriving (Eq, Show)

-- represented as a sorted sequence of ids
invariant :: DocIdSet -> Bool
invariant (DocIdSet vec) =
    strictlyAscending (Vec.toList vec)
  where
    strictlyAscending (a:xs@(b:_)) = a < b && strictlyAscending xs
    strictlyAscending _  = True


size :: DocIdSet -> Int
size (DocIdSet vec) = Vec.length vec

null :: DocIdSet -> Bool
null (DocIdSet vec) = Vec.null vec

empty :: DocIdSet
empty = DocIdSet Vec.empty

singleton :: DocId -> DocIdSet
singleton = DocIdSet . Vec.singleton

fromList :: [DocId] -> DocIdSet
fromList = DocIdSet . Vec.fromList . Set.toAscList . Set.fromList

toList ::  DocIdSet -> [DocId]
toList (DocIdSet vec) = Vec.toList vec

toSet ::  DocIdSet -> Set DocId
toSet (DocIdSet vec) = Set.fromDistinctAscList (Vec.toList vec)

insert :: DocId -> DocIdSet -> DocIdSet
insert x (DocIdSet vec) =
    case binarySearch vec 0 (Vec.length vec - 1) x of
      (_, True)  -> DocIdSet vec
      (i, False) -> case Vec.splitAt i vec of
                      (before, after) ->
                        DocIdSet (Vec.concat [before, Vec.singleton x, after])

delete :: DocId -> DocIdSet -> DocIdSet
delete x (DocIdSet vec) =
    case binarySearch vec 0 (Vec.length vec - 1) x of
      (_, False) -> DocIdSet vec
      (i, True)  -> case Vec.splitAt i vec of
                      (before, after) ->
                        DocIdSet (before Vec.++ Vec.tail after)

binarySearch :: Vec.Vector DocId -> Int -> Int -> DocId -> (Int, Bool)
binarySearch vec !a !b !key
  | a > b     = (a, False)
  | otherwise =
    let mid = (a + b) `div` 2
     in case compare key (vec Vec.! mid) of
          LT -> binarySearch vec a (mid-1) key
          EQ -> (mid, True)
          GT -> binarySearch vec (mid+1) b key

union :: DocIdSet -> DocIdSet -> DocIdSet
union (DocIdSet xs) (DocIdSet ys) =
    DocIdSet (Vec.create (MVec.new sizeBound >>= writeMerged xs ys))
  where
    sizeBound = Vec.length xs + Vec.length ys

writeMerged :: Vec.Vector DocId -> Vec.Vector DocId ->
              MVec.MVector s DocId -> ST s (MVec.MVector s DocId)
writeMerged xs0 ys0 out = do
    i <- go xs0 ys0 0
    return $! MVec.take i out
  where
    go !xs !ys !i
      | Vec.null xs = do Vec.copy (MVec.slice i (Vec.length ys) out) ys
                         return (i + Vec.length ys)
      | Vec.null ys = do Vec.copy (MVec.slice i (Vec.length xs) out) xs
                         return (i + Vec.length xs)
      | otherwise   = let x = Vec.head xs; y = Vec.head ys
                      in case compare x y of
                GT -> do MVec.write out i y
                         go           xs  (Vec.tail ys) (i+1) 
                EQ -> do MVec.write out i x
                         go (Vec.tail xs) (Vec.tail ys) (i+1)
                LT -> do MVec.write out i x   
                         go (Vec.tail xs)           ys  (i+1)

instance MemSize DocIdSet where
  memSize (DocIdSet vec) = memSizeUVector 2 vec


-------------
-- tests
--
{-
instance Arbitrary DocIdSet where
  arbitrary = fromList `fmap` (listOf arbitrary)

instance Arbitrary DocId where
  arbitrary = DocId `fmap` choose (0,15)


prop_insert :: DocIdSet -> DocId -> Bool
prop_insert dset x =
    let dset' = insert x dset
     in invariant dset && invariant dset'
     && all (`member` dset') (x : toList dset)

prop_delete :: DocIdSet -> DocId -> Bool
prop_delete dset x =
    let dset' = DocIdSet.delete x dset
     in invariant dset && invariant dset'
     && all (`member` dset') (List.delete x (toList dset))
     && not (x `member` dset')

prop_delete' :: DocIdSet -> Bool
prop_delete' dset =
    all (prop_delete dset) (toList dset)

prop_union :: DocIdSet -> DocIdSet -> Bool
prop_union dset1 dset2 =
    let dset  = union dset1 dset2
        dset' = fromList (List.union (toList dset1) (toList dset2))

     in invariant dset && invariant dset'
     && dset == dset'

prop_union' :: DocIdSet -> DocIdSet -> Bool
prop_union' dset1 dset2 =
    let dset   = union dset1 dset2
        dset'  = List.foldl' (\s i -> insert i s) dset1 (toList dset2)
        dset'' = List.foldl' (\s i -> insert i s) dset2 (toList dset1)
     in invariant dset && invariant dset' && invariant dset''
     && dset == dset'
     && dset' == dset''

member :: DocId -> DocIdSet -> Bool
member x (DocIdSet vec) =
   x `List.elem` Vec.toList vec
-}
