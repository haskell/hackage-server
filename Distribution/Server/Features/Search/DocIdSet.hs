{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, TypeFamilies, MultiParamTypeClasses #-}

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
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import qualified Data.Vector.Generic.Base    as VG
import qualified Data.Vector.Generic.Mutable as MVG
import Control.Monad (liftM)
import Control.Monad.ST
import Data.Set (Set)
import qualified Data.Set as Set

import Prelude hiding (null)

--import Test.QuickCheck
--import qualified Data.List as List

newtype DocId = DocId Word32
  deriving (Eq, Ord, Show, Enum, Bounded, MemSize, VU.Unbox)

newtype DocIdSet = DocIdSet (VU.Vector DocId)
  deriving (Eq, Show)


newtype instance MVU.MVector s DocId = MVU_DocId (MVU.MVector s Word32)
newtype instance VU.Vector DocId = VU_DocId (VU.Vector Word32)

instance VG.Vector VU.Vector DocId where
-- fixme, can i do a noop/coerce/unsafecoerce instead of these fmaps on newtypes?
-- or does it even matter?
  basicUnsafeFreeze (MVU_DocId mvw)= VU_DocId `liftM` VG.basicUnsafeFreeze mvw
  basicUnsafeThaw (VU_DocId vw) = MVU_DocId `liftM` VG.basicUnsafeThaw vw
  basicLength (VU_DocId vw) = VG.basicLength vw
  basicUnsafeSlice start end (VU_DocId vw) = VU_DocId $ VG.basicUnsafeSlice start end vw
  basicUnsafeIndexM (VU_DocId vw) ix = DocId `liftM` VG.basicUnsafeIndexM vw ix

 -- basicLength, basicUnsafeSlice, basicOverlaps, basicUnsafeNew, basicUnsafeRead, basicUnsafeWrite
instance MVG.MVector MVU.MVector DocId where
  basicLength (MVU_DocId vw) = MVG.basicLength vw
  basicUnsafeSlice start end (MVU_DocId vw) = MVU_DocId $ MVG.basicUnsafeSlice start end vw
  basicOverlaps (MVU_DocId vw1) (MVU_DocId vw2) = MVG.basicOverlaps vw1 vw2
  basicUnsafeNew sz = MVU_DocId `liftM` MVG.basicUnsafeNew sz
  basicUnsafeRead (MVU_DocId vw) ix   = DocId `liftM` MVG.basicUnsafeRead vw ix
  basicUnsafeWrite (MVU_DocId vw) ix (DocId word) = MVG.basicUnsafeWrite vw ix word
  basicInitialize (MVU_DocId vw) = MVG.basicInitialize vw

-- represented as a sorted sequence of ids
invariant :: DocIdSet -> Bool
invariant (DocIdSet vec) =
    strictlyAscending (VU.toList vec)
  where
    strictlyAscending (a:xs@(b:_)) = a < b && strictlyAscending xs
    strictlyAscending _  = True


size :: DocIdSet -> Int
size (DocIdSet vec) = VU.length vec

null :: DocIdSet -> Bool
null (DocIdSet vec) = VU.null vec

empty :: DocIdSet
empty = DocIdSet VU.empty

singleton :: DocId -> DocIdSet
singleton = DocIdSet . VU.singleton

fromList :: [DocId] -> DocIdSet
fromList = DocIdSet . VU.fromList . Set.toAscList . Set.fromList

toList ::  DocIdSet -> [DocId]
toList (DocIdSet vec) = VU.toList vec

toSet ::  DocIdSet -> Set DocId
toSet (DocIdSet vec) = Set.fromDistinctAscList (VU.toList vec)

insert :: DocId -> DocIdSet -> DocIdSet
insert x (DocIdSet vec) =
    case binarySearch vec 0 (VU.length vec - 1) x of
      (_, True)  -> DocIdSet vec
      (i, False) -> case VU.splitAt i vec of
                      (before, after) ->
                        DocIdSet (VU.concat [before, VU.singleton x, after])

delete :: DocId -> DocIdSet -> DocIdSet
delete x (DocIdSet vec) =
    case binarySearch vec 0 (VU.length vec - 1) x of
      (_, False) -> DocIdSet vec
      (i, True)  -> case VU.splitAt i vec of
                      (before, after) ->
                        DocIdSet (before VU.++ VU.tail after)

binarySearch :: VU.Vector DocId -> Int -> Int -> DocId -> (Int, Bool)
binarySearch vec !a !b !key
  | a > b     = (a, False)
  | otherwise =
    let mid = (a + b) `div` 2
     in case compare key (vec VU.! mid) of
          LT -> binarySearch vec a (mid-1) key
          EQ -> (mid, True)
          GT -> binarySearch vec (mid+1) b key

union :: DocIdSet -> DocIdSet -> DocIdSet
union x y | null x = y
          | null y = x
union (DocIdSet xs) (DocIdSet ys) =
    DocIdSet (VU.create (MVU.new sizeBound >>= writeMerged xs ys))
  where
    sizeBound = VU.length xs + VU.length ys

writeMerged :: VU.Vector DocId -> VU.Vector DocId ->
              MVU.MVector s DocId -> ST s (MVU.MVector s DocId)
writeMerged xs0 ys0 out = do
    i <- go xs0 ys0 0
    return $! MVU.take i out
  where
    go !xs !ys !i
      | VU.null xs = do VU.copy (MVU.slice i (VU.length ys) out) ys;
                         return (i + VU.length ys)
      | VU.null ys = do VU.copy (MVU.slice i (VU.length xs) out) xs;
                         return (i + VU.length xs)
      | otherwise   = let x = VU.head xs; y = VU.head ys
                      in case compare x y of
                GT -> do MVU.write out i y
                         go           xs  (VU.tail ys) (i+1)
                EQ -> do MVU.write out i x
                         go (VU.tail xs) (VU.tail ys) (i+1)
                LT -> do MVU.write out i x
                         go (VU.tail xs)           ys  (i+1)

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
   x `List.elem` VU.toList vec
-}
