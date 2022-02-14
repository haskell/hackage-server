{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, ScopedTypeVariables #-}

module Data.IntTrie (

  IntTrie(..),
  construct,

  lookup,
  TrieLookup(..),

#ifdef TESTS
  tests,
  prop_construct1,
  prop_construct2,
#endif

 ) where

import Prelude hiding (lookup)

import Data.Typeable (Typeable)

import qualified Data.Array.Unboxed as A
import Data.Array.IArray  ((!))
import qualified Data.Bits as Bits
import Data.Word (Word32)

import Data.List hiding (lookup)
import Data.Function (on)
import Data.SafeCopy (base, deriveSafeCopy)

import Distribution.Server.Framework.Instances()
import Distribution.Server.Framework.MemSize

#ifdef TESTS
import Test.QuickCheck
import Control.Applicative ((<$>), (<*>))
#endif


-- | A compact mapping from sequences of small ints to small ints.
--
newtype IntTrie k v = IntTrie (A.UArray Word32 Word32)
    deriving (Show, Typeable)

-- Version 0 used 16-bit integers and is no longer supported
-- (To upgrade, DELETE /server-status/tarindices to wipe the tar indices state)
$(deriveSafeCopy 1 'base ''IntTrie)

instance MemSize (IntTrie k v) where
    memSize (IntTrie o) = memSizeUArray 4 o

-- Compact, read-only implementation of a trie. It's intended for use with file
-- paths, but we do that via string ids.

#ifdef TESTS
-- Example mapping:
--
example0 :: [(FilePath, Int)]
example0 =
  [("foo-1.0/foo-1.0.cabal", 512)   -- tar block 1
  ,("foo-1.0/LICENSE",       2048)  -- tar block 4
  ,("foo-1.0/Data/Foo.hs",   4096)] -- tar block 8

-- After converting path components to integers this becomes:
--
example1 :: Paths Word32 Word32
example1 =
  [([1,2],   512)
  ,([1,3],   2048)
  ,([1,4,5], 4096)]

-- As a trie this looks like:

--  [ (1, *) ]
--        |
--        [ (2, 512), (3, 1024), (4, *) ]
--                                   |
--                                   [ (5, 4096) ]

-- We use an intermediate trie representation

example2 :: Trie Word32 Word32
example2 = Trie [ Node 1 t1 ]
  where
    t1   = Trie [ Leaf 2 512, Leaf 3 2048, Node 4 t2 ]
    t2   = Trie [ Leaf 5 4096 ]


example2' :: Trie Word32 Word32
example2' = Trie [ Node 0 t1 ]
  where
    t1   = Trie [ Node 3 t2 ]
    t2   = Trie [ Node 1 t3, Node 2 t4 ]
    t3   = Trie [ Leaf 4 10608 ]
    t4   = Trie [ Leaf 4 10612 ]
{-
0: [1,N0,3]

  3: [1,N3,6]

   6: [2,N1,N2,11,12]

     11: [1,4,10608]
     14: [1,4,10612]
-}

example2'' :: Trie Word32 Word32
example2'' = Trie [ Node 1 t1, Node 2 t2 ]
  where
    t1   = Trie [ Leaf 4 10608 ]
    t2   = Trie [ Leaf 4 10612 ]

example2''' :: Trie Word32 Word32
example2''' = Trie [ Node 0 t3 ]
  where
    t3  = Trie [ Node 4 t8, Node 6 t11 ]
    t8  = Trie [ Node 1 t14 ]
    t11 = Trie [ Leaf 5 10605 ]
    t14 = Trie [ Node 2 t19, Node 3 t22 ]
    t19 = Trie [ Leaf 7 10608 ]
    t22 = Trie [ Leaf 7 10612 ]
{-
 0: [1,N0,3]
 3: [2,N4,N6,8,11]
 8: [1,N1,11]
11: [1,5,10605]
14: [2,N2,N3,16,19]
19: [1,7,10608]
22: [1,7,10612]
-}

-- We convert from the 'Paths' to the 'Trie' using 'mkTrie':
--
test1 = example2 == mkTrie example1
#endif

-- Each node has a size and a sequence of keys followed by an equal length
-- sequnce of corresponding entries. Since we're going to flatten this into
-- a single array then we will need to replace the trie structure with pointers
-- represented as array offsets.

-- Each node is a pair of arrays, one of keys and one of Either value pointer.
-- We need to distinguish values from internal pointers. We use a tag bit:
--
tagLeaf, tagNode, untag :: Word32 -> Word32
tagLeaf = id
tagNode = flip Bits.setBit   31
untag   = flip Bits.clearBit 31

isNode :: Word32 -> Bool
isNode = flip Bits.testBit 31

-- So the overall array form of the above trie is:
--
-- offset:   0   1    2    3   4  5  6    7    8     9     10  11  12
-- array:  [ 1 | N1 | 3 ][ 3 | 2, 3, N4 | 512, 2048, 10 ][ 1 | 5 | 4096 ]
--                     \__/                           \___/

#ifdef TESTS
example3 :: [Word32]
example3 =
 [1, tagNode 1,
     3,
  3, tagLeaf 2, tagLeaf 3, tagNode 4,
     512,       2048,      10,
  1, tagLeaf 5,
     4096
 ]

-- We get the array form by using flattenTrie:

test2 = example3 == flattenTrie example2

example4 :: IntTrie Int Int
example4 = IntTrie (mkArray example3)

test3 = case lookup example4 [1] of
          Just (Completions [(2,_),(3,_),(4,_)]) -> True
          _                          -> False

test1, test2, test3, tests :: Bool
tests = test1 && test2 && test3
#endif

-------------------------------------
-- Decoding the trie array form
--

completionsFrom :: (Enum k, Enum v) => IntTrie k v -> Word32 -> Completions k v
completionsFrom trie@(IntTrie arr) nodeOff =
    [ (word32ToEnum (untag key), next)
    | keyOff <- [keysStart..keysEnd]
    , let key   = arr ! keyOff
          entry = arr ! (keyOff + nodeSize)
          next | isNode key = Completions (completionsFrom trie entry)
               | otherwise  = Entry (word32ToEnum entry)
    ]
  where
    nodeSize  = arr ! nodeOff
    keysStart = nodeOff + 1
    keysEnd   = nodeOff + nodeSize

-------------------------------------
-- Toplevel trie array construction
--

-- So constructing the 'IntTrie' as a whole is just a matter of stringing
-- together all the bits

-- | Build an 'IntTrie' from a bunch of (key, value) pairs, where the keys
-- are sequences.
--
construct :: (Ord k, Enum k, Enum v) => [([k], v)] -> IntTrie k v
construct = IntTrie . mkArray . flattenTrie . mkTrie

mkArray :: [Word32] -> A.UArray Word32 Word32
mkArray xs = A.listArray (0, fromIntegral (length xs) - 1) xs


---------------------------------
-- Looking up in the trie array
--

data TrieLookup  k v = Entry !v | Completions (Completions k v) deriving Show
type Completions k v = [(k, TrieLookup k v)]

lookup :: forall k v. (Enum k, Enum v) => IntTrie k v -> [k] -> Maybe (TrieLookup k v)
lookup trie@(IntTrie arr) = go 0
  where
    go :: Word32 -> [k] -> Maybe (TrieLookup k v)
    go nodeOff []     = Just (completions nodeOff)
    go nodeOff (k:ks) = case search nodeOff (tagLeaf k') of
      Just entryOff
        | null ks   -> Just (entry entryOff)
        | otherwise -> Nothing
      Nothing       -> case search nodeOff (tagNode k') of
        Nothing       -> Nothing
        Just entryOff -> go (arr ! entryOff) ks
      where
        k' = enumToWord32 k

    entry       entryOff = Entry (word32ToEnum (arr ! entryOff))
    completions nodeOff  = Completions (completionsFrom trie nodeOff)

    search :: Word32 -> Word32 -> Maybe Word32
    search nodeOff key = fmap (+nodeSize) (bsearch keysStart keysEnd key)
      where
        nodeSize  = arr ! nodeOff
        keysStart = nodeOff + 1
        keysEnd   = nodeOff + nodeSize

    bsearch :: Word32 -> Word32 -> Word32 -> Maybe Word32
    bsearch a b key
      | a > b     = Nothing
      | otherwise = case compare key (arr ! mid) of
          LT -> bsearch a (mid-1) key
          EQ -> Just mid
          GT -> bsearch (mid+1) b key
      where mid = (a + b) `div` 2


enumToWord32 :: Enum n => n -> Word32
enumToWord32 = fromIntegral . fromEnum

word32ToEnum :: Enum n => Word32 -> n
word32ToEnum = toEnum . fromIntegral


-------------------------
-- Intermediate Trie type
--

-- The trie node functor
data TrieNodeF k v x = Leaf k v | Node k x deriving (Eq, Show)

instance Functor (TrieNodeF k v) where
  fmap _ (Leaf k v) = Leaf k v
  fmap f (Node k x) = Node k (f x)

-- The trie functor
type TrieF k v x = [TrieNodeF k v x]

-- Trie is the fixpoint of the 'TrieF' functor
newtype Trie  k v   = Trie (TrieF k v (Trie k v)) deriving (Eq, Show)


unfoldTrieNode :: (s -> TrieNodeF k v [s]) -> s -> TrieNodeF k v (Trie k v)
unfoldTrieNode f = fmap (unfoldTrie f) . f

unfoldTrie :: (s -> TrieNodeF k v [s]) -> [s] -> Trie k v
unfoldTrie f = Trie . map (unfoldTrieNode f)

{-
trieSize :: Trie k v -> Int
trieSize (Trie ts) = 1 + sum (map trieNodeSize ts)

trieNodeSize :: TrieNodeF k v (Trie k v) -> Int
trieNodeSize (Leaf _ _) = 2
trieNodeSize (Node _ t) = 2 + trieSize t
-}

---------------------------------
-- Building and flattening Tries
--

-- | A list of key value pairs. The keys must be distinct and non-empty.
type Paths k v = [([k], v)]


mkTrie :: Ord k => Paths k v -> Trie k v
mkTrie = unfoldTrie (fmap split) . split
       . sortBy (compare `on` fst)
  where
    split :: Eq k => Paths k v -> TrieF k v (Paths k v)
    split = map mkGroup . groupBy ((==) `on` (head . fst))
      where
        mkGroup = \ksvs@((k0:_,v0):_) ->
          case [ (ks, v) | (_:ks, v) <- ksvs, not (null ks) ] of
            []    -> Leaf k0 v0
            ksvs' -> Node k0 ksvs'

type Offset = Int

-- This is a breadth-first traversal. We keep a list of the tries that we are
-- to write out next. Each of these have an offset allocated to them at the
-- time we put them into the list. We keep a running offset so we know where
-- to allocate next.
--
flattenTrie :: (Enum k, Enum v) => Trie k v -> [Word32]
flattenTrie trie = go [trie] (size trie)
  where
    size (Trie tns) = 1 + 2 * length tns

    go :: (Enum k, Enum v) => [Trie k v] -> Offset -> [Word32]
    go []                  _      = []
    go (Trie tnodes:tries) offset = flat ++ go (tries++tries') offset'
      where
        count = length tnodes
        flat  = fromIntegral count : keys ++ values
        (keys, values) = unzip (sortBy (compare `on` fst) keysValues)
        (keysValues, tries', offset') = doNodes offset [] [] tnodes

    doNodes off kvs ts' []       = (kvs, reverse ts', off)
    doNodes off kvs ts' (tn:tns) = case tn of
      Leaf k v -> doNodes off            (leafKV k v  :kvs)    ts'  tns
      Node k t -> doNodes (off + size t) (nodeKV k off:kvs) (t:ts') tns

    leafKV k v = (tagLeaf (enum2Word32 k), enum2Word32 v)
    nodeKV k o = (tagNode (enum2Word32 k), int2Word32  o)

int2Word32 :: Int -> Word32
int2Word32 = fromIntegral

enum2Word32 :: Enum n => n -> Word32
enum2Word32 = int2Word32 . fromEnum


-------------------------
-- Correctness property
--

#ifdef TESTS

prop_construct1 :: ValidPaths -> Bool
prop_construct1 (ValidPaths paths) =
  flip all paths $ \(key, value) ->
    case lookup trie key of
      Just (Entry value') | value' == value -> True
      Just (Entry value') -> error $ "IntTrie: " ++ show (key, value, value')
      Nothing             -> error $ "IntTrie: didn't find " ++ show key
      Just (Completions xs) -> error $ "IntTrie: " ++ show xs

  where
    trie = construct paths

prop_construct2 :: ValidPaths -> Bool
prop_construct2 (ValidPaths paths) =
    mkTrie paths == convertCompletions (completionsFrom (construct paths) 0)
  where
    convertCompletions :: Ord k => Completions k v -> Trie k v
    convertCompletions kls =
      Trie [ case l of
               Entry v          -> Leaf k v
               Completions kls' -> Node k (convertCompletions kls')
           | (k, l) <- sortBy (compare `on` fst) kls ]

newtype ValidPaths = ValidPaths (Paths Char Char) deriving Show

instance Arbitrary ValidPaths where
  arbitrary =
      ValidPaths . makeNoPrefix <$> listOf ((,) <$> listOf1 arbitrary <*> arbitrary)
    where
      makeNoPrefix [] = []
      makeNoPrefix ((k,v):kvs)
        | all (\(k', _) -> not (isPrefixOfOther k k')) kvs
                     = (k,v) : makeNoPrefix kvs
        | otherwise  =         makeNoPrefix kvs

  shrink (ValidPaths kvs) =
      map ValidPaths . filter noPrefix . filter nonEmpty . shrink $ kvs
    where
      noPrefix []          = True
      noPrefix ((k,_):kvs) = all (\(k', _) -> not (isPrefixOfOther k k')) kvs
                          && noPrefix kvs
      nonEmpty = all (not . null . fst)

isPrefixOfOther a b = a `isPrefixOf` b || b `isPrefixOf` a

#endif
