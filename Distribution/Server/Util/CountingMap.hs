{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, DeriveDataTypeable, ScopedTypeVariables #-}
module Distribution.Server.Util.CountingMap (
    NestedCountingMap(..)
  , SimpleCountingMap(..)
  , CountingMap(..)
  , cmFromCSV
  ) where

import Prelude hiding (rem)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Text.CSV (CSV, Record)
import Control.Applicative ((<$>), (<*>))

import Data.SafeCopy (SafeCopy(..), safeGet, safePut, contain)

import Distribution.Parsec (Parsec(..))
import Distribution.Pretty (Pretty(..))
import Distribution.Text (display)

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize
import Distribution.Server.Framework.BackupRestore (parseRead, parseText)

{------------------------------------------------------------------------------
  We define some generic machinery to give us functions for arbitrarily
  nested "counting maps".

  We define SimpleCountingMap as a separate type from NestedCountingMap as
  a hint to the type checker (we get into trouble with the functional
  dependencies otherwise).
------------------------------------------------------------------------------}

data NestedCountingMap a b = NCM {
    nestedTotalCount  :: !Int
  , nestedCountingMap :: !(Map a b)
  }
  deriving (Show, Eq, Ord, Typeable)

newtype SimpleCountingMap a = SCM {
    simpleCountingMap :: NestedCountingMap a Int
  }
  deriving (Show, Eq, Ord, Typeable)

class CountingMap k a | a -> k where
  cmEmpty  :: a
  cmTotal  :: a -> Int
  cmInsert :: k -> Int -> a -> a
  cmFind   :: k -> a -> Int
  cmUnion  :: a -> a -> a
  cmToList :: a -> [(k, Int)]

  cmToCSV        :: a -> CSV
  cmInsertRecord :: Monad m => Record -> a -> m (a, Int)

instance (Ord k, Typeable k, Parsec k, Pretty k) => CountingMap k (SimpleCountingMap k) where
  cmEmpty = SCM (NCM 0 Map.empty)

  cmTotal (SCM (NCM total _)) = total

  cmInsert k n (SCM (NCM total m)) =
    SCM (NCM (total + n) (adjustFrom (+ n) k 0 m))

  cmUnion (SCM (NCM total1 m1)) (SCM (NCM total2 m2)) =
    SCM (NCM (total1 + total2) (Map.unionWith (+) m1 m2))

  cmFind k (SCM (NCM _ m)) = Map.findWithDefault 0 k m

  cmToList (SCM (NCM _ m)) = Map.toList m

  cmToCSV (SCM (NCM _ m)) = map aux (Map.toList m)
    where
      aux :: (k, Int) -> Record
      aux (k, n) = [display k, show n]

  cmInsertRecord [k, n] m = do
     key   <- parseText "key"   k
     count <- parseRead "count" n
     return (cmInsert key count m, count)
  cmInsertRecord _ _ =
    fail "cmInsertRecord: Invalid record"

instance (Typeable k, Parsec k, Pretty k, Ord k, Eq l, CountingMap l a) => CountingMap (k, l) (NestedCountingMap k a) where
  cmEmpty = NCM 0 Map.empty

  cmTotal (NCM total _m) = total

  cmInsert (k, l) n (NCM total m) =
    NCM (total + n) (adjustFrom (cmInsert l n) k cmEmpty m)

  cmFind (k, l) (NCM _ m) = cmFind l (Map.findWithDefault cmEmpty k m)

  cmUnion (NCM total1 m1) (NCM total2 m2) =
    NCM (total1 + total2) (Map.unionWith cmUnion m1 m2)

  cmToList (NCM _ m) = concatMap aux (Map.toList m)
    where
      aux :: (k, a) -> [((k, l), Int)]
      aux (k, m') = map (\(l, c) -> ((k, l), c)) (cmToList m')

  cmToCSV (NCM _ m) = concatMap aux (Map.toList m)
    where
      aux :: (k, a) -> CSV
      aux (k, m') = map (display k:) (cmToCSV m')

  cmInsertRecord (k : record) (NCM total m) = do
    key <- parseText "key" k
    let submap = Map.findWithDefault cmEmpty key m
    (submap', added) <- cmInsertRecord record submap
    return (NCM (total + added) (Map.insert key submap' m), added)
  cmInsertRecord [] _ =
    fail "cmInsertRecord: Invalid record"

cmFromCSV :: (Monad m, CountingMap k a) => CSV -> m a
cmFromCSV = go cmEmpty
  where
    go acc []     = return acc
    go acc (r:rs) = do
      (acc', _) <- cmInsertRecord r acc
      go acc' rs

{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

adjustFrom :: Ord k => (a -> a) -> k -> a -> Map k a -> Map k a
adjustFrom func key value = Map.alter (Just . func . fromMaybe value) key

{------------------------------------------------------------------------------
  Type classes instances
------------------------------------------------------------------------------}

instance MemSize a => MemSize (SimpleCountingMap a) where
  memSize (SCM m) = memSize m

instance (MemSize a, MemSize b) => MemSize (NestedCountingMap a b) where
  memSize (NCM a b) = memSize2 a b

instance (Ord a, SafeCopy a, SafeCopy b) => SafeCopy (NestedCountingMap a b) where
  putCopy (NCM total m) = contain $ do safePut total ; safePut m
  getCopy = contain $ NCM <$> safeGet <*> safeGet

instance (Ord a, SafeCopy a) => SafeCopy (SimpleCountingMap a) where
  putCopy (SCM m) = contain $ safePut m
  getCopy = contain $ SCM <$> safeGet
