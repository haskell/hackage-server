{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Distribution.Server.Features.Search.DocTermIds (
    DocTermIds,
    TermId,
    fieldLength,
    fieldTermCount,
    fieldElems,
    create,
    vecIndexIx,
    vecCreateIx,
  ) where

import Distribution.Server.Features.Search.TermBag (TermBag, TermId)
import qualified Distribution.Server.Features.Search.TermBag as TermBag

import Distribution.Server.Framework.MemSize

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vec
import Data.Ix (Ix)
import qualified Data.Ix as Ix


-- | The 'TermId's for the 'Term's that occur in a document. Documents may have
-- multiple fields and the 'DocTerms' type holds them separately for each field.
--
newtype DocTermIds field = DocTermIds (Vector TermBag)
  deriving (Show, MemSize)

getField :: (Ix field, Bounded field) => DocTermIds field -> field -> TermBag
getField (DocTermIds fieldVec) = vecIndexIx fieldVec

create :: (Ix field, Bounded field) =>
          (field -> [TermId]) -> DocTermIds field
create docTermIds =
    DocTermIds (vecCreateIx (TermBag.fromList . docTermIds))

-- | The number of terms in a field within the document.
fieldLength :: (Ix field, Bounded field) => DocTermIds field -> field -> Int
fieldLength docterms field =
    TermBag.size (getField docterms field)

-- | The frequency of a particular term in a field within the document.
fieldTermCount :: (Ix field, Bounded field) => DocTermIds field -> field -> TermId -> Int
fieldTermCount docterms field termid =
    TermBag.termCount (getField docterms field) termid

fieldElems :: (Ix field, Bounded field) => DocTermIds field -> field -> [TermId]
fieldElems docterms field =
    TermBag.elems (getField docterms field)

---------------------------------
-- Vector indexed by Ix Bounded
--

vecIndexIx  :: forall ix a . (Ix ix, Bounded ix) => Vector a -> ix -> a
vecIndexIx vec ix = vec ! Ix.index (minBound :: ix, maxBound :: ix) ix

vecCreateIx :: forall ix a . (Ix ix, Bounded ix) => (ix -> a) -> Vector a
vecCreateIx f = Vec.fromListN (Ix.rangeSize bounds)
                  [ y | ix <- Ix.range bounds, let !y = f ix ]
  where
    bounds :: (ix, ix)
    bounds = (minBound, maxBound)
