{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module Distribution.Server.Features.Search.DocFeatVals (
    DocFeatVals,
    featureValue,
    create,
  ) where

import Distribution.Server.Features.Search.DocTermIds (vecIndexIx, vecCreateIx)
import Distribution.Server.Framework.MemSize
import Data.Vector (Vector)
import Data.Ix (Ix)


-- | Storage for the non-term feature values i a document.
--
newtype DocFeatVals feature = DocFeatVals (Vector Float)
  deriving (Show, MemSize)

featureValue :: (Ix feature, Bounded feature) => DocFeatVals feature -> feature -> Float
featureValue (DocFeatVals featVec) = vecIndexIx featVec

create :: (Ix feature, Bounded feature) =>
          (feature -> Float) -> DocFeatVals feature
create docFeatVals =
    DocFeatVals (vecCreateIx docFeatVals)

