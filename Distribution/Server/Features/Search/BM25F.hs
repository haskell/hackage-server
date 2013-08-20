{-# LANGUAGE RecordWildCards #-}
module Distribution.Server.Features.Search.BM25F (
    Context(..),
    Doc(..),
    score,

    Explanation(..),
    explain,
  ) where

import Data.Ix

data Context term field = Context {
         numDocsTotal     :: !Int,
         avgFieldLength   :: field -> Float,
         numDocsWithTerm  :: term -> Int,
         paramK1          :: !Float,
         paramB           :: field -> Float,
         -- consider minimum length to prevent massive B bonus?
         fieldWeight      :: field -> Float
       }

data Doc term field = Doc {
         docFieldLength        :: field -> Int,
         docFieldTermFrequency :: field -> term -> Int
       }

weightIDF :: Context term field -> term -> Float
weightIDF ctx t =
    log ((n - n_t + 0.5) / (n_t + 0.5))
  where
    n   = fromIntegral (numDocsTotal ctx)
    n_t = fromIntegral (numDocsWithTerm ctx t)

lengthNorm :: Context term field ->
              Doc term field -> field -> Float
lengthNorm ctx doc field =
    (1-b_f) + b_f * sl_f / avgsl_f
  where
    b_f     = paramB ctx field
    sl_f    = fromIntegral (docFieldLength doc field)
    avgsl_f = avgFieldLength ctx field

weightedDocTermFrequency :: (Ix field, Bounded field) =>
                            Context term field ->
                            Doc term field -> term -> Float
weightedDocTermFrequency ctx doc t =
    sum [ w_f * tf_f / _B_f
        | field <- range (minBound, maxBound)
        , let w_f  = fieldWeight ctx field
              tf_f = fromIntegral (docFieldTermFrequency doc field t)
              _B_f = lengthNorm ctx doc field
        ]

weightBM25 :: (Ix field, Bounded field) => Context term field ->
              Doc term field -> term -> Float
weightBM25 ctx doc t =
    weightIDF ctx t *     tf'
                     / (k1 + tf')
  where
    tf' = weightedDocTermFrequency ctx doc t
    k1  = paramK1 ctx

score :: (Ix field, Bounded field) => Context term field ->
             Doc term field -> [term] -> Float
score ctx doc ts =
    sum (map (weightBM25 ctx doc) ts)

------------------
-- Explanation
--

-- | A breakdown of the BM25F score, to explain somewhat how it relates to
-- the inputs, and so you can compare the scores of different documents.
--
data Explanation field term = Explanation {
       -- | The overall score is the sum of the 'termScores', 'positionScore'
       -- and 'nonTermScore'
       overallScore  :: Float,

       -- | There is a score contribution from each query term. This is the
       -- score for the term across all fields in the document (but see
       -- 'termFieldScores').
       termScores    :: [(term, Float)],
{-
       -- | There is a score contribution for positional information. Terms
       -- appearing in the document close together give a bonus.
       positionScore :: [(field, Float)],

       -- | The document can have an inate bonus score independent of the terms
       -- in the query. For example this might be a popularity score.
       nonTermScore  :: Float,
-}
       -- | This does /not/ contribute to the 'overallScore'. It is an
       -- indication of how the 'termScores' relates to per-field scores.
       -- Note however that the term score for all fields is /not/ simply
       -- sum of the per-field scores. The point of the BM25F scoring function
       -- is that a linear combination of per-field scores is wrong, and BM25F
       -- does a more cunning non-linear combination.
       --
       -- However, it is still useful as an indication to see scores for each
       -- field for a term, to see how the compare.
       --
       termFieldScores :: [(term, [(field, Float)])]
     }
  deriving Show

instance Functor (Explanation field) where
  fmap f e@Explanation{..} =
    e {
      termScores      = [ (f t, s)  | (t, s)  <- termScores ],
      termFieldScores = [ (f t, fs) | (t, fs) <- termFieldScores ]
    }

explain :: (Ix field, Bounded field) => Context term field ->
            Doc term field -> [term] -> Explanation field term
explain ctx doc ts =
    Explanation {..}
  where
    overallScore  = sum (map snd termScores)
--                  + sum (map snd positionScore)
--                  + nonTermScore
    termScores    = [ (t, weightBM25 ctx doc t) | t <- ts ]
--    positionScore = [ (f, 0) | f <- range (minBound, maxBound) ]
--    nonTermScore  = 0

    termFieldScores =
      [ (t, fieldScores)
      | t <- ts
      , let fieldScores =
              [ (f, weightBM25 ctx' doc t)
              | f <- range (minBound, maxBound)
              , let ctx' = ctx { fieldWeight = fieldWeightOnly f }
              ]
      ]
    fieldWeightOnly f f' | sameField f f' = fieldWeight ctx f'
                         | otherwise      = 0

    sameField f f' = index (minBound, maxBound) f
                  == index (minBound, maxBound) f'
