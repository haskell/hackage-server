-- | See:
--
-- * \"The Probabilistic Relevance Framework: BM25 and Beyond\"
--   <www.soi.city.ac.uk/~ser/papers/foundations_bm25_review.pdf‎>
--
-- * \"An Introduction to Information Retrieval\"
--   <http://nlp.stanford.edu/IR-book/pdf/irbookonlinereading.pdf>
--
module Distribution.Server.Features.Search.BM25F (
    Context(..),
    FeatureFunction(..),
    Doc(..),
    score,
  ) where

import Data.Ix

data Context term field feature = Context {
         numDocsTotal     :: !Int,
         avgFieldLength   :: field -> Float,
         numDocsWithTerm  :: term -> Int,
         paramK1          :: !Float,
         paramB           :: field -> Float,
         -- consider minimum length to prevent massive B bonus?
         fieldWeight      :: field -> Float,
         featureWeight    :: feature -> Float,
         featureFunction  :: feature -> FeatureFunction
       }

data Doc term field feature = Doc {
         docFieldLength        :: field -> Int,
         docFieldTermFrequency :: field -> term -> Int,
         docFeatureValue       :: feature -> Float
       }


-- | The BM25F score for a document for a given set of terms.
--
score :: (Ix field, Bounded field, Ix feature, Bounded feature) =>
         Context term field feature ->
         Doc term field feature -> [term] -> Float
score ctx doc terms =
    sum (map (weightedTermScore    ctx doc) terms)
  + sum (map (weightedNonTermScore ctx doc) features)

  where
    features = range (minBound, maxBound)


weightedTermScore :: (Ix field, Bounded field) =>
                     Context term field feature ->
                     Doc term field feature -> term -> Float
weightedTermScore ctx doc t =
    weightIDF ctx t *     tf'
                     / (k1 + tf')
  where
    tf' = weightedDocTermFrequency ctx doc t
    k1  = paramK1 ctx


weightIDF :: Context term field feature -> term -> Float
weightIDF ctx t =
    log ((n - n_t + 0.5) / (n_t + 0.5))
  where
    n   = fromIntegral (numDocsTotal ctx)
    n_t = fromIntegral (numDocsWithTerm ctx t)


weightedDocTermFrequency :: (Ix field, Bounded field) =>
                            Context term field feature ->
                            Doc term field feature -> term -> Float
weightedDocTermFrequency ctx doc t =
    sum [ w_f * tf_f / _B_f
        | field <- range (minBound, maxBound)
        , let w_f  = fieldWeight ctx field
              tf_f = fromIntegral (docFieldTermFrequency doc field t)
              _B_f = lengthNorm ctx doc field
        ]


lengthNorm :: Context term field feature ->
              Doc term field feature -> field -> Float
lengthNorm ctx doc field =
    (1-b_f) + b_f * sl_f / avgsl_f
  where
    b_f     = paramB ctx field
    sl_f    = fromIntegral (docFieldLength doc field)
    avgsl_f = avgFieldLength ctx field


weightedNonTermScore :: (Ix feature, Bounded feature) =>
                        Context term field feature ->
                        Doc term field feature -> feature -> Float
weightedNonTermScore ctx doc feature =
    w_f * _V_f f_f
  where
    w_f  = featureWeight ctx feature
    _V_f = applyFeatureFunction (featureFunction ctx feature)
    f_f  = docFeatureValue doc feature


data FeatureFunction
   = LogarithmicFunction   Float -- ^ @log (\lambda_i + f_i)@
   | RationalFunction      Float -- ^ @f_i / (\lambda_i + f_i)@
   | SigmoidFunction Float Float -- ^ @1 / (\lambda + exp(-(\lambda' * f_i))@

applyFeatureFunction :: FeatureFunction -> (Float -> Float)
applyFeatureFunction (LogarithmicFunction p1) = \fi -> log (p1 + fi)
applyFeatureFunction (RationalFunction    p1) = \fi -> fi / (p1 + fi)
applyFeatureFunction (SigmoidFunction  p1 p2) = \fi -> 1 / (p1 + exp (-fi * p2))
