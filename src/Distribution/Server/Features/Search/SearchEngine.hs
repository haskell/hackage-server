{-# LANGUAGE BangPatterns, NamedFieldPuns, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Server.Features.Search.SearchEngine (
    SearchEngine,
    SearchConfig(..),
    SearchRankParameters(..),
    BM25F.FeatureFunction(..),
    Term,
    initSearchEngine,
    insertDoc,
    insertDocs,
    deleteDoc,
    query,

    NoFeatures,
    noFeatures,

    queryExplain,
    BM25F.Explanation(..),
    setRankParams,

    invariant,
  ) where

import Distribution.Server.Features.Search.SearchIndex (SearchIndex, Term, TermId)
import qualified Distribution.Server.Features.Search.SearchIndex as SI
import Distribution.Server.Features.Search.DocIdSet (DocIdSet, DocId)
import qualified Distribution.Server.Features.Search.DocIdSet as DocIdSet
import Distribution.Server.Features.Search.DocTermIds (DocTermIds)
import qualified Distribution.Server.Features.Search.DocTermIds as DocTermIds
import Distribution.Server.Features.Search.DocFeatVals (DocFeatVals)
import qualified Distribution.Server.Features.Search.DocFeatVals as DocFeatVals
import qualified Distribution.Server.Features.Search.BM25F as BM25F

import Distribution.Server.Framework.MemSize

import Data.Ix
import Data.Array.Unboxed
import Data.List
import Data.Function
import Data.Maybe

-------------------
-- Doc layer
--
-- That is, at the layer of documents, so covering the issues of:
--  - inserting/removing whole documents
--  - documents having multiple fields
--  - documents having multiple terms
--  - transformations (case-fold/normalisation/stemming) on the doc terms
--  - transformations on the search terms
--

data SearchConfig doc key field feature = SearchConfig {
       documentKey          :: doc -> key,
       extractDocumentTerms :: doc -> field -> [Term],
       transformQueryTerm   :: Term -> field -> Term,
       documentFeatureValue :: doc -> feature -> Float,
       makeKey :: Term -> key

     }

data SearchRankParameters field feature = SearchRankParameters {
       paramK1                 :: !Float,
       paramB                  :: field -> Float,
       paramFieldWeights       :: field -> Float,
       paramFeatureWeights     :: feature -> Float,
       paramFeatureFunctions   :: feature -> BM25F.FeatureFunction,
       paramResultsetSoftLimit :: !Int,
       paramResultsetHardLimit :: !Int
     }

data SearchEngine doc key field feature = SearchEngine {
       searchIndex      :: !(SearchIndex      key field feature),
       searchConfig     :: !(SearchConfig doc key field feature),
       searchRankParams :: !(SearchRankParameters field feature),

       -- cached info
       sumFieldLengths :: !(UArray field Int),
       bm25Context     :: BM25F.Context TermId field feature
     }

initSearchEngine :: (Ix field, Bounded field, Ix feature, Bounded feature) =>
                    SearchConfig doc key field feature ->
                    SearchRankParameters field feature ->
                    SearchEngine doc key field feature
initSearchEngine config params =
    cacheBM25Context
      SearchEngine {
        searchIndex      = SI.emptySearchIndex,
        searchConfig     = config,
        searchRankParams = params,
        sumFieldLengths  = listArray (minBound, maxBound) (repeat 0),
        -- FIXME this use of undefined bears explaining
        bm25Context      = undefined
      }

setRankParams :: SearchRankParameters field feature ->
                 SearchEngine doc key field feature ->
                 SearchEngine doc key field feature
setRankParams params@SearchRankParameters{..} se =
    se {
      searchRankParams = params,
      bm25Context      = (bm25Context se) {
        BM25F.paramK1         = paramK1,
        BM25F.paramB          = paramB,
        BM25F.fieldWeight     = paramFieldWeights,
        BM25F.featureWeight   = paramFeatureWeights,
        BM25F.featureFunction = paramFeatureFunctions
      }
    }

invariant :: (Ord key, Ix field, Bounded field) =>
             SearchEngine doc key field feature -> Bool
invariant SearchEngine{searchIndex} =
    SI.invariant searchIndex
-- && check caches

cacheBM25Context :: Ix field =>
                    SearchEngine doc key field feature ->
                    SearchEngine doc key field feature
cacheBM25Context
    se@SearchEngine {
      searchRankParams = SearchRankParameters{..},
      searchIndex,
      sumFieldLengths
    }
  = se { bm25Context = bm25Context' }
  where
    bm25Context' = BM25F.Context {
      BM25F.numDocsTotal    = SI.docCount searchIndex,
      BM25F.avgFieldLength  = \f -> fromIntegral (sumFieldLengths ! f)
                                  / fromIntegral (SI.docCount searchIndex),
      BM25F.numDocsWithTerm = DocIdSet.size . SI.lookupTermId searchIndex,
      BM25F.paramK1         = paramK1,
      BM25F.paramB          = paramB,
      BM25F.fieldWeight     = paramFieldWeights,
      BM25F.featureWeight   = paramFeatureWeights,
      BM25F.featureFunction = paramFeatureFunctions
    }

updateCachedFieldLengths :: (Ix field, Bounded field) =>
                            Maybe (DocTermIds field) -> Maybe (DocTermIds field) ->
                            SearchEngine doc key field feature ->
                            SearchEngine doc key field feature
updateCachedFieldLengths Nothing (Just newDoc) se@SearchEngine{sumFieldLengths} =
    se {
      sumFieldLengths =
        array (bounds sumFieldLengths)
              [ (i, n + DocTermIds.fieldLength newDoc i)
              | (i, n) <- assocs sumFieldLengths ]
    }
updateCachedFieldLengths (Just oldDoc) (Just newDoc) se@SearchEngine{sumFieldLengths} =
    se {
      sumFieldLengths =
        array (bounds sumFieldLengths)
              [ (i, n - DocTermIds.fieldLength oldDoc i
                      + DocTermIds.fieldLength newDoc i)
              | (i, n) <- assocs sumFieldLengths ]
    }
updateCachedFieldLengths (Just oldDoc) Nothing se@SearchEngine{sumFieldLengths} =
    se {
      sumFieldLengths =
        array (bounds sumFieldLengths)
              [ (i, n - DocTermIds.fieldLength oldDoc i)
              | (i, n) <- assocs sumFieldLengths ]
    }
updateCachedFieldLengths Nothing Nothing se = se

insertDocs :: (Ord key, Ix field, Bounded field, Ix feature, Bounded feature) =>
              [doc] ->
              SearchEngine doc key field feature ->
              SearchEngine doc key field feature
insertDocs docs se = foldl' (\se' doc -> insertDoc doc se') se docs

insertDoc :: (Ord key, Ix field, Bounded field, Ix feature, Bounded feature) =>
             doc ->
             SearchEngine doc key field feature ->
             SearchEngine doc key field feature
insertDoc doc se@SearchEngine{ searchConfig = SearchConfig {
                                 documentKey,
                                 extractDocumentTerms,
                                 documentFeatureValue
                               }
                             , searchIndex } =
    let key = documentKey doc
        searchIndex' = SI.insertDoc key (extractDocumentTerms doc)
                                        (documentFeatureValue doc)
                                        searchIndex
        oldDoc       = SI.lookupDocKey searchIndex  key
        newDoc       = SI.lookupDocKey searchIndex' key

     in cacheBM25Context $
        updateCachedFieldLengths oldDoc newDoc $
          se { searchIndex = searchIndex' }

deleteDoc :: (Ord key, Ix field, Bounded field) =>
             key ->
             SearchEngine doc key field feature ->
             SearchEngine doc key field feature
deleteDoc key se@SearchEngine{searchIndex} =
    let searchIndex' = SI.deleteDoc key searchIndex
        oldDoc       = SI.lookupDocKey searchIndex key

     in cacheBM25Context $
        updateCachedFieldLengths oldDoc Nothing $
          se { searchIndex = searchIndex' }

query :: (Ix field, Bounded field, Ix feature, Bounded feature, Ord key) =>
         SearchEngine doc key field feature ->
         [Term] -> [key]
query se@SearchEngine{ searchIndex,
                       searchConfig     = SearchConfig{transformQueryTerm, makeKey},
                       searchRankParams = SearchRankParameters{..} }
      terms =

  let -- Start by transforming/normalising all the query terms.
      -- This can be done differently for each field we search by.
      lookupTerms :: [Term]
      lookupTerms = [ term'
                    | term  <- terms
                    , let transformForField = transformQueryTerm term
                    , term' <- nub [ transformForField field
                                   | field <- range (minBound, maxBound) ]
                    ]

      -- Then we look up all the normalised terms in the index.
      rawresults :: [Maybe (TermId, DocIdSet)]
      rawresults = map (SI.lookupTerm searchIndex) lookupTerms

      -- Check if there is one term then it exactly matches a package
      exactMatch :: Maybe DocId
      exactMatch = case terms of
                     [] -> Nothing
                     [x] -> SI.lookupDocKeyReal searchIndex (makeKey x)
                     (_:_) -> Nothing

      -- For the terms that occur in the index, this gives us the term's id
      -- and the set of documents that the term occurs in.
      termids   :: [TermId]
      docidsets :: [DocIdSet]
      (termids, docidsets) = unzip (catMaybes rawresults)

      -- We looked up the documents that *any* of the term occur in (not all)
      -- so this could be rather a lot of docs if the user uses a few common
      -- terms. Scoring these result docs is a non-trivial cost so we want to
      -- limit the number that we have to score. The standard trick is to
      -- consider the doc sets in the order of size, smallest to biggest. Once
      -- we have gone over a certain threshold of docs then don't bother with
      -- the doc sets for the remaining terms. This tends to work because the
      -- scoring gives lower weight to terms that occur in many documents.
      unrankedResults :: DocIdSet
      unrankedResults = pruneRelevantResults
                          paramResultsetSoftLimit
                          paramResultsetHardLimit
                          docidsets

      --TODO: technically this isn't quite correct. Because each field can
      -- be normalised differently, we can end up with different termids for
      -- the same original search term, and then we score those as if they
      -- were different terms, which makes a difference when the term appears
      -- in multiple fields (exactly the case BM25F is supposed to deal with).
      -- What we ought to have instead is an Array (Int, field) TermId, and
      -- make the scoring use the appropriate termid for each field, but to
      -- consider them the "same" term.
   in rankResults se exactMatch termids (DocIdSet.toList unrankedResults)

rankResults :: forall field key feature doc .
               (Ix field, Bounded field, Ix feature, Bounded feature) =>
               SearchEngine doc key field feature -> Maybe DocId ->
               [TermId] -> [DocId] -> [key]
rankResults se@SearchEngine{searchIndex} exactMatch queryTerms docids =
    maybe id prependExactMatch exactMatch (map snd
    $ sortBy (flip compare `on` fst)
      [ (relevanceScore se queryTerms doctermids docfeatvals, dockey)
      | docid <- docids
      , maybe True (/= docid) exactMatch
      , let (dockey, doctermids, docfeatvals) = SI.lookupDocId searchIndex docid ])
  where
    prependExactMatch :: DocId -> [key] -> [key]
    prependExactMatch docid keys = SI.lookupDocId' searchIndex docid : keys

relevanceScore :: (Ix field, Bounded field, Ix feature, Bounded feature) =>
                  SearchEngine doc key field feature ->
                  [TermId] -> DocTermIds field -> DocFeatVals feature -> Float
relevanceScore SearchEngine{bm25Context} queryTerms doctermids docfeatvals =
    BM25F.score bm25Context doc queryTerms
  where
    doc = indexDocToBM25Doc doctermids docfeatvals

indexDocToBM25Doc :: (Ix field, Bounded field, Ix feature, Bounded feature) =>
                     DocTermIds field ->
                     DocFeatVals feature ->
                     BM25F.Doc TermId field feature
indexDocToBM25Doc doctermids docfeatvals =
    BM25F.Doc {
      BM25F.docFieldLength        = DocTermIds.fieldLength    doctermids,
      BM25F.docFieldTermFrequency = DocTermIds.fieldTermCount doctermids,
      BM25F.docFeatureValue       = DocFeatVals.featureValue docfeatvals
    }

pruneRelevantResults :: Int -> Int -> [DocIdSet] -> DocIdSet
pruneRelevantResults softLimit hardLimit =
    -- Look at the docsets starting with the smallest ones. Smaller docsets
    -- correspond to the rarer terms, which are the ones that score most highly.
    go DocIdSet.empty . sortBy (compare `on` DocIdSet.size)
  where
    go !acc [] = acc
    go !acc (d:ds)
        -- If this is the first one, we add it anyway, otherwise we're in
        -- danger of returning no results at all.
      | DocIdSet.null acc = go d ds
        -- We consider the size our docset would be if we add this extra one...
        -- If it puts us over the hard limit then stop.
      | size > hardLimit  = acc
        -- If it puts us over soft limit then we add it and stop
      | size > softLimit  = DocIdSet.union acc d
        -- Otherwise we can add it and carry on to consider the remainder
      | otherwise         = go (DocIdSet.union acc d) ds
      where
        size = DocIdSet.size acc + DocIdSet.size d

-----------------------------

queryExplain :: (Ix field, Bounded field, Ix feature, Bounded feature, Ord key) =>
                SearchEngine doc key field feature ->
                [Term] -> (Maybe key, [(BM25F.Explanation field feature Term, key)])
queryExplain se@SearchEngine{ searchIndex,
                              searchConfig     = SearchConfig{transformQueryTerm, makeKey},
                              searchRankParams = SearchRankParameters{..} }
      terms =

  -- See 'query' above for explanation. Really we ought to combine them.
  let lookupTerms :: [Term]
      lookupTerms = [ term'
                    | term  <- terms
                    , let transformForField = transformQueryTerm term
                    , term' <- nub [ transformForField field
                                   | field <- range (minBound, maxBound) ]
                    ]

      exactMatch :: Maybe DocId
      exactMatch = case terms of
                     [] -> Nothing
                     [x] -> SI.lookupDocKeyReal searchIndex (makeKey x)
                     (_:_) -> Nothing

      rawresults :: [Maybe (TermId, DocIdSet)]
      rawresults = map (SI.lookupTerm searchIndex) lookupTerms

      termids   :: [TermId]
      docidsets :: [DocIdSet]
      (termids, docidsets) = unzip (catMaybes rawresults)

      unrankedResults :: DocIdSet
      unrankedResults = pruneRelevantResults
                          paramResultsetSoftLimit
                          paramResultsetHardLimit
                          docidsets

   in ( fmap (SI.lookupDocId' searchIndex) exactMatch
      , rankExplainResults se termids (DocIdSet.toList unrankedResults)
      )

rankExplainResults :: (Ix field, Bounded field, Ix feature, Bounded feature) =>
                      SearchEngine doc key field feature ->
                      [TermId] ->
                      [DocId] ->
                      [(BM25F.Explanation field feature Term, key)]
rankExplainResults se@SearchEngine{searchIndex} queryTerms docids =
    sortBy (flip compare `on` (BM25F.overallScore . fst))
      [ (explainRelevanceScore se queryTerms doctermids docfeatvals, dockey)
      | docid <- docids
      , let (dockey, doctermids, docfeatvals) = SI.lookupDocId searchIndex docid ]

explainRelevanceScore :: (Ix field, Bounded field, Ix feature, Bounded feature) =>
                         SearchEngine doc key field feature ->
                         [TermId] ->
                         DocTermIds field ->
                         DocFeatVals feature ->
                         BM25F.Explanation field feature Term
explainRelevanceScore SearchEngine{bm25Context, searchIndex}
                      queryTerms doctermids docfeatvals =
    fmap (SI.getTerm searchIndex) (BM25F.explain bm25Context doc queryTerms)
  where
    doc = indexDocToBM25Doc doctermids docfeatvals

-----------------------------

data NoFeatures = NoFeatures
  deriving (Eq, Ord, Bounded)

instance Ix NoFeatures where
  range   _   = []
  inRange _ _ = False
  index   _ _ = -1

noFeatures :: NoFeatures -> a
noFeatures _ = error "noFeatures"

-----------------------------

instance MemSize key => MemSize (SearchEngine doc key field feature) where
  memSize SearchEngine {searchIndex} = 25 + memSize searchIndex
