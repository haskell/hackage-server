{-# LANGUAGE BangPatterns, NamedFieldPuns, RecordWildCards #-}

module Distribution.Server.Features.Search.SearchEngine (
    SearchEngine,
    SearchConfig(..),
    SearchRankParameters(..),
    Term,
    initSearchEngine,
    insertDocs,
    insertDoc,
    deleteDoc,
    query,

    queryExplain,
    BM25F.Explanation(..),
    withRankParams,

    invariant,
  ) where

import Distribution.Server.Features.Search.SearchIndex (SearchIndex, Term, TermId)
import qualified Distribution.Server.Features.Search.SearchIndex as SI
import Distribution.Server.Features.Search.DocIdSet (DocIdSet, DocId)
import qualified Distribution.Server.Features.Search.DocIdSet as DocIdSet
import Distribution.Server.Features.Search.DocTermIds (DocTermIds)
import qualified Distribution.Server.Features.Search.DocTermIds as DocTermIds
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

data SearchConfig doc key field = SearchConfig {
       documentKey          :: doc -> key,
       extractDocumentTerms :: doc -> field -> [Term],
       transformQueryTerm   :: Term -> field -> Term
     }

data SearchRankParameters field = SearchRankParameters {
       paramK1                 :: !Float,
       paramB                  :: field -> Float,
       paramFieldWeights       :: field -> Float,
       paramResultsetSoftLimit :: !Int,
       paramResultsetHardLimit :: !Int
     }

data SearchEngine doc key field = SearchEngine {
       searchIndex      :: !(SearchIndex      key field),
       searchConfig     :: !(SearchConfig doc key field),
       searchRankParams :: !(SearchRankParameters field),

       -- cached info
       sumFieldLengths :: !(UArray field Int),
       bm25Context     :: BM25F.Context TermId field
     }

initSearchEngine :: (Ix field, Bounded field) =>
                    SearchConfig doc key field ->
                    SearchRankParameters field ->
                    SearchEngine doc key field
initSearchEngine config params =
    cacheBM25Context
      SearchEngine {
        searchIndex      = SI.emptySearchIndex,
        searchConfig     = config,
        searchRankParams = params,
        sumFieldLengths  = listArray (minBound, maxBound) (repeat 0),
        bm25Context      = undefined
      }

withRankParams :: SearchEngine doc key field -> SearchRankParameters field -> SearchEngine doc key field
withRankParams se ps = se { searchRankParams = ps }

invariant :: (Ord key, Ix field, Bounded field) => SearchEngine doc key field -> Bool
invariant SearchEngine{searchIndex} =
    SI.invariant searchIndex
-- && check caches

cacheBM25Context :: Ix field => SearchEngine doc key field -> SearchEngine doc key field
cacheBM25Context
    se@SearchEngine {
      searchRankParams = SearchRankParameters{paramK1, paramB, paramFieldWeights},
      searchIndex      = si,
      sumFieldLengths
    }
  = se { bm25Context = bm25Context' }
  where
    bm25Context' = BM25F.Context {
      BM25F.numDocsTotal    = SI.docCount si,
      BM25F.avgFieldLength  = \f -> fromIntegral (sumFieldLengths ! f)
                            / fromIntegral (SI.docCount si),
      BM25F.numDocsWithTerm = DocIdSet.size . SI.lookupTermId si,
      BM25F.paramK1         = paramK1,
      BM25F.paramB          = paramB,
      BM25F.fieldWeight     = paramFieldWeights
    }

updateCachedFieldLengths :: (Ix field, Bounded field) =>
                            Maybe (DocTermIds field) -> Maybe (DocTermIds field) ->
                            SearchEngine doc key field -> SearchEngine doc key field
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

insertDocs :: (Ord key, Ix field, Bounded field) => [doc] -> SearchEngine doc key field -> SearchEngine doc key field
insertDocs docs se = foldl' (\se' doc -> insertDoc doc se') se docs

insertDoc :: (Ord key, Ix field, Bounded field) => doc -> SearchEngine doc key field -> SearchEngine doc key field
insertDoc doc se@SearchEngine{ searchConfig = SearchConfig{documentKey, extractDocumentTerms}
                             , searchIndex } =
    let key = documentKey doc
        searchIndex' = SI.insertDoc key (extractDocumentTerms doc) searchIndex
        oldDoc       = SI.lookupDocKey searchIndex  key
        newDoc       = SI.lookupDocKey searchIndex' key

     in cacheBM25Context $
        updateCachedFieldLengths oldDoc newDoc $
          se { searchIndex = searchIndex' }

deleteDoc :: (Ord key, Ix field, Bounded field) => key -> SearchEngine doc key field -> SearchEngine doc key field
deleteDoc key se@SearchEngine{searchIndex} =
    let searchIndex' = SI.deleteDoc key searchIndex
        oldDoc       = SI.lookupDocKey searchIndex key

     in cacheBM25Context $
        updateCachedFieldLengths oldDoc Nothing $
          se { searchIndex = searchIndex' }

query :: (Ix field, Bounded field) => SearchEngine doc key field -> [Term] -> [key]
query se@SearchEngine{ searchIndex,
                       searchConfig     = SearchConfig{transformQueryTerm},
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
   in rankResults se termids (DocIdSet.toList unrankedResults)

rankResults :: (Ix field, Bounded field) => SearchEngine doc key field -> 
               [TermId] -> [DocId] -> [key]
rankResults se@SearchEngine{searchIndex} queryTerms docids =
    map snd
  $ sortBy (flip compare `on` fst)
      [ (relevanceScore se queryTerms doctermids, dockey)
      | docid <- docids
      , let (dockey, doctermids) = SI.lookupDocId searchIndex docid ]

relevanceScore :: (Ix field, Bounded field) => SearchEngine doc key field ->
                  [TermId] -> DocTermIds field -> Float
relevanceScore SearchEngine{bm25Context} queryTerms doctermids =
    BM25F.score bm25Context doc queryTerms
  where
    doc = indexDocToBM25Doc doctermids

indexDocToBM25Doc :: (Ix field, Bounded field) => DocTermIds field -> BM25F.Doc TermId field
indexDocToBM25Doc doctermids =
    BM25F.Doc {
      BM25F.docFieldLength        = DocTermIds.fieldLength    doctermids,
      BM25F.docFieldTermFrequency = DocTermIds.fieldTermCount doctermids
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

queryExplain :: (Ix field, Bounded field) => SearchEngine doc key field -> [Term] -> [(BM25F.Explanation field Term, key)]
queryExplain se@SearchEngine{ searchIndex,
                              searchConfig     = SearchConfig{transformQueryTerm},
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

   in rankExplainResults se termids (DocIdSet.toList unrankedResults)

rankExplainResults :: (Ix field, Bounded field) => SearchEngine doc key field -> 
               [TermId] -> [DocId] -> [(BM25F.Explanation field Term, key)]
rankExplainResults se@SearchEngine{searchIndex} queryTerms docids =
    sortBy (flip compare `on` (BM25F.overallScore . fst))
      [ (explainRelevanceScore se queryTerms doctermids, dockey)
      | docid <- docids
      , let (dockey, doctermids) = SI.lookupDocId searchIndex docid ]

explainRelevanceScore :: (Ix field, Bounded field) => SearchEngine doc key field ->
                         [TermId] -> DocTermIds field -> BM25F.Explanation field Term
explainRelevanceScore SearchEngine{bm25Context, searchIndex} queryTerms doctermids =
    fmap (SI.getTerm searchIndex) (BM25F.explain bm25Context doc queryTerms)
  where
    doc = indexDocToBM25Doc doctermids

-----------------------------

instance MemSize key => MemSize (SearchEngine doc key field) where
  memSize SearchEngine {searchIndex} = 25 + memSize searchIndex

