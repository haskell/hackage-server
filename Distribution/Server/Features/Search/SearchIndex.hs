{-# LANGUAGE BangPatterns, NamedFieldPuns #-}

module Distribution.Server.Features.Search.SearchIndex (
    SearchIndex,
    Term,
    TermId,
    DocId,

    emptySearchIndex,
    insertDoc,
    deleteDoc,

    docCount,
    lookupTerm,
    lookupTermId,
    lookupDocId,
    lookupDocKey,
    
    getTerm,
    
    invariant,
  ) where

import Distribution.Server.Features.Search.DocIdSet (DocIdSet, DocId)
import qualified Distribution.Server.Features.Search.DocIdSet as DocIdSet
import Distribution.Server.Features.Search.DocTermIds (DocTermIds, TermId, vecIndexIx, vecCreateIx)
import qualified Distribution.Server.Features.Search.DocTermIds as DocTermIds

import Distribution.Server.Framework.MemSize

import Data.Ix (Ix)
import qualified Data.Ix as Ix
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Data.Text (Text)
import Data.List (foldl')

import Control.Exception (assert)

type Term = Text

-- | The search index is essentially a many-to-many mapping between documents
-- and terms. Each document contains many terms and each term occurs in many
-- documents. It is a bidirectional mapping as we need to support lookups in
-- both directions.
--
-- Documents are identified by a key (in Ord) while terms are text values.
-- Inside the index however we assign compact numeric ids to both documents and
-- terms. The advantage of this is a much more compact in-memory representation
-- and the disadvantage is greater complexity. In particular it means we have
-- to manage bidirectional mappings between document keys and ids, and between
-- terms and term ids.
--
-- So the mappings we maintain can be depicted as:
--
-- >  Term   <-- 1:1 -->   TermId
-- >                         ^
-- >                         |
-- >                     many:many
-- >                         |
-- >                         v
-- > DocKey  <-- 1:1 -->   DocId
--
-- For efficiency, these details are exposed in the interface. In particular
-- the mapping from TermId to many DocIds is exposed via a 'DocIdSet',
-- and the mapping from DocIds to TermIds is exposed via 'DocTermIds'.
--
data SearchIndex key field = SearchIndex {
       -- the indexes
       termMap           :: !(Map Term TermInfo),
       termIdMap         :: !(IntMap Term),
       docIdMap          :: !(IntMap (DocInfo key field)),
       docKeyMap         :: !(Map key DocId),

       -- auto-increment key counters
       nextTermId        :: TermId,
       nextDocId         :: DocId
     }
  deriving Show

data TermInfo = TermInfo !TermId !DocIdSet
  deriving Show

data DocInfo key field = DocInfo !key !(DocTermIds field)
  deriving Show


-----------------------
-- SearchIndex basics
--

emptySearchIndex :: SearchIndex key field
emptySearchIndex =
    SearchIndex
      Map.empty
      IntMap.empty
      IntMap.empty
      Map.empty
      minBound
      minBound

checkInvariant :: (Ord key, Ix field, Bounded field) => SearchIndex key field -> SearchIndex key field
checkInvariant si = assert (invariant si) si

invariant :: (Ord key, Ix field, Bounded field) => SearchIndex key field -> Bool
invariant SearchIndex{termMap, termIdMap, docKeyMap, docIdMap} =
      and [ IntMap.lookup (fromEnum termId) termIdMap == Just term
          | (term, (TermInfo termId _)) <- Map.assocs termMap ]
  &&  and [ case Map.lookup term termMap of
              Just (TermInfo termId' _) -> toEnum termId == termId'
              Nothing                   -> False
          | (termId, term) <- IntMap.assocs termIdMap ]
  &&  and [ case IntMap.lookup (fromEnum docId) docIdMap of
              Just (DocInfo docKey' _) -> docKey == docKey'
              Nothing                  -> False
          | (docKey, docId) <- Map.assocs docKeyMap ]
  &&  and [ Map.lookup docKey docKeyMap == Just (toEnum docId)
          | (docId, DocInfo docKey _) <- IntMap.assocs docIdMap ]
  &&  and [ DocIdSet.invariant docIdSet
          | (_term, (TermInfo _ docIdSet)) <- Map.assocs termMap ]
  &&  and [ any (\field -> DocTermIds.fieldTermCount docterms field termId > 0) fields
          | (_term, (TermInfo termId docIdSet)) <- Map.assocs termMap
          , docId <- DocIdSet.toList docIdSet
          , let DocInfo _ docterms = docIdMap IntMap.! fromEnum docId ]
  &&  and [ IntMap.member (fromEnum termid) termIdMap
          | (_docId, DocInfo _ docTerms) <- IntMap.assocs docIdMap
          , field <- fields
          , termid <- DocTermIds.fieldElems docTerms field ]
  where
    fields = Ix.range (minBound, maxBound)


-------------------
-- Lookups
--

docCount :: SearchIndex key field -> Int
docCount SearchIndex{docIdMap} = IntMap.size docIdMap

lookupTerm :: SearchIndex key field -> Term -> Maybe (TermId, DocIdSet)
lookupTerm SearchIndex{termMap} term =
    case Map.lookup term termMap of
      Nothing                         -> Nothing
      Just (TermInfo termid docidset) -> Just (termid, docidset)

lookupTermId :: SearchIndex key field -> TermId -> DocIdSet
lookupTermId SearchIndex{termIdMap, termMap} termid =
    case IntMap.lookup (fromEnum termid) termIdMap of
      Nothing   -> error $ "lookupTermId: not found " ++ show termid
      Just term ->
        case Map.lookup term termMap of
          Nothing                    -> error "lookupTermId: internal error"
          Just (TermInfo _ docidset) -> docidset

lookupDocId :: SearchIndex key field -> DocId -> (key, DocTermIds field)
lookupDocId SearchIndex{docIdMap} docid =
    case IntMap.lookup (fromEnum docid) docIdMap of
      Nothing                       -> error $ "lookupDocId: not found " ++ show docid
      Just (DocInfo key doctermids) -> (key, doctermids)

lookupDocKey :: Ord key => SearchIndex key field -> key -> Maybe (DocTermIds field)
lookupDocKey SearchIndex{docKeyMap, docIdMap} key = do
    case Map.lookup key docKeyMap of
      Nothing    -> Nothing
      Just docid ->
        case IntMap.lookup (fromEnum docid) docIdMap of
          Nothing                        -> error "lookupDocKey: internal error"
          Just (DocInfo _key doctermids) -> Just doctermids


getTerm :: SearchIndex key field -> TermId -> Term
getTerm SearchIndex{termIdMap} termId =
    termIdMap IntMap.! fromEnum termId

getTermId :: SearchIndex key field -> Term -> TermId
getTermId SearchIndex{termMap} term =
    case termMap Map.! term of TermInfo termid _ -> termid

--------------------
-- Insert & delete
--

-- Procedure for adding a new doc...
-- (key, field -> [Term])
-- alloc docid for key
-- add term occurences for docid (include rev map for termid)
-- construct indexdoc now that we have all the term -> termid entries
-- insert indexdoc

-- Procedure for updating a doc...
-- (key, field -> [Term])
-- find docid for key
-- lookup old terms for docid (using termid rev map)
-- calc term occurrences to add, term occurrences to delete
-- add new term occurrences, delete old term occurrences
-- construct indexdoc now that we have all the term -> termid entries
-- insert indexdoc

-- Procedure for deleting a doc...
-- (key, field -> [Term])
-- find docid for key
-- lookup old terms for docid (using termid rev map)
-- delete old term occurrences
-- delete indexdoc

-- | This is the representation for documents to be added to the index.
-- Documents may 
--
type DocTerms field = field -> [Term]

insertDoc :: (Ord key, Ix field, Bounded field) =>
              key -> DocTerms field ->
              SearchIndex key field -> SearchIndex key field
insertDoc key userDoc si@SearchIndex{docKeyMap}
  | Just docid <- Map.lookup key docKeyMap
  = -- Some older version of the doc is already present in the index,
    -- So we keep its docid. Now have to update the doc itself
    -- and update the terms by removing old ones and adding new ones.
    let (_,oldDoc)  = lookupDocId si docid
        newDoc      = memoiseDocTerms userDoc
        newTerms    = docTermSet newDoc
        oldTerms    = docTermIdsTermSet si oldDoc
        -- We optimise for the typical case of significant overlap between
        -- the terms in the old and new versions of the document.
        delTerms    = oldTerms `Set.difference` newTerms
        addTerms    = newTerms `Set.difference` oldTerms

     -- Note: adding the doc relies on all the terms being in the termMap
     -- already, so we first add all the term occurences for the docid.
     in checkInvariant
      . insertDocIdToDocEntry docid key newDoc
      . insertTermToDocIdEntries (Set.toList addTerms) docid
      . deleteTermToDocIdEntries (Set.toList delTerms) docid
      $ si

  | otherwise
  = -- We're dealing with a new doc, so allocate a docid for the key
    let (si', docid) = allocFreshDocId si
        newDoc       = memoiseDocTerms userDoc
        addTerms     = docTermSet newDoc

     -- Note: adding the doc relies on all the terms being in the termMap
     -- already, so we first add all the term occurences for the docid.
     in checkInvariant
      . insertDocIdToDocEntry docid key newDoc
       -- (\si'' -> insertDocIdToDocEntry docid key (mkDocTermIds si'' newDoc) si'')
      . insertDocKeyToIdEntry key docid
      . insertTermToDocIdEntries (Set.toList addTerms) docid
      $ si'

deleteDoc :: (Ord key, Ix field, Bounded field) =>
             key -> SearchIndex key field -> SearchIndex key field
deleteDoc key si@SearchIndex{docKeyMap}
  | Just docid <- Map.lookup key docKeyMap
  = let (_,oldDoc) = lookupDocId si docid
        oldTerms   = docTermIdsTermSet si oldDoc
     in checkInvariant
      . deleteDocEntry docid key
      . deleteTermToDocIdEntries (Set.toList oldTerms) docid
      $ si
  
  | otherwise = si


----------------------------------
-- Insert & delete support utils
--


memoiseDocTerms :: (Ix field, Bounded field) => DocTerms field -> DocTerms field
memoiseDocTerms docTermsFn =
    \field -> vecIndexIx vec field
  where
    vec = vecCreateIx docTermsFn

docTermSet :: (Bounded t, Ix t) => DocTerms t -> Set.Set Term
docTermSet docterms =
    Set.unions [ Set.fromList (docterms field)
               | field <- Ix.range (minBound, maxBound) ]

docTermIdsTermSet :: (Bounded field, Ix field) =>
                     SearchIndex key field -> DocTermIds field -> Set.Set Term
docTermIdsTermSet si doctermids =
    Set.unions [ Set.fromList terms
               | field <- Ix.range (minBound, maxBound)
               , let termids = DocTermIds.fieldElems doctermids field
                     terms   = map (getTerm si) termids ]

--
-- The Term <-> DocId mapping
--

-- | Add an entry into the 'Term' to 'DocId' mapping.
insertTermToDocIdEntry :: Term -> DocId -> SearchIndex key field -> SearchIndex key field
insertTermToDocIdEntry term !docid si@SearchIndex{termMap, termIdMap, nextTermId} =
    case Map.lookup term termMap of
      Nothing ->
        let !termInfo' = TermInfo nextTermId (DocIdSet.singleton docid)
         in si { termMap    = Map.insert term termInfo' termMap
               , termIdMap  = IntMap.insert (fromEnum nextTermId) term termIdMap
               , nextTermId = succ nextTermId }

      Just (TermInfo termId docIdSet) ->
        let !termInfo' = TermInfo termId (DocIdSet.insert docid docIdSet)
         in si { termMap = Map.insert term termInfo' termMap }

-- | Add multiple entries into the 'Term' to 'DocId' mapping: many terms that
-- map to the same document.
insertTermToDocIdEntries :: [Term] -> DocId -> SearchIndex key field -> SearchIndex key field
insertTermToDocIdEntries terms !docid si =
    foldl' (\si' term -> insertTermToDocIdEntry term docid si') si terms

-- | Delete an entry from the 'Term' to 'DocId' mapping.
deleteTermToDocIdEntry :: Term -> DocId -> SearchIndex key field -> SearchIndex key field
deleteTermToDocIdEntry term !docid si@SearchIndex{termMap, termIdMap} =
    case  Map.lookup term termMap of
      Nothing -> si
      Just (TermInfo termId docIdSet) ->
        let docIdSet' = DocIdSet.delete docid docIdSet
            termInfo' = TermInfo termId docIdSet'
        in if DocIdSet.null docIdSet'
            then si { termMap = Map.delete term termMap
                    , termIdMap = IntMap.delete (fromEnum termId) termIdMap }
            else si { termMap = Map.insert term termInfo' termMap }

-- | Delete multiple entries from the 'Term' to 'DocId' mapping: many terms
-- that map to the same document.
deleteTermToDocIdEntries :: [Term] -> DocId -> SearchIndex key field -> SearchIndex key field
deleteTermToDocIdEntries terms !docid si =
    foldl' (\si' term -> deleteTermToDocIdEntry term docid si') si terms

--
-- The DocId <-> Doc mapping
--

allocFreshDocId :: SearchIndex key field -> (SearchIndex key field, DocId)
allocFreshDocId si@SearchIndex{nextDocId} =
    let !si' = si { nextDocId = succ nextDocId }
     in (si', nextDocId)

insertDocKeyToIdEntry :: Ord key => key -> DocId ->
                         SearchIndex key field -> SearchIndex key field
insertDocKeyToIdEntry dockey !docid si@SearchIndex{docKeyMap} =
    si { docKeyMap = Map.insert dockey docid docKeyMap }

insertDocIdToDocEntry :: (Ix field, Bounded field) => DocId -> key -> DocTerms field ->
                         SearchIndex key field -> SearchIndex key field
insertDocIdToDocEntry !docid dockey docterms si@SearchIndex{docIdMap} =
    let doctermids = DocTermIds.create (map (getTermId si) . docterms)
        !docinfo   = DocInfo dockey doctermids
     in si { docIdMap  = IntMap.insert (fromEnum docid) docinfo docIdMap }

deleteDocEntry :: Ord key => DocId -> key ->
                  SearchIndex key field -> SearchIndex key field
deleteDocEntry docid key si@SearchIndex{docIdMap, docKeyMap} =
     si { docIdMap  = IntMap.delete (fromEnum docid) docIdMap
        , docKeyMap = Map.delete key docKeyMap }


----------------------
-- MemSize instances

instance MemSize key => MemSize (SearchIndex key field) where
  memSize (SearchIndex a b c d e f) = memSize6 a b c d e f

instance MemSize TermInfo where
  memSize (TermInfo a b) = memSize2 a b

instance MemSize key => MemSize (DocInfo key field) where
  memSize (DocInfo a b) = memSize2 a b

