{-# LANGUAGE DeriveDataTypeable, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
-- TypeOperators, TypeSynonymInstances, TypeFamilies

module Distribution.Server.Util.NameIndex where

import Data.Map (Map)
import Data.Typeable (Typeable)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char (toLower)
import Data.List (unfoldr, foldl')
import Data.Maybe (maybeToList)
import Control.DeepSeq
import Data.SafeCopy

import Distribution.Server.Framework.MemSize

-- | Case-insensitive name search. This is meant to be an enhanced set of
-- names, not a full text search. It's also meant to be a sort of a short-term
-- solution for name suggestion searches; e.g., package searches should also
-- consider the tagline of a package.
data NameIndex = NameIndex {
    -- | This is the mapping from case-insensitive search term -> name.
    nameIndex :: Map String (Set String),
    -- | This is the set of names.
    storedNamesIndex :: Set String,
    -- | This is the specification of the type of generator, mainly here because
    -- functions can't be serialized. Just str means to break on any char in
    -- str (breakGenerator); Nothing is defaultGenerator.
    nameGenType :: Maybe [Char],
    -- | This is the generator of search terms from names.
    nameSearchGenerator :: String -> [String]
} deriving (Typeable)

emptyNameIndex :: Maybe [Char] -> NameIndex
emptyNameIndex gen = NameIndex Map.empty Set.empty gen $ case gen of
    Nothing -> defaultGenerator
    Just st -> breakGenerator st

defaultGenerator :: String -> [String]
defaultGenerator name = [name]

breakGenerator :: [Char] -> String -> [String]
breakGenerator breakStr name = name:unfoldr unfoldName name
  where unfoldName str = case break (`elem` breakStr) str of
            ([], _) -> Nothing
            (_, []) -> Nothing
            (_, _:str') -> Just (str', str')

constructIndex :: [String] -> Maybe [Char] -> NameIndex
constructIndex strs gen = foldl' (flip addName) (emptyNameIndex gen) strs

addName :: String -> NameIndex -> NameIndex
addName caseName (NameIndex index stored gen' gen) =
    let name = map toLower caseName
        nameSet = Set.singleton caseName
        forName = Map.fromList $ map (\term -> (term, nameSet)) (gen name)
    in NameIndex (Map.unionWith Set.union index forName)
                 (Set.insert caseName stored) gen' gen

deleteName :: String -> NameIndex -> NameIndex
deleteName caseName (NameIndex index stored gen' gen) =
    let name = map toLower caseName
        nameSet = Set.singleton caseName
        forName = Map.fromList $ map (\term -> (term, nameSet)) (gen name)
    in NameIndex (Map.differenceWith (\a b -> keepSet $ Set.difference a b) index forName)
                 (Set.delete caseName stored) gen' gen
  where keepSet s = if Set.null s then Nothing else Just s

lookupName :: String -> NameIndex -> Set String
lookupName caseName (NameIndex index _ _ _) =
    Map.findWithDefault Set.empty (map toLower caseName) index

lookupPrefix :: String -> NameIndex -> Set String
lookupPrefix caseName (NameIndex index _ _ _) =
    let name = map toLower caseName
        (_, mentry, startTree) = Map.splitLookup name index
        -- the idea is, select all names in the range [name, mapLast succ name)
        -- an alternate idea would just be to takeWhile (`isPrefixOf` name)
        (totalTree, _, _) = Map.splitLookup (mapLast succ name) startTree
        nameSets = maybeToList mentry ++ Map.elems totalTree
    in Set.unions nameSets

takeSetPrefix :: String -> Set String -> Set String
takeSetPrefix name strs =
    let (_, present, startSet) = Set.splitMember name strs
        (totalSet, _, _) = Set.splitMember (mapLast succ name) startSet
    in (if present then Set.insert name else id) totalSet

-- | Map only the last element of a list
mapLast :: (a -> a) -> [a] -> [a]
mapLast f (x:[]) = f x:[]
mapLast f (x:xs) = x:mapLast f xs
mapLast _ [] = []

-- store arguments which can be sent to constructIndex :: [String] -> Maybe [Char] -> NameIndex
instance SafeCopy NameIndex where
    putCopy index = contain $ safePut (nameGenType index) >> safePut (storedNamesIndex index)
    getCopy = contain $ do
        gen <- safeGet
        index <- safeGet
        return $ constructIndex (Set.toList index) gen

instance NFData NameIndex where
    rnf (NameIndex a b _ _) = rnf a `seq` rnf b

instance MemSize NameIndex where
    memSize (NameIndex a b c d) = memSize4 a b c d
