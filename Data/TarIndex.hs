{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}

module Data.TarIndex {-(
  
    TarIndex,
    TarIndexEntry(..),
    TarEntryOffset,
      
    lookup,
    construct,
    
    prop_lookup, prop,

  )-} where

import Data.Typeable

import qualified Data.StringTable as StringTable
import Data.StringTable (StringTable)
import qualified Data.IntTrie as IntTrie

import Data.IntTrie (IntTrie)
import qualified Data.List as List
import qualified System.FilePath as FilePath
import Prelude hiding (lookup)

import Happstack.Data

-- | An index of the entries in a tar file. This lets us look up a filename
-- within the tar file and find out where in the tar file (ie the file offset)
-- that entry occurs.
--
data TarIndex = TarIndex

  -- As an example of how the mapping works, consider these example files:
  --   "foo/bar.hs" at offset 0
  --   "foo/baz.hs" at offset 1024
  --
  -- We split the paths into components and enumerate them.
  --   { "foo" -> TokenId 0, "bar.hs" -> TokenId 1,  "baz.hs" -> TokenId 2 }
  --
  -- We convert paths into sequences of 'TokenId's, i.e.
  --   "foo/bar.hs" becomes [PathComponentId 0, PathComponentId 1]
  --   "foo/baz.hs" becomes [PathComponentId 0, PathComponentId 2]
  --
  -- We use a trie mapping sequences of 'PathComponentId's to the entry offset:
  --  { [PathComponentId 0, PathComponentId 1] -> offset 0
  --  , [PathComponentId 0, PathComponentId 1] -> offset 1024 }

  -- | The mapping of filepath components as strings to ids.
  !(StringTable PathComponentId)

  -- Mapping of sequences of filepath component ids to tar entry offsets.
  !(IntTrie PathComponentId TarEntryOffset)
  deriving (Show, Typeable)

instance Version TarIndex where

data TarIndexEntry = TarFileEntry !TarEntryOffset
                   | TarDir [FilePath]
  deriving (Show, Typeable)

instance Version TarIndexEntry where


newtype PathComponentId = PathComponentId Int
  deriving (Eq, Ord, Enum, Show, Typeable)

instance Version PathComponentId where
    mode = Primitive

type TarEntryOffset = Int

$(deriveSerializeFor
  [ ''TarIndex
  , ''PathComponentId
  , ''TarIndexEntry
  ]
 )

-- | Look up a given filepath in the index. It may return a 'TarFileEntry'
-- containing the offset and length of the file within the tar file, or if
-- the filepath identifies a directory then it returns a 'TarDir' containing
-- the list of files within that directory.
--
lookup :: TarIndex -> FilePath -> Maybe TarIndexEntry
lookup (TarIndex pathTable pathTrie) path =
    case toComponentIds pathTable path of
      Nothing   -> Nothing
      Just path -> fmap (mkIndexEntry path) (IntTrie.lookup pathTrie path)
  where
    pathComponentIds = toComponentIds pathTable path

    mkIndexEntry path (IntTrie.Entry offset)        = TarFileEntry offset
    mkIndexEntry path (IntTrie.Completions entries) =
      TarDir [ fromComponentIds pathTable [entry]
             | entry <- entries ]

-- | Construct a 'TarIndex' from a list of filepaths and their corresponding
--
construct :: [(FilePath, TarEntryOffset)] -> TarIndex
construct pathsOffsets = TarIndex pathTable pathTrie
  where
    pathComponents = concatMap (FilePath.splitDirectories . fst) pathsOffsets
    pathTable = StringTable.construct pathComponents
    pathTrie  = IntTrie.construct
                  [ (cids, offset)
                  | (path, offset) <- pathsOffsets
                  , let Just cids = toComponentIds pathTable path ]

toComponentIds :: StringTable PathComponentId -> FilePath -> Maybe [PathComponentId]
toComponentIds table = lookupComponents [] . FilePath.splitDirectories
  where
    lookupComponents cs' []     = Just (reverse cs')
    lookupComponents cs' (c:cs) = case StringTable.lookup table c of
      Nothing  -> Nothing
      Just cid -> lookupComponents (cid:cs') cs
      
fromComponentIds :: StringTable PathComponentId -> [PathComponentId] -> FilePath
fromComponentIds table = FilePath.joinPath . map (StringTable.index table)

-- properties of a finite mapping...

prop_lookup :: [(FilePath, TarEntryOffset)] -> FilePath -> Bool
prop_lookup xs x =
  case (lookup (construct xs) x, List.lookup x xs) of
    (Nothing,                    Nothing)      -> True
    (Just (TarFileEntry offset), Just offset') -> offset == offset'
    _                                          -> False

prop :: [(FilePath, TarEntryOffset)] -> Bool
prop paths
  | not $ StringTable.prop pathbits = error "TarIndex: bad string table"
  | not $ IntTrie.prop intpaths     = error "TarIndex: bad int trie"
  | not $ prop'                     = error "TarIndex: bad prop"
  | otherwise                       = True

  where
    index@(TarIndex pathTable _) = construct paths

    pathbits = concatMap (FilePath.splitDirectories . fst) paths
    intpaths = [ (cids, offset)
               | (path, offset) <- paths
               , let Just cids = toComponentIds pathTable path ]
    prop' = flip all paths $ \(file, offset) ->
      case lookup index file of
        Just (TarFileEntry offset') -> offset' == offset
        _                           -> False


example0 :: [(FilePath, Int)]
example0 =
  [("foo-1.0/foo-1.0.cabal", 512)   -- tar block 1
  ,("foo-1.0/LICENSE",       2048)  -- tar block 4
  ,("foo-1.0/Data/Foo.hs",   4096)] -- tar block 8
