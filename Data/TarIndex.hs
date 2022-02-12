{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}

module Data.TarIndex (

    TarIndex,
    TarIndexEntry(..),
    TarEntryOffset,

    lookup,
    construct,

#ifdef TESTS
    prop_lookup, prop,
#endif
  ) where

import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)

import Codec.Archive.Tar (Entry(..), EntryContent(..), Entries(..), entryPath)
import qualified Data.StringTable as StringTable
import Data.StringTable (StringTable)
import qualified Data.IntTrie as IntTrie

import Data.IntTrie (IntTrie)
import qualified System.FilePath.Posix as FilePath
import Prelude hiding (lookup)
#ifdef TESTS
import qualified Prelude
#endif

import Distribution.Server.Framework.MemSize


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
  --  , [PathComponentId 0, PathComponentId 2] -> offset 1024 }

  !(StringTable PathComponentId)            -- ^ The mapping of filepath components as strings to ids.
  !(IntTrie PathComponentId TarEntryOffset) -- ^ Mapping of sequences of filepath component ids to tar entry offsets.
  deriving (Show, Typeable)


data TarIndexEntry = TarFileEntry !TarEntryOffset
                   | TarDir [(FilePath, TarIndexEntry)]
  deriving (Show, Typeable)


newtype PathComponentId = PathComponentId Int
  deriving (Eq, Ord, Enum, Show, Typeable)

type TarEntryOffset = Int

$(deriveSafeCopy 0 'base ''TarIndex)
$(deriveSafeCopy 0 'base ''PathComponentId)
$(deriveSafeCopy 0 'base ''TarIndexEntry)

instance MemSize TarIndex where
    memSize (TarIndex a b) = memSize2 a b


-- | Look up a given filepath in the index. It may return a 'TarFileEntry'
-- containing the offset and length of the file within the tar file, or if
-- the filepath identifies a directory then it returns a 'TarDir' containing
-- the list of files within that directory.
--
lookup :: TarIndex -> FilePath -> Maybe TarIndexEntry
lookup (TarIndex pathTable pathTrie) path = do
    fpath  <- toComponentIds pathTable path
    tentry <- IntTrie.lookup pathTrie fpath
    return (mkIndexEntry tentry)
  where
    mkIndexEntry (IntTrie.Entry offset)        = TarFileEntry offset
    mkIndexEntry (IntTrie.Completions entries) =
      TarDir [ (fromComponentId pathTable key, mkIndexEntry entry)
             | (key, entry) <- entries ]

data IndexBuilder = IndexBuilder [(FilePath, TarEntryOffset)]
                                 {-# UNPACK #-} !TarEntryOffset

initialIndexBuilder :: IndexBuilder
initialIndexBuilder = IndexBuilder [] 0

accumIndexBuilder :: Entry -> IndexBuilder -> IndexBuilder
accumIndexBuilder entry (IndexBuilder acc nextOffset) =
    IndexBuilder ((entryPath entry, nextOffset):acc) nextOffset'
  where
    nextOffset' = nextOffset + 1
                + case entryContent entry of
                   NormalFile     _   size -> blocks size
                   OtherEntryType _ _ size -> blocks size
                   _                           -> 0
    blocks size = 1 + ((fromIntegral size - 1) `div` 512)

finaliseIndexBuilder :: IndexBuilder -> TarIndex
finaliseIndexBuilder (IndexBuilder pathsOffsets _) =
    TarIndex pathTable pathTrie
  where
    pathComponents = concatMap (FilePath.splitDirectories . fst) pathsOffsets
    pathTable = StringTable.construct pathComponents
    pathTrie  = IntTrie.construct
                  [ (cids, offset)
                  | (path, offset) <- pathsOffsets
                  , let Just cids = toComponentIds pathTable path ]

-- | Construct a 'TarIndex' from a sequence of tar 'Entries'. The 'Entries' are
-- assumed to start at offset @0@ within a file.
--
construct :: Entries e -> Either e TarIndex
construct = go initialIndexBuilder
  where
    go builder (Next e es) = go (accumIndexBuilder e builder) es
    go builder  Done       = Right $! finaliseIndexBuilder builder
    go _       (Fail err)  = Left err

toComponentIds :: StringTable PathComponentId -> FilePath -> Maybe [PathComponentId]
toComponentIds table = lookupComponents [] . FilePath.splitDirectories
  where
    lookupComponents cs' []     = Just (reverse cs')
    lookupComponents cs' (c:cs) = case StringTable.lookup table c of
      Nothing  -> Nothing
      Just cid -> lookupComponents (cid:cs') cs

fromComponentId :: StringTable PathComponentId -> PathComponentId -> FilePath
fromComponentId table = StringTable.index table

#ifdef TESTS

-- properties of a finite mapping...

prop_lookup :: [(FilePath, TarEntryOffset)] -> FilePath -> Bool
prop_lookup xs x =
  case (lookup (construct xs) x, Prelude.lookup x xs) of
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

#endif
