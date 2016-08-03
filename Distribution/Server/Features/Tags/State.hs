{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Distribution.Server.Features.Tags.State where
import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import qualified Distribution.ParseUtils   as Parse
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Text
import Distribution.Package
import qualified Text.PrettyPrint as Disp

import Data.Acid (Query, Update, makeAcidic)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (liftM2)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Control.Monad.State (get, put, modify)
import Control.Monad.Reader (ask, asks)
import Control.DeepSeq

newtype TagList = TagList [Tag] deriving (Show, Typeable)
instance Text TagList where
    disp (TagList tags) = Disp.hsep . Disp.punctuate Disp.comma $ map disp tags
    parse = fmap TagList $ Parse.skipSpaces >> Parse.parseCommaList parse

-- A tag is a string describing a package; presently the preferred word-separation
-- character is the dash.
newtype Tag = Tag String deriving (Show, Typeable, Ord, Eq, NFData, MemSize)
instance Text Tag where
    disp (Tag tag) = Disp.text tag
    parse = do
        -- adding 'many1 $ do' here would allow multiword tags.
        -- spaces aren't very aesthetic in URIs, though.
        strs <- do
            t <- liftM2 (:) (Parse.satisfy tagInitialChar)
               $ Parse.munch1 tagLaterChar
            Parse.skipSpaces
            return t
        return $ Tag strs

tagInitialChar, tagLaterChar :: Char -> Bool
-- reserve + and - first-letters for queries
tagInitialChar c = Char.isAlphaNum c || c `elem` ".#*"
tagLaterChar   c = Char.isAlphaNum c || c `elem` "-+#*."

-- mutilates a string to appease the parser
tagify :: String -> Tag
tagify (x:xs) = Tag $ (if tagInitialChar x then (x:) else id) $ tagify' xs
  where tagify' (c:cs) | tagLaterChar c = c:tagify' cs
        tagify' (c:cs) | c `elem` " /\\" = '-':tagify' cs -- dash is the preferred word separator?
        tagify' (_:cs) = tagify' cs
        tagify' [] = []
tagify [] = Tag ""

data PackageTags = PackageTags {
    -- the primary index
    packageTags :: Map PackageName (Set Tag),
    -- a secondary reverse mapping
    tagPackages :: Map Tag (Set PackageName),
    -- Packagename (Proposed Additions, Proposed Deletions)
    reviewTags :: Map PackageName (Set Tag, Set Tag)
} deriving (Eq, Show, Typeable)


data TagAlias = TagAlias (Map Tag (Set Tag)) deriving (Eq, Show, Typeable)

addTagAlias :: Tag -> Tag -> Update TagAlias ()
addTagAlias tag alias = do
        TagAlias  m <- get
        put (TagAlias (Map.insertWith Set.union tag (Set.singleton alias) m))

lookupTagAlias :: Tag -> Query TagAlias (Maybe (Set Tag))
lookupTagAlias tag
    = do TagAlias m <- ask
         return (Map.lookup tag m)

getTagAlias :: Tag -> Query TagAlias Tag
getTagAlias tag
    = do TagAlias m <- ask
         if tag `elem` Map.keys m
            then return tag
            else if tag `Set.member` foldr Set.union Set.empty (Map.elems m)
                then return $ head (Map.keys $ Map.filter (tag `Set.member`) m)
                else return tag

emptyPackageTags :: PackageTags
emptyPackageTags = PackageTags Map.empty Map.empty Map.empty

emptyTagAlias :: TagAlias
emptyTagAlias = TagAlias Map.empty

tagToPackages :: Tag -> PackageTags -> Set PackageName
tagToPackages tag = Map.findWithDefault Set.empty tag . tagPackages

packageToTags :: PackageName -> PackageTags -> Set Tag
packageToTags pkg = Map.findWithDefault Set.empty pkg . packageTags

alterTags :: PackageName -> Maybe (Set Tag) -> PackageTags -> PackageTags
alterTags name mtagList pt@(PackageTags tags packages _) =
    let tagList = fromMaybe Set.empty mtagList
        oldTags = Map.findWithDefault Set.empty name tags
        adjustPlusTags pkgMap tag' = addSetMap tag' name pkgMap
        adjustMinusTags pkgMap tag' = removeSetMap tag' name pkgMap
        packages' = flip (foldl' adjustPlusTags) (Set.toList $ Set.difference tagList oldTags)
                  $ foldl' adjustMinusTags packages (Set.toList $ Set.difference oldTags tagList)
    in pt{
        packageTags = Map.alter (const mtagList) name tags,
        tagPackages = packages'
    }

setTags :: PackageName -> Set Tag -> PackageTags -> PackageTags
setTags pkgname tagList = alterTags pkgname (keepSet tagList)

setAliases :: Tag -> Set Tag -> TagAlias -> TagAlias
setAliases tag aliases (TagAlias ta) = TagAlias (Map.insertWith Set.union tag aliases ta)

deletePackageTags :: PackageName -> PackageTags -> PackageTags
deletePackageTags name = alterTags name Nothing

addTag :: PackageName -> Tag -> PackageTags -> Maybe PackageTags
addTag name tag (PackageTags tags packages review) =
    let existing = Map.findWithDefault Set.empty name tags
    in if tag `Set.member` existing then Nothing else Just $ PackageTags (addSetMap name tag tags)
                                   (addSetMap tag name packages)
                                   review

removeTag :: PackageName -> Tag -> PackageTags -> Maybe PackageTags
removeTag name tag (PackageTags tags packages review) =
    let existing = Map.findWithDefault Set.empty name tags
    in if tag `Set.member` existing then Just $ PackageTags (removeSetMap name tag tags)
                                  (removeSetMap tag name packages)
                                  review else Nothing

addSetMap :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
addSetMap key val = Map.alter (Just . Set.insert val . fromMaybe Set.empty) key

removeSetMap :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)
removeSetMap key val = Map.update (keepSet . Set.delete val) key

alterTag :: Tag -> Maybe (Set PackageName) -> PackageTags -> PackageTags
alterTag tag mpkgList (PackageTags tags packages review) =
    let pkgList = fromMaybe Set.empty mpkgList
        oldPkgs = Map.findWithDefault Set.empty tag packages
        adjustPlusPkgs tagMap name' = addSetMap name' tag tagMap
        adjustMinusPkgs tagMap name' = removeSetMap name' tag tagMap
        tags' = flip (foldl' adjustPlusPkgs) (Set.toList $ Set.difference pkgList oldPkgs)
              $ foldl' adjustMinusPkgs tags (Set.toList $ Set.difference oldPkgs pkgList)
    in PackageTags tags' (Map.alter (const mpkgList) tag packages) review

keepSet :: Ord a => Set a -> Maybe (Set a)
keepSet s = if Set.null s then Nothing else Just s

-- these three are not currently exposed as happstack-state functions
setTag :: Tag -> Set PackageName -> PackageTags -> PackageTags
setTag tag pkgs = alterTag tag (keepSet pkgs)

deleteTag :: Tag -> PackageTags -> PackageTags
deleteTag tag = alterTag tag Nothing

renameTag :: Tag -> Tag -> PackageTags -> PackageTags
renameTag tag tag' pkgTags@(PackageTags _ packages _) =
    let oldPkgs = Map.findWithDefault Set.empty tag packages
    in setTag tag' oldPkgs . deleteTag tag $ pkgTags
-------------------------------------------------------------------------------

$(deriveSafeCopy 0 'base ''Tag)
$(deriveSafeCopy 0 'base ''PackageTags)
$(deriveSafeCopy 0 'base ''TagAlias)

instance NFData PackageTags where
    rnf (PackageTags a b c) = rnf a `seq` rnf b `seq` rnf c

instance MemSize PackageTags where
    memSize (PackageTags a b c) = memSize3 a b c

initialPackageTags :: PackageTags
initialPackageTags = emptyPackageTags

tagsForPackage :: PackageName -> Query PackageTags (Set Tag)
tagsForPackage name = asks $ Map.findWithDefault Set.empty name . packageTags

packagesForTag :: Tag -> Query PackageTags (Set PackageName)
packagesForTag tag = asks $ Map.findWithDefault Set.empty tag . tagPackages

getTagList :: Query PackageTags [(Tag, Set PackageName)]
getTagList = asks $ Map.toList . tagPackages

getPackageTags :: Query PackageTags PackageTags
getPackageTags = ask

replacePackageTags :: PackageTags -> Update PackageTags ()
replacePackageTags = put

getTagAliasesState :: Query TagAlias TagAlias
getTagAliasesState = ask

addTagAliasesState :: TagAlias -> Update TagAlias ()
addTagAliasesState = put


setPackageTags :: PackageName -> Set Tag -> Update PackageTags ()
setPackageTags name tagList = modify $ setTags name tagList

setTagPackages :: Tag -> Set PackageName -> Update PackageTags ()
setTagPackages tag pkgList = modify $ setTag tag pkgList


-- | Tag a package. Returns True if the element was inserted, and False if
-- the tag as already present (same result though)
addPackageTag :: PackageName -> Tag -> Update PackageTags Bool
addPackageTag name tag = do
    pkgTags <- get
    case addTag name tag pkgTags of
        Nothing -> return False
        Just pkgTags' -> put pkgTags' >> return True

-- | Untag a package. Return True if the element was removed, and False if
-- it wasn't there in the first place (again, same outcome)
removePackageTag :: PackageName -> Tag -> Update PackageTags Bool
removePackageTag name tag = do
    pkgTags <- get
    case removeTag name tag pkgTags of
        Nothing -> return False
        Just pkgTags' -> put pkgTags' >> return True

clearReviewTags :: PackageName -> Update PackageTags ()
clearReviewTags pkgname = do
        PackageTags p t r <- get
        put (PackageTags p t (Map.insert pkgname (Set.empty,Set.empty) r))

insertReviewTags :: PackageName -> Set Tag -> Set Tag -> Update PackageTags ()
insertReviewTags pkgname add del = do
        PackageTags p t r <- get
        put (PackageTags p t (Map.insertWith insertReviewHelper pkgname (add,del) r))

insertReviewTags' :: PackageName -> Set Tag -> Set Tag -> Update PackageTags ()
insertReviewTags' pkgname add del = do
        PackageTags p t r <- get
        put (PackageTags p t (Map.insert pkgname (add,del) r))


insertReviewHelper :: (Set Tag, Set Tag) -> (Set Tag, Set Tag) -> (Set Tag, Set Tag)
insertReviewHelper (a,b) (c,d) = (Set.union a c, Set.union b d)

lookupReviewTags :: PackageName -> Query PackageTags (Set Tag, Set Tag)
lookupReviewTags pkgname = asks $ Map.findWithDefault (Set.empty, Set.empty) pkgname . reviewTags


$(makeAcidic ''TagAlias ['addTagAlias
                        ,'getTagAlias
                        ,'lookupTagAlias
                        ,'addTagAliasesState
                        ,'getTagAliasesState
                      ])

$(makeAcidic ''PackageTags ['tagsForPackage
                         ,'packagesForTag
                         ,'getTagList
                         ,'getPackageTags
                         ,'replacePackageTags
                         ,'setPackageTags
                         ,'setTagPackages
                         ,'addPackageTag
                         ,'removePackageTag
                         ,'insertReviewTags
                         ,'insertReviewTags'
                         ,'lookupReviewTags
                         ,'clearReviewTags
                         ])


