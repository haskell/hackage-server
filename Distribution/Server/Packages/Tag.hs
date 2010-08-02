{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}

module Distribution.Server.Packages.Tag where

import Distribution.Server.Instances ()

import qualified Distribution.ParseUtils   as Parse
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Text
import Distribution.Package
import qualified Text.PrettyPrint as Disp

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (liftM2)
import Data.Typeable (Typeable)
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import Data.List (insert, delete, union, nub, foldl')
import Control.Monad.State (get, put, modify)
import Control.Monad.Reader (ask, asks)
import Control.Parallel.Strategies

import Happstack.State

newtype TagList = TagList [Tag] deriving (Show, Typeable)
instance Text TagList where
    disp (TagList tags) = Disp.hsep . Disp.punctuate Disp.comma $ map disp tags
    parse = fmap TagList $ Parse.skipSpaces >> Parse.parseCommaList parse

newtype Tag = Tag String deriving (Show, Typeable, Ord, Eq, NFData)
instance Text Tag where
    disp (Tag tag) = Disp.text tag
    parse = do
        -- adding many1 here would allow multiword tags. dashes should suffice though, and they're more aesthetic in URIs.
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

-- a forcefully forgiving version of the parser
tagify :: String -> Tag
tagify (x:xs) = Tag $ (if tagInitialChar x then (x:) else id) $ tagify' xs
  where tagify' (c:cs) | tagLaterChar c = c:tagify' cs
        tagify' (c:cs) | c `elem` " /\\" = '-':tagify' cs -- dash is the preferred separation tag?
        tagify' (_:cs) = tagify' cs
        tagify' [] = []
tagify [] = Tag ""

-- TODO: use Set
data PackageTags = PackageTags {
    -- the primary index
    packageTags :: Map PackageName [Tag],
    -- a secondary reverse mapping
    tagPackages :: Map Tag [PackageName]
} deriving (Show, Typeable)

emptyPackageTags :: PackageTags
emptyPackageTags = PackageTags Map.empty Map.empty

setTags :: PackageName -> [Tag] -> PackageTags -> PackageTags
setTags name tagList (PackageTags tags packages) =
    let oldTags  = Map.findWithDefault [] name tags
        allTagList = union (nub tagList) oldTags
        packages'  = foldl' (adjustTags oldTags) packages allTagList
    in PackageTags (Map.insert name tagList tags) packages'
  where
    adjustTags :: [Tag] -> Map Tag [PackageName] -> Tag -> Map Tag [PackageName]
    adjustTags oldTags pkgMap tagExamine = case tagExamine `elem` tagList of
        True -> case tagExamine `elem` oldTags of
            True  -> pkgMap
            False -> Map.alter (Just . insert name . fromMaybe []) tagExamine pkgMap
        False -> deletePackageFromTag name tagExamine pkgMap

addTag :: PackageName -> Tag -> PackageTags -> Maybe PackageTags
addTag name tag (PackageTags tags packages) =
    let existing = Map.findWithDefault [] name tags
    in case tag `elem` existing of
        True -> Nothing
        False -> Just $ PackageTags (Map.insert name (insert tag existing) tags)
                         (Map.adjust (insert name) tag packages)

removeTag :: PackageName -> Tag -> PackageTags -> Maybe PackageTags
removeTag name tag (PackageTags tags packages) =
    let existing = Map.findWithDefault [] name tags
    in case tag `elem` existing of
        True ->
            let existing' = delete tag existing
            in Just $  PackageTags (Map.insert name existing' tags)
                                   (deletePackageFromTag name tag packages)
        False -> Nothing

{-
setTag :: Tag -> [PackageName] -> PackageTags -> PackageTags
setTag tag pkgList (PackageTags tags packages) =
    let oldPkgs = Map.findWithDefault [] tag packages
        allPackageList = union (nub pkgList) oldPkgs
        tags' = foldl' (adjustPkgs oldPkgs) tags allPackageList
    in PackageTags tags' (Map.insert tag pkgList packages)
  where
    adjustPkgs oldPkgs tagMap pkg = case pkg `elem` pkgList of
        ...
-}

-- Deletes a (tag, package) mapping from the (tag -> package) map, deleting the
-- tag if no other package names have it.
deletePackageFromTag :: PackageName -> Tag -> Map Tag [PackageName] -> Map Tag [PackageName]
deletePackageFromTag name tag packages = Map.update deleteUpdate tag packages
  where
    deleteUpdate pkgs = case delete name pkgs of
        []    -> Nothing
        pkgs' -> Just pkgs'

-------------------------------------------------------------------------------

instance Version Tag where mode = Versioned 0 Nothing
$(deriveSerialize ''Tag)
instance Version PackageTags where mode = Versioned 0 Nothing
$(deriveSerialize ''PackageTags)

instance NFData PackageTags where
    rnf (PackageTags a b) = rnf a `seq` rnf b

instance Component PackageTags where
    type Dependencies PackageTags = End
    initialValue = emptyPackageTags

tagsForPackage :: PackageName -> Query PackageTags [Tag]
tagsForPackage name = asks $ Map.findWithDefault [] name . packageTags

packagesForTag :: Tag -> Query PackageTags [PackageName]
packagesForTag tag = asks $ Map.findWithDefault [] tag . tagPackages

getTagList :: Query PackageTags [(Tag, [PackageName])]
getTagList = asks $ Map.toList . tagPackages

getPackageTags :: Query PackageTags PackageTags
getPackageTags = ask

replacePackageTags :: PackageTags -> Update PackageTags ()
replacePackageTags = put

setPackageTags :: PackageName -> [Tag] -> Update PackageTags ()
setPackageTags name tagList = modify $ setTags name tagList

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

-- May also be useful, but more complicated (result = packages affected):
-- renameTag :: Tag -> Tag -> Update PackageTags [PackageName]
-- deleteTag :: Tag -> Update PackageTags [PackageName]

$(mkMethods ''PackageTags ['tagsForPackage
                          ,'packagesForTag
                          ,'getTagList
                          ,'getPackageTags
                          ,'replacePackageTags
                          ,'setPackageTags
                          ,'addPackageTag
                          ,'removePackageTag
                          ])

