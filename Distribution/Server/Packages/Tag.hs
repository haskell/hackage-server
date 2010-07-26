{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}

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
import Data.List (insert, delete, foldl')
import Control.Monad.State (get, put)
import Control.Monad.Reader (ask, asks)

import Happstack.State

newtype TagList = TagList [Tag] deriving (Show, Typeable)
instance Text TagList where
    disp (TagList tags) = Disp.hsep . Disp.punctuate Disp.comma $ map disp tags
    parse = fmap TagList $ Parse.skipSpaces >> Parse.parseCommaList parse

newtype Tag = Tag String deriving (Show, Typeable, Ord, Eq)
instance Text Tag where
    disp (Tag tag) = Disp.text tag
    parse = do
        -- adding many1 here would allow multiword tags. dashes should suffice though, and they're more aesthetic in URIs.
        strs <- do
            t <- liftM2 (:) (Parse.satisfy Char.isAlphaNum)
               $ Parse.munch1 (\c -> Char.isAlphaNum c || c `elem` "-+#*.")
            Parse.skipSpaces
            return t
        return $ Tag strs

-- TODO: use Set
data PackageTags = PackageTags {
    -- the primary index
    packageTags :: Map PackageName [Tag],
    -- a secondary reverse mapping
    tagPackages :: Map Tag [PackageName]
} deriving (Show, Typeable)

emptyPackageTags :: PackageTags
emptyPackageTags = PackageTags Map.empty Map.empty

-------------------------------------------------------------------------------

instance Version Tag where mode = Versioned 0 Nothing
$(deriveSerialize ''Tag)
instance Version PackageTags where mode = Versioned 0 Nothing
$(deriveSerialize ''PackageTags)

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
setPackageTags name tagList = do
    PackageTags tags packages <- get
    let oldTags = Map.findWithDefault [] name tags
        packages' = foldl' adjustTags packages oldTags
    put $ PackageTags (Map.insert name tagList tags) packages'
  where
    adjustTags pkgMap oldTag = case oldTag `elem` tagList of
        True  -> pkgMap
        False -> deletePackageFromTag name oldTag pkgMap

-- | Tag a package. Returns True if the element was inserted, and False if
-- the tag as already present (same result though)
addPackageTag :: PackageName -> Tag -> Update PackageTags Bool
addPackageTag name tag = do
    PackageTags tags packages <- get
    let existing = Map.findWithDefault [] name tags
    case tag `elem` existing of
        True -> return False
        False -> do
            put $ PackageTags (Map.insert name (insert tag existing) tags)
                              (Map.adjust (\names -> insert name names) tag packages)
            return True

-- | Untag a package. Return True if the element was removed, and False if
-- it wasn't there in the first place (again, same outcome)
removePackageTag :: PackageName -> Tag -> Update PackageTags Bool
removePackageTag name tag = do
    PackageTags tags packages <- get
    let existing = Map.findWithDefault [] name tags
    case tag `elem` existing of
        True -> do
            let existing' = delete tag existing
            put $ PackageTags (Map.insert name existing' tags)
                              (deletePackageFromTag name tag packages)
            return True
        False -> return False

deletePackageFromTag :: PackageName -> Tag -> Map Tag [PackageName] -> Map Tag [PackageName]
deletePackageFromTag name tag packages = Map.update deleteUpdate tag packages
  where
    deleteUpdate pkgs = case delete name pkgs of
        []    -> Nothing
        pkgs' -> Just pkgs'

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

