module Distribution.Server.Features.Tags (
    TagsFeature,
    tagsResource,
    TagsResource(..),
    setCalculatedTag,
    tagsUpdated,
    initTagsFeature,

    withTagPath,
    collectTags,
    putTags,
    constructTagIndex
  ) where

import Control.Applicative (optional)

import Distribution.Server.Acid
import Distribution.Server.Framework
import Distribution.Server.Features.Core
import Distribution.Server.Features.Packages (categorySplit)
import Distribution.Server.Packages.Tag

import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import Distribution.Server.Packages.State (GetPackagesState(..), packageList)
import Distribution.Server.Packages.Types
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Packages.Backup.Tags
import qualified Distribution.Server.Framework.Cache as Cache

import Distribution.Text
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.License

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function (fix)
import Data.List (foldl')
import Data.Char (toLower)
import Control.Monad (mzero, forM_, liftM)
import Control.Monad.Trans (MonadIO)


data TagsFeature = TagsFeature {
    tagsResource :: TagsResource,
    -- All package names that were modified, and all tags that were modified
    -- In almost all cases, one of these will be a singleton. Happstack
    -- functions should be used to query the resultant state.
    tagsUpdated :: Hook (Set PackageName -> Set Tag -> IO ()),
    -- Calculated tags are used so that other features can reserve a
    -- tag for their own use (a calculated, rather than freely
    -- assignable, tag). It is a subset of the main mapping.
    --
    -- This feature itself defines a few such tags: libary, executable,
    -- and license tags, as well as package categories on
    -- initial import.
    calculatedTags :: Cache.Cache PackageTags,
    setCalculatedTag :: Tag -> Set PackageName -> IO ()
}

data TagsResource = TagsResource {
    tagsListing :: Resource,
    tagListing :: Resource,
    packageTagsListing :: Resource,
    packageTagsEdit :: Resource,

    tagUri :: String -> Tag -> String,
    tagsUri :: String -> String,
    packageTagsUri :: String -> PackageName -> String
}

instance IsHackageFeature TagsFeature where
    getFeatureInterface tags = (emptyHackageFeature "tags") {
        featureResources = map ($tagsResource tags) [tagsListing, tagListing, packageTagsListing]
      , featurePostInit = initImmutableTags
      , featureDumpRestore = Just (dumpBackup, restoreBackup, testRoundtripByQuery (query GetPackageTags))
      }
      where
        initImmutableTags = do
                index <- fmap packageList $ query GetPackagesState
                let calcTags = tagPackages $ constructImmutableTagIndex index
                forM_ (Map.toList calcTags) $ uncurry $ setCalculatedTag tags
        dumpBackup    = do
            pkgTags <- query GetPackageTags
            return [csvToBackup ["tags.csv"] $ tagsToCSV pkgTags]
        restoreBackup = tagsBackup


initTagsFeature :: ServerEnv -> CoreFeature -> IO TagsFeature
initTagsFeature _ cf = do
    specials <- Cache.newCacheable emptyPackageTags
    updateTag <- newHook
    registerHook (packageAddHook cf) $ \pkginfo -> do 
      let pkgname = packageName . packageDescription . pkgDesc $ pkginfo
          tags = Set.fromList . constructImmutableTags . pkgDesc $ pkginfo
      update . SetPackageTags pkgname $ tags
    return TagsFeature 
            { tagsResource = fix $ \r -> TagsResource
              { tagsListing = resourceAt "/packages/tags/.:format"
              , tagListing = resourceAt "/packages/tag/:tag.:format"
              , packageTagsListing = resourceAt "/package/:package/tags.:format"
              , packageTagsEdit    = resourceAt "/package/:package/tags/edit"
              , tagUri = \format tag -> renderResource (tagListing r) [display tag, format]
              , tagsUri = \format -> renderResource (tagsListing r) [format]
              , packageTagsUri = \format pkgname -> renderResource (packageTagsListing r) [display pkgname, format]
            -- for more fine-tuned tag manipulation, could also define:
            -- * DELETE /package/:package/tag/:tag (remove single tag)
            -- * POST /package/:package\/tags (add single tag)
            -- renaming tags and deleting them are also supported as happstack-state
            -- operations, but make sure this wouldn't circumvent calculated tags.
              }
            , tagsUpdated = updateTag
            , calculatedTags = specials
            , setCalculatedTag = \tag pkgs -> do
              Cache.modifyCache specials (setTag tag pkgs)
              update $ SetTagPackages tag pkgs
              runHook'' updateTag pkgs (Set.singleton tag)
            }

withTagPath :: DynamicPath -> (Tag -> Set PackageName -> ServerPart a) -> ServerPart a
withTagPath dpath func = case simpleParse =<< lookup "tag" dpath of
    Nothing -> mzero
    Just tag -> do
        pkgs <- query $ PackagesForTag tag
        func tag pkgs

collectTags :: MonadIO m => Set PackageName -> m (Map PackageName (Set Tag))
collectTags pkgs = do
    pkgMap <- liftM packageTags $ query GetPackageTags
    return $ Map.fromDistinctAscList . map (\pkg -> (pkg, Map.findWithDefault Set.empty pkg pkgMap)) $ Set.toList pkgs

putTags :: TagsFeature -> PackageName -> ServerPartE ()
putTags tagf pkgname = withPackageAll pkgname $ \_ -> do
    -- let anyone edit tags for the moment. otherwise, we can do:
    -- users <- query GetUserDb; withHackageAuth users Nothing $ \_ _ -> do
    mtags <- optional $ look "tags"
    case simpleParse =<< mtags of
        Just (TagList tags) -> do
            calcTags <- fmap (packageToTags pkgname) $ Cache.getCache $ calculatedTags tagf
            let tagSet = Set.fromList tags `Set.union` calcTags
            update $ SetPackageTags pkgname tagSet
            runHook'' (tagsUpdated tagf) (Set.singleton pkgname) tagSet
            return ()
        Nothing -> errBadRequest "Tags not recognized" [MText "Couldn't parse your tag list. It should be comma separated with any number of alphanumerical tags. Tags can also also have -+#*."]

-- initial tags, on import
constructTagIndex :: PackageIndex PkgInfo -> PackageTags
constructTagIndex = foldl' addToTags emptyPackageTags . PackageIndex.allPackagesByName
  where addToTags pkgTags pkgList =
            let info = pkgDesc $ last pkgList
                pkgname = packageName info
                categoryTags = Set.fromList . constructCategoryTags . packageDescription $ info
                immutableTags = Set.fromList . constructImmutableTags $ info
            in setTags pkgname (Set.union categoryTags immutableTags) pkgTags

-- tags on startup
constructImmutableTagIndex :: PackageIndex PkgInfo -> PackageTags
constructImmutableTagIndex = foldl' addToTags emptyPackageTags . PackageIndex.allPackagesByName
  where addToTags calcTags pkgList =
            let info = pkgDesc $ last pkgList
            in setTags (packageName info) (Set.fromList $ constructImmutableTags info) calcTags

-- These are constructed when a package is uploaded/on startup
constructCategoryTags :: PackageDescription -> [Tag]
constructCategoryTags = map (tagify . map toLower) . fillMe . categorySplit . category
  where
    fillMe [] = ["unclassified"]
    fillMe xs = xs

-- These are reassigned as immutable tags
constructImmutableTags :: GenericPackageDescription -> [Tag]
constructImmutableTags genDesc =
    let desc = flattenPackageDescription genDesc
    in licenseToTag (license desc)
    ++ (if hasLibs desc then [Tag "library"] else [])
    ++ (if hasExes desc then [Tag "program"] else [])
  where
    licenseToTag :: License -> [Tag]
    licenseToTag l = case l of
        GPL  _ -> [Tag "gpl"]
        LGPL _ -> [Tag "lgpl"]
        BSD3 -> [Tag "bsd3"]
        BSD4 -> [Tag "bsd4"]
        MIT  -> [Tag "mit"]
        PublicDomain -> [Tag "public-domain"]
        AllRightsReserved -> [Tag "all-rights-reserved"]
        _ -> []

