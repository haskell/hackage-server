{-# LANGUAGE BangPatterns, RankNTypes, NamedFieldPuns, RecordWildCards #-}

module Distribution.Server.Features.Tags (
    TagsFeature(..),
    TagsResource(..),
    initTagsFeature,

    Tag(..),
    constructTagIndex
  ) where

import Control.Applicative (optional)

import Distribution.Server.Framework
import Distribution.Server.Framework.Auth
import Distribution.Server.Framework.BackupDump

import Distribution.Server.Features.Tags.State
import Distribution.Server.Features.Tags.Backup
import Distribution.Server.Features.Users( guardAuthorised' )
import Distribution.Server.Features.Core
import Distribution.Server.Features.Upload

import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Render (categorySplit)

import Distribution.Text
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.License

import Data.Maybe(fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function (fix)
import Data.List (foldl')
import Data.Char (toLower)


data TagsFeature = TagsFeature {
    tagsFeatureInterface :: HackageFeature,

    tagsResource :: TagsResource,

    queryGetTagList     :: forall m. MonadIO m => m [(Tag, Set PackageName)],
    queryTagsForPackage :: forall m. MonadIO m => PackageName -> m (Set Tag),
    queryReviewTagsForPackage :: forall m. MonadIO m => PackageName -> m (Maybe (Set Tag,Set Tag)),

    -- All package names that were modified, and all tags that were modified
    -- In almost all cases, one of these will be a singleton. Happstack
    -- functions should be used to query the resultant state.
    tagsUpdated :: Hook (Set PackageName, Set Tag) (),

    -- Calculated tags are used so that other features can reserve a
    -- tag for their own use (a calculated, rather than freely
    -- assignable, tag). It is a subset of the main mapping.
    --
    -- This feature itself defines a few such tags: libary, executable,
    -- and license tags, as well as package categories on
    -- initial import.
    setCalculatedTag :: Tag -> Set PackageName -> IO (),

    withTagPath :: forall a. DynamicPath -> (Tag -> Set PackageName -> ServerPartE a) -> ServerPartE a,
    collectTags :: forall m. MonadIO m => Set PackageName -> m (Map PackageName (Set Tag)),

    putTags     :: PackageName -> ServerPartE ()

}

instance IsHackageFeature TagsFeature where
    getFeatureInterface = tagsFeatureInterface

data TagsResource = TagsResource {
    tagsListing :: Resource,
    tagListing :: Resource,
    packageTagsListing :: Resource,
    packageTagsEdit :: Resource,

    tagUri :: String -> Tag -> String,
    tagsUri :: String -> String,
    packageTagsUri :: String -> PackageName -> String
}

initTagsFeature :: ServerEnv
                -> IO (CoreFeature
                    -> UploadFeature
                    -> IO TagsFeature)
initTagsFeature ServerEnv{serverStateDir} = do
    tagsState <- tagsStateComponent serverStateDir
    tagsReview <- tagsReviewComponent serverStateDir
    specials  <- newMemStateWHNF emptyPackageTags
    updateTag <- newHook

    return $ \core@CoreFeature{..} upload -> do
      let feature = tagsFeature core upload tagsState tagsReview specials updateTag

      registerHookJust packageChangeHook isPackageChangeAny $ \(pkgid, mpkginfo) ->
        case mpkginfo of
          Nothing      -> return ()
          Just pkginfo -> do
            let pkgname = packageName pkgid
                tags = Set.fromList . constructImmutableTags . pkgDesc $ pkginfo
            updateState tagsState . SetPackageTags pkgname $ tags
            runHook_ updateTag (Set.singleton pkgname, tags)

      return feature

tagsStateComponent :: FilePath -> IO (StateComponent AcidState PackageTags)
tagsStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Tags" </> "Existing") initialPackageTags
  return StateComponent {
      stateDesc    = "Package tags"
    , stateHandle  = st
    , getState     = query st GetPackageTags
    , putState     = update st . ReplacePackageTags
    , backupState  = \_ pkgTags -> [csvToBackup ["tags.csv"] $ tagsToCSV pkgTags]
    , restoreState = tagsBackup
    , resetState   = tagsStateComponent
    }

tagsReviewComponent :: FilePath -> IO (StateComponent AcidState ReviewTags)
tagsReviewComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Tags" </> "Review") emptyReviewTags
  return StateComponent {
      stateDesc    = "Review tags"
    , stateHandle  = st
    , getState     = query st GetReviewTags
    , putState     = update st . ReplaceReviewTags
    -- , backupState  = \_ pkgTags -> [csvToBackup ["tags.csv"] $ tagsToCSV pkgTags]
    -- , restoreState = tagsBackup
    -- , resetState   = tagsStateComponent
    }



tagsFeature :: CoreFeature
            -> UploadFeature
            -> StateComponent AcidState PackageTags
            -> StateComponent AcidState ReviewTags
            -> MemState PackageTags
            -> Hook (Set PackageName, Set Tag) ()
            -> TagsFeature

tagsFeature CoreFeature{ queryGetPackageIndex
                       , coreResource = CoreResource { guardValidPackageName }
                       }
            UploadFeature{ guardAuthorisedAsUploaderOrMaintainerOrTrustee }
            tagsState
            tagsReview
            calculatedTags
            tagsUpdated
  = TagsFeature{..}
  where
    tagsResource = fix $ \r -> TagsResource
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

    tagsFeatureInterface = (emptyHackageFeature "tags") {
        featureResources =
          map ($ tagsResource) [
              tagsListing
            , tagListing
            , packageTagsListing
            ]
      , featurePostInit = initImmutableTags
      , featureState    = [abstractAcidStateComponent tagsState]
      , featureCaches   = [
            CacheComponent {
              cacheDesc       = "calculated tags",
              getCacheMemSize = memSize <$> readMemState calculatedTags
            }
          ]
      }

    initImmutableTags :: IO ()
    initImmutableTags = do
            index <- queryGetPackageIndex
            let calcTags = tagPackages $ constructImmutableTagIndex index
            forM_ (Map.toList calcTags) $ uncurry setCalculatedTag

    queryGetTagList :: MonadIO m => m [(Tag, Set PackageName)]
    queryGetTagList = queryState tagsState GetTagList

    queryTagsForPackage :: MonadIO m => PackageName -> m (Set Tag)
    queryTagsForPackage pkgname = queryState tagsState (TagsForPackage pkgname)

    queryReviewTagsForPackage :: MonadIO m => PackageName -> m (Maybe (Set Tag,Set Tag))
    queryReviewTagsForPackage pkgname = queryState tagsReview (LookupReviewTags pkgname)

    setCalculatedTag :: Tag -> Set PackageName -> IO ()
    setCalculatedTag tag pkgs = do
      modifyMemState calculatedTags (setTag tag pkgs)
      void $ updateState tagsState $ SetTagPackages tag pkgs
      runHook_ tagsUpdated (pkgs, Set.singleton tag)

    withTagPath :: DynamicPath -> (Tag -> Set PackageName -> ServerPartE a) -> ServerPartE a
    withTagPath dpath func = case simpleParse =<< lookup "tag" dpath of
        Nothing -> mzero
        Just tag -> do
            pkgs <- queryState tagsState $ PackagesForTag tag
            func tag pkgs

    collectTags :: MonadIO m => Set PackageName -> m (Map PackageName (Set Tag))
    collectTags pkgs = do
        pkgMap <- liftM packageTags $ queryState tagsState GetPackageTags
        return $ Map.fromDistinctAscList . map (\pkg -> (pkg, Map.findWithDefault Set.empty pkg pkgMap)) $ Set.toList pkgs

    putTags :: PackageName -> ServerPartE ()
    putTags pkgname = do
      guardValidPackageName pkgname
      addns <- optional $ look "addns"
      delns <- optional $ look "delns"
      raddns <- optional $ look "raddns"
      rdelns <- optional $ look "rdelns"
      case simpleParse =<< addns of
          Just (TagList add) -> do
                case simpleParse =<< delns of
                    Just (TagList del) -> do
                        user <- guardAuthorisedAsUploaderOrMaintainerOrTrustee pkgname
                        case user of
                            "Uploaders" -> do
                                calcTags <- queryTagsForPackage pkgname
                                let addTags = Set.fromList add `Set.difference` calcTags
                                    delTags = Set.fromList del `Set.intersection` calcTags
                                void $ updateState tagsReview $ InsertReviewTags pkgname addTags delTags
                                return ()
                            _ -> do
                                calcTags <- queryTagsForPackage pkgname
                                revTags <- queryReviewTagsForPackage pkgname
                                let tagSet = (addTags `Set.union` calcTags) `Set.difference` delTags
                                    addTags = Set.fromList add
                                    delTags = Set.fromList del
                                    rdel = case simpleParse =<< rdelns of
                                        Just (TagList rdel) -> rdel
                                        Nothing -> []
                                    radd = case simpleParse =<< raddns of
                                        Just (TagList radd) -> radd
                                        Nothing -> []
                                    addRev = Set.difference (fst $ fromMaybe (Set.empty, Set.empty) revTags) (Set.fromList add `Set.union` Set.fromList radd)
                                    delRev = Set.difference (snd $ fromMaybe (Set.empty, Set.empty) revTags) (Set.fromList del `Set.union` Set.fromList rdel)
                                void $ updateState tagsState $ SetPackageTags pkgname tagSet
                                void $ updateState tagsReview $ InsertReviewTags_ pkgname addRev delRev
                                runHook_ tagsUpdated (Set.singleton pkgname, tagSet)
                                return ()
                    _ -> errBadRequest "Tags not recognized" [MText "Couldn't parse your tag list. It should be comma separated with any number of alphanumerical tags. Tags can also also have -+#*."]
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
                !pn = packageName info
                !tags = constructImmutableTags info
            in setTags pn (Set.fromList tags) calcTags

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
        !l = license desc
        !hl = hasLibs desc
        !he = hasExes desc
        !ht = hasTests desc
        !hb = hasBenchmarks desc
    in licenseToTag l
    ++ (if hl then [Tag "library"] else [])
    ++ (if he then [Tag "program"] else [])
    ++ (if ht then [Tag "test"] else [])
    ++ (if hb then [Tag "benchmark"] else [])
    ++ (constructCategoryTags desc)
  where
    licenseToTag :: License -> [Tag]
    licenseToTag l = case l of
        GPL  _ -> [Tag "gpl"]
        AGPL _ -> [Tag "agpl"]
        LGPL _ -> [Tag "lgpl"]
        BSD2 -> [Tag "bsd2"]
        BSD3 -> [Tag "bsd3"]
        BSD4 -> [Tag "bsd4"]
        MIT  -> [Tag "mit"]
        MPL _ -> [Tag "mpl"]
        Apache _ -> [Tag "apache"]
        PublicDomain -> [Tag "public-domain"]
        AllRightsReserved -> [Tag "all-rights-reserved"]
        _ -> []
