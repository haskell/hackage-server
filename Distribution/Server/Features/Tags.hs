{-# LANGUAGE BangPatterns, RankNTypes, NamedFieldPuns, RecordWildCards #-}

module Distribution.Server.Features.Tags (
    TagsFeature(..),
    TagsResource(..),
    initTagsFeature,

    Tag(..),
    constructTagIndex
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump

import Distribution.Server.Features.Tags.State
import Distribution.Server.Features.Tags.Backup
import Distribution.Server.Features.Core
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Users

import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.PackageIndex (PackageIndex)
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.Render (categorySplit)

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


data TagsFeature = TagsFeature {
    tagsFeatureInterface :: HackageFeature,

    tagsResource :: TagsResource,

    queryGetTagList     :: forall m. MonadIO m => m [(Tag, Set PackageName)],
    queryTagsForPackage :: forall m. MonadIO m => PackageName -> m (Set Tag),
    queryReviewTagsForPackage :: forall m. MonadIO m => PackageName -> m (Set Tag,Set Tag),
    queryAliasForTag :: forall m. MonadIO m => Tag -> m Tag,

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
    putTags     :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> PackageName -> ServerPartE (),
    mergeTags   :: Maybe String -> Tag -> ServerPartE ()

}

instance IsHackageFeature TagsFeature where
    getFeatureInterface = tagsFeatureInterface

data TagsResource = TagsResource {
    tagsListing :: Resource,
    tagListing :: Resource,
    packageTagsListing :: Resource,
    packageTagsEdit :: Resource,
    tagAliasEdit :: Resource,
    tagAliasEditForm :: Resource,

    tagUri :: String -> Tag -> String,
    tagsUri :: String -> String,
    packageTagsUri :: String -> PackageName -> String
}

initTagsFeature :: ServerEnv
                -> IO (CoreFeature
                    -> UploadFeature
                    -> UserFeature
                    -> IO TagsFeature)
initTagsFeature ServerEnv{serverStateDir} = do
    tagsState <- tagsStateComponent serverStateDir
    tagAlias <- tagsAliasComponent serverStateDir
    specials  <- newMemStateWHNF emptyPackageTags
    updateTag <- newHook

    return $ \core@CoreFeature{..} upload user -> do
      let feature = tagsFeature core upload user tagsState tagAlias specials updateTag

      registerHookJust packageChangeHook isPackageChangeAny $ \(pkgid, mpkginfo) ->
        case mpkginfo of
          Nothing      -> return ()
          Just pkginfo -> do
            let pkgname = packageName pkgid
                tags = constructImmutableTags . pkgDesc $ pkginfo
            aliases <- mapM (queryState tagAlias . GetTagAlias) tags
            let newtags = Set.fromList aliases
            updateState tagsState . SetPackageTags pkgname $ newtags
            runHook_ updateTag (Set.singleton pkgname, newtags)

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

tagsAliasComponent :: FilePath -> IO (StateComponent AcidState TagAlias)
tagsAliasComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Tags" </> "Alias") emptyTagAlias
  return StateComponent {
      stateDesc    = "Tags Alias"
    , stateHandle  = st
    , getState     = query st GetTagAliasesState
    , putState     = update st . AddTagAliasesState
    , backupState  = \_ aliases -> [csvToBackup ["aliases.csv"] $ aliasToCSV aliases]
    , restoreState = aliasBackup
    , resetState   = tagsAliasComponent
    }

tagsFeature :: CoreFeature
            -> UploadFeature
            -> UserFeature
            -> StateComponent AcidState PackageTags
            -> StateComponent AcidState TagAlias
            -> MemState PackageTags
            -> Hook (Set PackageName, Set Tag) ()
            -> TagsFeature

tagsFeature CoreFeature{ queryGetPackageIndex }
            UploadFeature{ maintainersGroup, trusteesGroup }
            UserFeature{ guardAuthorised' }
            tagsState
            tagsAlias
            calculatedTags
            tagsUpdated
  = TagsFeature{..}
  where
    tagsResource = fix $ \r -> TagsResource
        { tagsListing = resourceAt "/packages/tags/.:format"
        , tagListing = resourceAt "/packages/tag/:tag.:format"
        , tagAliasEdit = resourceAt "/packages/tag/:tag/alias"
        , tagAliasEditForm = resourceAt "/packages/tag/:tag/alias/edit"
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
            aliases <- mapM (queryState tagsAlias . GetTagAlias) $ Map.keys calcTags
            let calcTags' = Map.toList . Map.fromListWith Set.union $ zip aliases (Map.elems calcTags)
            forM_ calcTags' $ uncurry setCalculatedTag

    queryGetTagList :: MonadIO m => m [(Tag, Set PackageName)]
    queryGetTagList = queryState tagsState GetTagList

    queryTagsForPackage :: MonadIO m => PackageName -> m (Set Tag)
    queryTagsForPackage pkgname = queryState tagsState (TagsForPackage pkgname)

    queryAliasForTag :: MonadIO m => Tag -> m Tag
    queryAliasForTag tag = queryState tagsAlias (GetTagAlias tag)

    queryReviewTagsForPackage :: MonadIO m => PackageName -> m (Set Tag,Set Tag)
    queryReviewTagsForPackage pkgname = queryState tagsState (LookupReviewTags pkgname)

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

    mergeTags :: Maybe String -> Tag -> ServerPartE ()
    mergeTags targetTag deprTag =
        case simpleParse =<< targetTag of
            Just (Tag orig) -> do
                index <- queryGetPackageIndex
                void $ updateState tagsAlias $ AddTagAlias (Tag orig) deprTag
                void $ constructMergedTagIndex (Tag orig) deprTag index
            _ -> errBadRequest "Tag not recognised" [MText "Couldn't parse tag. It should be a single tag."]

    -- tags on merging
    constructMergedTagIndex :: forall m. (Functor m, MonadIO m) => Tag -> Tag -> PackageIndex PkgInfo -> m PackageTags
    constructMergedTagIndex orig depr = foldM addToTags emptyPackageTags . PackageIndex.allPackagesByName
      where addToTags calcTags pkgList = do
                let info = pkgDesc $ last pkgList
                    !pn = packageName info
                pkgTags <- queryTagsForPackage pn
                if Set.member depr pkgTags
                    then do
                        let newTags = Set.delete depr (Set.insert orig pkgTags)
                        void $ updateState tagsState $ SetPackageTags pn newTags
                        runHook_ tagsUpdated (Set.singleton pn, newTags)
                        return $ setTags pn newTags calcTags
                    else return $ setTags pn pkgTags calcTags

    putTags :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> PackageName -> ServerPartE ()
    putTags addns delns raddns rdelns pkgname =
      case simpleParse =<< addns of
          Just (TagList add) ->
                case simpleParse =<< delns of
                    Just (TagList del) -> do
                        trustainer <- guardAuthorised' [InGroup (maintainersGroup pkgname), InGroup trusteesGroup]
                        user <- guardAuthorised' [AnyKnownUser]
                        if trustainer
                            then do
                                calcTags <- queryTagsForPackage pkgname
                                aliases <- mapM (queryState tagsAlias . GetTagAlias) add
                                revTags <- queryReviewTagsForPackage pkgname
                                let tagSet = (addTags `Set.union` calcTags) `Set.difference` delTags
                                    addTags = Set.fromList aliases
                                    delTags = Set.fromList del
                                    rdel' = case simpleParse =<< rdelns of
                                        Just (TagList rdel) -> rdel
                                        Nothing -> []
                                    radd' = case simpleParse =<< raddns of
                                        Just (TagList radd) -> radd
                                        Nothing -> []
                                    addRev = Set.difference (fst revTags) (Set.fromList add `Set.union` Set.fromList radd')
                                    delRev = Set.difference (snd revTags) (Set.fromList del `Set.union` Set.fromList rdel')
                                void $ updateState tagsState $ SetPackageTags pkgname tagSet
                                void $ updateState tagsState $ InsertReviewTags' pkgname addRev delRev
                                runHook_ tagsUpdated (Set.singleton pkgname, tagSet)
                                return ()
                            else if user
                                then do
                                    aliases <- mapM (queryState tagsAlias . GetTagAlias) add
                                    calcTags <- queryTagsForPackage pkgname
                                    let addTags = Set.fromList aliases `Set.difference` calcTags
                                        delTags = Set.fromList del `Set.intersection` calcTags
                                    void $ updateState tagsState $ InsertReviewTags pkgname addTags delTags
                                    return ()
                                else errBadRequest "Authorization Error" [MText "You need to be logged in to propose tags"]
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
    ++ [Tag "library" | hl] -- (if hl then [Tag "library"] else [])
    ++ [Tag "program" | he]
    ++ [Tag "test" | ht]
    ++ [Tag "benchmark" | hb]
    ++ constructCategoryTags desc
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
