{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.PreferredVersions (
    VersionsFeature(..),
    VersionsResource(..),
    initVersionsFeature,

    PreferredInfo(..),
    VersionStatus(..),
    classifyVersions,

    PreferredRender(..),
  ) where

import Distribution.Server.Framework
import qualified Distribution.Server.Framework.ResourceTypes as Resource

import Distribution.Server.Features.PreferredVersions.State

import Distribution.Server.Features.Core
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Tags

import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types

import Distribution.Package
import Distribution.Version
import Distribution.Text

import Data.Either   (rights)
import Data.Function (fix)
import Data.List (intercalate, find)
import Data.Maybe (isJust, fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Control.Arrow (second)
import Control.Applicative (optional)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.JSON
         ( JSValue(..), toJSObject, toJSString )

data VersionsFeature = VersionsFeature {
    versionsFeatureInterface :: HackageFeature,
    
    queryGetPreferredInfo :: MonadIO m => PackageName -> m PreferredInfo,
    queryGetDeprecatedFor :: MonadIO m => PackageName -> m (Maybe [PackageName]),

    versionsResource :: VersionsResource,
    preferredHook  :: Hook (PackageName -> PreferredInfo -> IO ()),
    deprecatedHook :: Hook (PackageName -> Maybe [PackageName] -> IO ()),
    putDeprecated :: PackageName -> ServerPartE Bool,
    putPreferred  :: PackageName -> ServerPartE (),
    updateDeprecatedTags :: IO (),

    doPreferredRender     :: PackageName -> ServerPartE PreferredRender,
    doDeprecatedRender    :: PackageName -> ServerPartE (Maybe [PackageName]),
    doPreferredsRender    :: MonadIO m => m [(PackageName, PreferredRender)],
    doDeprecatedsRender   :: MonadIO m => m [(PackageName, [PackageName])],
    makePreferredVersions :: MonadIO m => m String,
    withPackagePreferred     :: forall a. PackageId -> (PkgInfo -> [PkgInfo] -> ServerPartE a) -> ServerPartE a,
    withPackagePreferredPath :: forall a. DynamicPath -> (PkgInfo -> [PkgInfo] -> ServerPartE a) -> ServerPartE a
}

instance IsHackageFeature VersionsFeature where
    getFeatureInterface = versionsFeatureInterface


data VersionsResource = VersionsResource {
    preferredResource :: Resource,
    preferredText :: Resource,
    preferredPackageResource :: Resource,
    deprecatedResource :: Resource,
    deprecatedPackageResource :: Resource,

    preferredUri :: String -> String,
    preferredPackageUri :: String -> PackageName -> String,
    deprecatedUri :: String -> String,
    deprecatedPackageUri :: String -> PackageName -> String
}

data PreferredRender = PreferredRender {
    rendSumRange :: String,
    rendRanges   :: [String],
    rendVersions :: [Version]
} deriving (Show, Eq)


initVersionsFeature :: ServerEnv -> CoreFeature -> UploadFeature -> TagsFeature -> IO VersionsFeature
initVersionsFeature ServerEnv{serverStateDir} core upload tags = do

    -- Canonical state
    preferredState  <- openLocalStateFrom
                         (serverStateDir </> "db" </> "PreferredVersions")
                         initialPreferredVersions

    preferredHook  <- newHook
    deprecatedHook <- newHook
    
    return $
      versionsFeature core upload tags
                      preferredState
                      preferredHook deprecatedHook
      

versionsFeature :: CoreFeature
                -> UploadFeature
                -> TagsFeature
                -> AcidState PreferredVersions
                -> Hook (PackageName -> PreferredInfo -> IO ())
                -> Hook (PackageName -> Maybe [PackageName] -> IO ())
                -> VersionsFeature

versionsFeature CoreFeature{..} UploadFeature{..} TagsFeature{..}
                preferredState preferredHook deprecatedHook
  = VersionsFeature{..}
  where
    versionsFeatureInterface = (emptyHackageFeature "versions") {
        featureResources = map ($versionsResource)
            [preferredResource, preferredPackageResource,
             deprecatedResource, deprecatedPackageResource,
             preferredText]
        --FIXME: don't we need to set the preferred-versions on init?
      , featurePostInit = updateDeprecatedTags
      , featureCheckpoint = do
          createCheckpoint preferredState
      , featureShutdown = do
          closeAcidState preferredState
        --FIXME: no backup!?!
      , featureDumpRestore = Nothing

      }

    queryGetPreferredInfo :: MonadIO m => PackageName -> m PreferredInfo
    queryGetPreferredInfo name = query' preferredState (GetPreferredInfo name)

    queryGetDeprecatedFor :: MonadIO m => PackageName -> m (Maybe [PackageName])
    queryGetDeprecatedFor name = query' preferredState (GetDeprecatedFor name)

    updateDeprecatedTags = do
      pkgs <- fmap (Map.keys . deprecatedMap) $ query preferredState GetPreferredVersions
      setCalculatedTag (Tag "deprecated") (Set.fromDistinctAscList pkgs)

    versionsResource = fix $ \r -> VersionsResource
          { preferredResource = resourceAt "/packages/preferred.:format"
          , preferredText = (resourceAt "/packages/preferred-versions") { resourceGet = [("txt", \_ -> textPreferred)] }
          , preferredPackageResource = resourceAt "/package/:package/preferred.:format"
          , deprecatedResource = (resourceAt "/packages/deprecated.:format") {
                                   resourceGet = [("json", handlePackagesDeprecatedGet)]
                                 }
          , deprecatedPackageResource = (resourceAt "/package/:package/deprecated.:format") {
                                          resourceGet = [("json", handlePackageDeprecatedGet) ]
                                        }

          , preferredUri = \format -> renderResource (preferredResource r) [format]
          , preferredPackageUri = \format pkgid -> renderResource (preferredPackageResource r) [display pkgid, format]
          , deprecatedUri = \format -> renderResource (deprecatedResource r) [format]
          , deprecatedPackageUri = \format pkgid -> renderResource (deprecatedPackageResource r) [display pkgid, format]
          }

    textPreferred = fmap toResponse makePreferredVersions

    handlePackagesDeprecatedGet _ = do
      deprPkgs <- deprecatedMap <$> query' preferredState GetPreferredVersions
      return $ toResponse $ Resource.JSON $
        JSArray
          [ JSObject $ toJSObject
              [ ("deprecated-package", JSString $ toJSString $ display deprPkg)
              , ("in-favour-of", JSArray [ JSString $ toJSString $ display pkg
                                         | pkg <- replacementPkgs ])
              ]
          | (deprPkg, replacementPkgs) <- Map.toList deprPkgs ]

    handlePackageDeprecatedGet dpath =
      runServerPartE $
      withPackageAllPath dpath $ \pkgname _ -> do
        mdep <- query' preferredState (GetDeprecatedFor pkgname)
        return $ toResponse $ Resource.JSON $
          JSObject $ toJSObject
              [ ("is-deprecated", JSBool (isJust mdep))
              , ("in-favour-of", JSArray [ JSString $ toJSString $ display pkg
                                         | pkg <- fromMaybe [] mdep ])
              ]

    ---------------------------
    -- This is a function used by the HTML feature to select the version to display.
    -- It could be enhanced by displaying a search page in the case of failure,
    -- which is outside of the scope of this feature.
    withPackagePreferred :: PackageId -> (PkgInfo -> [PkgInfo] -> ServerPartE a) -> ServerPartE a
    withPackagePreferred pkgid func = do
      pkgIndex <- queryGetPackageIndex
      case PackageIndex.lookupPackageName pkgIndex (packageName pkgid) of
            []   ->  packageError [MText "No such package in package index"]
            pkgs  | pkgVersion pkgid == Version [] [] -> query' preferredState (GetPreferredInfo $ packageName pkgid) >>= \info -> do
                let rangeToCheck = sumRange info
                case maybe id (\r -> filter (flip withinRange r . packageVersion)) rangeToCheck pkgs of
                    -- no preferred version available, choose latest from list ordered by version
                    []    -> func (last pkgs) pkgs
                    -- return latest preferred version
                    pkgs' -> func (last pkgs') pkgs
            pkgs -> case find ((== packageVersion pkgid) . packageVersion) pkgs of
                Nothing  -> packageError [MText $ "No such package version for " ++ display (packageName pkgid)]
                Just pkg -> func pkg pkgs
      where packageError = errNotFound "Package not found"

    withPackagePreferredPath :: DynamicPath -> (PkgInfo -> [PkgInfo] -> ServerPartE a) -> ServerPartE a
    withPackagePreferredPath dpath func =
      withPackageId dpath $ \pkgid ->
        withPackagePreferred pkgid func

    putPreferred :: PackageName -> ServerPartE ()
    putPreferred pkgname =
            withPackageAll pkgname $ \pkgs ->
            withPackageNameAuth pkgname $ \_ _ -> do
        pref <- optional $ fmap lines $ look "preferred"
        depr <- optional $ fmap (rights . map snd . filter ((=="deprecated") . fst)) $ lookPairs
        case sequence . map simpleParse =<< pref of
            Just prefs -> case sequence . map simpleParse =<< depr of
                Just deprs -> case all (`elem` map packageVersion pkgs) deprs of
                    True  -> do
                        void $ update' preferredState $ SetPreferredRanges pkgname prefs
                        void $ update' preferredState $ SetDeprecatedVersions pkgname deprs
                        newInfo <- query' preferredState $ GetPreferredInfo pkgname
                        prefVersions <- makePreferredVersions
                        now <- liftIO getCurrentTime
                        updateArchiveIndexEntry "preferred-versions" (BS.pack prefVersions, now)
                        runHook'' preferredHook pkgname newInfo
                        runHook packageIndexChange
                        return ()
                    False -> preferredError "Not all of the selected versions are in the main index."
                Nothing -> preferredError "Version could not be parsed."
            Nothing -> preferredError "Expected format of the preferred ranges field is one version range per line, e.g. '<2.3 || 3.*' (see Cabal documentation for the syntax)."
      where
        preferredError = errBadRequest "Preferred ranges failed" . return . MText

    putDeprecated :: PackageName -> ServerPartE Bool
    putDeprecated pkgname =
            withPackageAll pkgname $ \_ ->
            withPackageNameAuth pkgname $ \_ _ -> do
        index  <- queryGetPackageIndex
        isDepr <- optional $ look "deprecated"
        case isDepr of
            Just {} -> do
                depr <- optional $ fmap words $ look "by"
                case sequence . map simpleParse =<< depr of
                    Just deprs -> case filter (null . PackageIndex.lookupPackageName index) deprs of
                        [] -> case any (== pkgname) deprs of
                                True -> deprecatedError $ "You can not deprecate a package in favor of itself!"
                                _ -> do
                                  doUpdates (Just deprs)
                                  return True
                        pkgs -> deprecatedError $ "Some superseding packages aren't in the main index: " ++ intercalate ", " (map display pkgs)
                    Nothing -> deprecatedError "Expected format of the 'superseded by' field is a list of package names separated by spaces."
            Nothing -> do
                doUpdates Nothing
                return False
      where
        deprecatedError = errBadRequest "Deprecation failed" . return . MText
        doUpdates deprs = do
            void $ update' preferredState $ SetDeprecatedFor pkgname deprs
            runHook'' deprecatedHook pkgname deprs
            liftIO $ updateDeprecatedTags

    renderPrefInfo :: PreferredInfo -> PreferredRender
    renderPrefInfo pref = PreferredRender {
        rendSumRange = maybe "-any" display $ sumRange pref,
        rendRanges = map display $ preferredRanges pref,
        rendVersions = deprecatedVersions pref
    }

    doPreferredRender :: PackageName -> ServerPartE PreferredRender
    doPreferredRender pkgname = withPackageAll pkgname $ \_ -> do
        pref <- query' preferredState $ GetPreferredInfo pkgname
        return $ renderPrefInfo pref

    doDeprecatedRender :: PackageName -> ServerPartE (Maybe [PackageName])
    doDeprecatedRender pkgname = withPackageAll pkgname $ \_ -> query' preferredState $ GetDeprecatedFor pkgname

    doPreferredsRender :: MonadIO m => m [(PackageName, PreferredRender)]
    doPreferredsRender = query' preferredState GetPreferredVersions >>=
        return . map (second renderPrefInfo) . Map.toList . preferredMap

    doDeprecatedsRender :: MonadIO m => m [(PackageName, [PackageName])]
    doDeprecatedsRender = query' preferredState GetPreferredVersions >>=
        return . Map.toList . deprecatedMap

    makePreferredVersions :: MonadIO m => m String
    makePreferredVersions = query' preferredState GetPreferredVersions >>= \(PreferredVersions prefs _) -> do
        return . unlines . (topText++) . map (display . uncurry Dependency) . Map.toList $ Map.mapMaybe sumRange prefs
    -- note: setting noVersion is kind of useless..
    -- $ unionWith const (Map.mapMaybe sumRange deprs) (Map.map (const noVersion) prefs)
      where
        -- hard coded..
        topText =
          [ "-- A global set of preferred versions."
          , "--"
          , "-- This is to indicate a current recommended version, to allow stable and"
          , "-- experimental versions to co-exist on hackage and to help transitions"
          , "-- between major API versions."
          , "--"
          , "-- Tools like cabal-install take these preferences into account when"
          , "-- constructing install plans."
          , "--"
          ]

