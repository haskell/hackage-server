module Distribution.Server.Features.PreferredVersions (
    VersionsFeature(..),
    VersionsResource(..),
    initVersionsFeature,

    PreferredRender(..),
    doPreferredRender,
    doDeprecatedRender,
    doPreferredsRender,
    doDeprecatedsRender,
    makePreferredVersions,

    withPackagePreferred,
    withPackagePreferredPath
  ) where

import Distribution.Server.Feature
import Distribution.Server.Features.Core
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Tags
import Distribution.Server.Types
import Distribution.Server.Error
import Distribution.Server.Hook
import Distribution.Server.Resource
import qualified Distribution.Server.Cache as Cache

import qualified Distribution.Server.PackageIndex as PackageIndex
import Distribution.Server.Packages.Preferred
import Distribution.Server.Packages.Tag
import Distribution.Server.Packages.State
import Distribution.Server.Packages.Types

import Distribution.Package
import Distribution.Version
import Distribution.Text

import Data.Function (fix)
import Data.List (intercalate, find)
import Control.Arrow (second)
import Control.Monad.Trans (MonadIO, liftIO)
import Happstack.Server
import Happstack.State hiding (Version)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as BS
-- import Data.ByteString.Lazy.Char8 (ByteString)

data VersionsFeature = VersionsFeature {
    versionsResource :: VersionsResource,
    preferredHook  :: Hook (PackageName -> PreferredInfo -> IO ()),
    deprecatedHook :: Hook (PackageName -> Maybe [PackageName] -> IO ()),
    putDeprecated :: PackageName -> MServerPart Bool,
    putPreferred  :: PackageName -> MServerPart (),
    updateDeprecatedTags :: IO ()
}

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
    -- /packages/preferred
    -- /package/:package/preferred
    -- /package/:package/preferred/edit
    -- /packages/deprecated
    -- /package/:package/deprecated
    -- /package/:package/deprecated/edit
}

instance HackageFeature VersionsFeature where
    getFeature versions = HackageModule
      { featureName = "versions"
      , resources   = map ($versionsResource versions)
            [preferredResource, preferredPackageResource,
             deprecatedResource, deprecatedPackageResource,
             preferredText]
      , dumpBackup    = Nothing
      , restoreBackup = Nothing
      }
    initHooks versions = [updateDeprecatedTags versions]

initVersionsFeature :: Config -> CoreFeature -> UploadFeature -> TagsFeature -> IO VersionsFeature
initVersionsFeature _ core _ tags = do
    prefHook <- newHook
    deprHook <- newHook
    return $ fix $ \f -> VersionsFeature
      { versionsResource = fix $ \r -> VersionsResource
          { preferredResource = resourceAt "/packages/preferred.:format"
          , preferredText = (resourceAt "/packages/preferred-versions") { resourceGet = [("txt", \_ -> textPreferred)] }
          , preferredPackageResource = resourceAt "/package/:package/preferred.:format"
          , deprecatedResource = resourceAt "/packages/deprecated.:format"
          , deprecatedPackageResource = resourceAt "/package/:package/deprecated.:format"

          , preferredUri = \format -> renderResource (preferredResource r) [format]
          , preferredPackageUri = \format pkgid -> renderResource (preferredPackageResource r) [display pkgid, format]
          , deprecatedUri = \format -> renderResource (deprecatedResource r) [format]
          , deprecatedPackageUri = \format pkgid -> renderResource (deprecatedPackageResource r) [display pkgid, format]
          }
      , preferredHook  = prefHook
      , deprecatedHook = deprHook
      , putPreferred  = doPutPreferred f core
      , putDeprecated = doPutDeprecated f
      , updateDeprecatedTags = do
            pkgs <- fmap (Map.keys . deprecatedMap) $ query GetPreferredVersions
            setCalculatedTag tags (Tag "deprecated") (Set.fromDistinctAscList pkgs)
      }
  where
    textPreferred = fmap toResponse makePreferredVersions

---------------------------
withPackagePreferred :: PackageId -> (PkgInfo -> [PkgInfo] -> MServerPart a) -> MServerPart a
withPackagePreferred pkgid func = query GetPackagesState >>= \state ->
    case PackageIndex.lookupPackageName (packageList state) (packageName pkgid) of
        []   ->  packageError [MText "No such package in package index"]
        pkgs  | pkgVersion pkgid == Version [] [] -> query (GetPreferredInfo $ packageName pkgid) >>= \info -> do
            let rangeToCheck = sumRange info
            case maybe id (\r -> filter (flip withinRange r . packageVersion)) rangeToCheck pkgs of
                -- no preferred version available, choose latest from list ordered by version
                []    -> func (last pkgs) pkgs
                -- return latest preferred version
                pkgs' -> func (last pkgs') pkgs
        pkgs -> case find ((== packageVersion pkgid) . packageVersion) pkgs of
            Nothing  -> packageError [MText $ "No such package version for " ++ display (packageName pkgid)]
            Just pkg -> func pkg pkgs
  where packageError = returnError 404 "Package not found"

withPackagePreferredPath :: DynamicPath -> (PkgInfo -> [PkgInfo] -> MServerPart a) -> MServerPart a
withPackagePreferredPath dpath func = withPackageId dpath $ \pkgid -> withPackagePreferred pkgid func

doPutPreferred :: VersionsFeature -> CoreFeature -> PackageName -> MServerPart ()
doPutPreferred f core pkgname =
        withPackageAll pkgname $ \pkgs ->
        withPackageNameAuth pkgname $ \_ _ -> do
    pref <- getDataFn $ fmap lines $ look "preferred"
    depr <- getDataFn $ fmap (map snd . filter ((=="deprecated") . fst)) lookPairs
    case sequence . map simpleParse =<< pref of
        Just prefs -> case sequence . map simpleParse =<< depr of
            Just deprs -> case all (`elem` map packageVersion pkgs) deprs of
                True  -> do
                    update $ SetPreferredRanges pkgname prefs
                    update $ SetDeprecatedVersions pkgname deprs
                    newInfo <- query $ GetPreferredInfo pkgname
                    prefVersions <- makePreferredVersions
                    Cache.modifyCache (indexExtras core) $ Map.insert "preferred-versions" (BS.pack prefVersions)
                    runHook'' (preferredHook f) pkgname newInfo
                    runHook (packageIndexChange core)
                    returnOk ()
                False -> preferredError "Not all of the selected versions are in the main index."
            Nothing -> preferredError "Version could not be parsed."
        Nothing -> preferredError "Expected format of the preferred ranges field is one version range per line, e.g. '<2.3 || 3.*' (see Cabal documentation for the syntax)."
  where
    preferredError = returnError 400 "Preferred ranges failed" . return . MText

doPutDeprecated :: VersionsFeature -> PackageName -> MServerPart Bool
doPutDeprecated f pkgname =
        withPackageAll pkgname $ \_ ->
        withPackageNameAuth pkgname $ \_ _ -> do
    index  <- fmap packageList $ query GetPackagesState
    isDepr <- getDataFn $ look "deprecated"
    case isDepr of
        Just {} -> do
            depr <- getDataFn $ fmap words $ look "by"
            case sequence . map simpleParse =<< depr of
                Just deprs -> case filter (null . PackageIndex.lookupPackageName index) deprs of
                    [] -> do
                        doUpdates (Just deprs)
                        returnOk True
                    pkgs -> deprecatedError $ "Some superseding packages aren't in the main index: " ++ intercalate ", " (map display pkgs)
                Nothing -> deprecatedError "Expected format of the 'superseded by' field is a list of package names separated by spaces."
        Nothing -> do
            doUpdates Nothing
            returnOk False
  where
    deprecatedError = returnError 400 "Deprecation failed" . return . MText
    doUpdates deprs = do
        update $ SetDeprecatedFor pkgname deprs
        runHook'' (deprecatedHook f) pkgname deprs
        liftIO $ updateDeprecatedTags f

data PreferredRender = PreferredRender {
    rendSumRange :: String,
    rendRanges   :: [String],
    rendVersions :: [Version]
} deriving (Show, Eq)

renderPrefInfo :: PreferredInfo -> PreferredRender
renderPrefInfo pref = PreferredRender {
    rendSumRange = maybe "-any" display $ sumRange pref,
    rendRanges = map display $ preferredRanges pref,
    rendVersions = deprecatedVersions pref
}

doPreferredRender :: PackageName -> MServerPart PreferredRender
doPreferredRender pkgname = withPackageAll pkgname $ \_ -> do
    pref <- query $ GetPreferredInfo pkgname
    returnOk $ renderPrefInfo pref

doDeprecatedRender :: PackageName -> MServerPart (Maybe [PackageName])
doDeprecatedRender pkgname = withPackageAll pkgname $ \_ -> fmap Right . query $ GetDeprecatedFor pkgname

doPreferredsRender :: MonadIO m => m [(PackageName, PreferredRender)]
doPreferredsRender = query GetPreferredVersions >>=
    return . map (second renderPrefInfo) . Map.toList . preferredMap 

doDeprecatedsRender :: MonadIO m => m [(PackageName, [PackageName])]
doDeprecatedsRender = query GetPreferredVersions >>=
    return . Map.toList . deprecatedMap

makePreferredVersions :: MonadIO m => m String
makePreferredVersions = query GetPreferredVersions >>= \(PreferredVersions prefs _) -> do
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

