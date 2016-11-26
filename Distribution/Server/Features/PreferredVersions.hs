{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards,
             PatternGuards, OverloadedStrings #-}
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

import Distribution.Server.Features.PreferredVersions.State
import Distribution.Server.Features.PreferredVersions.Backup

import Distribution.Server.Features.Core
import Distribution.Server.Features.Upload
import Distribution.Server.Features.Tags

import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import Distribution.Server.Packages.Types

import Distribution.Package
import Distribution.Version
import Distribution.Text

import Data.Function (fix)
import Data.List (intercalate, find)
import Data.Maybe (isJust, fromMaybe, catMaybes)
import Data.Time.Clock (getCurrentTime)
import Control.Arrow (second)
import Control.Applicative (optional)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BS (pack) -- Only used for ASCII data
import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text
import qualified Data.Vector         as Vector

data VersionsFeature = VersionsFeature {
    versionsFeatureInterface :: HackageFeature,

    queryGetPreferredInfo :: forall m. MonadIO m => PackageName -> m PreferredInfo,
    queryGetDeprecatedFor :: forall m. MonadIO m => PackageName -> m (Maybe [PackageName]),
    queryGetPreferredVersions :: forall m. MonadIO m => m PreferredVersions,
    versionsResource :: VersionsResource,
    deprecatedHook :: Hook (PackageName, Maybe [PackageName]) (),
    putDeprecated :: PackageName -> ServerPartE Bool,
    putPreferred  :: PackageName -> ServerPartE (),
    updateDeprecatedTags :: IO (),

    doPreferredRender     :: PackageName -> ServerPartE PreferredRender,
    doDeprecatedRender    :: PackageName -> ServerPartE (Maybe [PackageName]),
    doPreferredsRender    :: forall m. MonadIO m => m [(PackageName, PreferredRender)],
    doDeprecatedsRender   :: forall m. MonadIO m => m [(PackageName, [PackageName])],

    withPackagePreferred     :: forall a. PackageId -> (PkgInfo -> [PkgInfo] -> ServerPartE a) -> ServerPartE a,
    withPackageVersion :: forall a. PackageId -> (PkgInfo -> ServerPartE a) -> ServerPartE a,
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


initVersionsFeature :: ServerEnv
                    -> IO (CoreFeature
                        -> UploadFeature
                        -> TagsFeature
                        -> IO VersionsFeature)
initVersionsFeature env@ServerEnv{serverStateDir} = do
    preferredState <- preferredStateComponent False serverStateDir
    deprecatedHook <- newHook

    return $ \core upload tags -> do

      let feature = versionsFeature env
                                    core upload tags
                                    preferredState deprecatedHook
      return feature

preferredStateComponent :: Bool -> FilePath -> IO (StateComponent AcidState PreferredVersions)
preferredStateComponent freshDB stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "PreferredVersions")
                           (initialPreferredVersions freshDB)
  return StateComponent {
      stateDesc    = "Preferred package versions"
    , stateHandle  = st
    , getState     = query st GetPreferredVersions
    , putState     = update st . ReplacePreferredVersions
    , resetState   = preferredStateComponent True
    , backupState  = \_ -> backupPreferredVersions
    , restoreState = restorePreferredVersions
    }

versionsFeature :: ServerEnv
                -> CoreFeature
                -> UploadFeature
                -> TagsFeature
                -> StateComponent AcidState PreferredVersions
                -> Hook (PackageName, Maybe [PackageName]) ()
                -> VersionsFeature
versionsFeature ServerEnv{ serverVerbosity = verbosity }
                CoreFeature{..}
                UploadFeature{..}
                TagsFeature{..}
                preferredState
                deprecatedHook
  = VersionsFeature{..}
  where
    versionsFeatureInterface = (emptyHackageFeature "versions") {
        featureResources =
          map ($ versionsResource) [
              preferredResource
            , preferredPackageResource
            , deprecatedResource
            , deprecatedPackageResource
            , preferredText
            ]
      , featurePostInit = do updateDeprecatedTags
                             ephemeralPrefsMigration
      , featureState    = [abstractAcidStateComponent preferredState]
      }

    queryGetPreferredInfo :: MonadIO m => PackageName -> m PreferredInfo
    queryGetPreferredInfo name = queryState preferredState (GetPreferredInfo name)

    queryGetDeprecatedFor :: MonadIO m => PackageName -> m (Maybe [PackageName])
    queryGetDeprecatedFor name = queryState preferredState (GetDeprecatedFor name)

    queryGetPreferredVersions :: MonadIO m => m PreferredVersions
    queryGetPreferredVersions = queryState preferredState GetPreferredVersions

    updateDeprecatedTags = do
      pkgs <- deprecatedMap <$> queryState preferredState GetPreferredVersions
      setCalculatedTag (Tag "deprecated") (Map.keysSet pkgs)

    CoreResource{..} = coreResource
    versionsResource = fix $ \r -> VersionsResource
      { preferredResource        = resourceAt "/packages/preferred.:format"
      , preferredPackageResource = (resourceAt "/package/:package/preferred.:format") {
            resourceDesc = [(GET, "List package's versions divided by type: normal, unpreferred, and deprecated")],
            resourceGet = [("json", handlePreferredPackageGet)]
          }
      , preferredText = (resourceAt "/packages/preferred-versions") {
            resourceGet = [("txt", \_ -> textPreferred)]
          }
      , deprecatedResource = (resourceAt "/packages/deprecated.:format") {
            resourceGet = [("json", handlePackagesDeprecatedGet)]
          }
      , deprecatedPackageResource = (resourceAt "/package/:package/deprecated.:format") {
            resourceGet = [("json", handlePackageDeprecatedGet) ],
            resourcePut = [("json", handlePackageDeprecatedPut) ]
          }
      , preferredUri = \format ->
          renderResource (preferredResource r) [format]
      , preferredPackageUri = \format pkgid ->
          renderResource (preferredPackageResource r) [display pkgid, format]
      , deprecatedUri = \format ->
          renderResource (deprecatedResource r) [format]
      , deprecatedPackageUri = \format pkgid ->
          renderResource (deprecatedPackageResource r) [display pkgid, format]
      }

    textPreferred = toResponse <$> makeGlobalPreferredVersions

    handlePreferredPackageGet :: DynamicPath -> ServerPartE Response
    handlePreferredPackageGet dpath = do
      pkgname <- packageInPath dpath
      pkgs <- lookupPackageName pkgname
      prefInfo <- queryGetPreferredInfo pkgname
      let
        classifiedVersions = Map.fromListWith (++)
          $ map (\(v, i) -> (i, [v]))
            $ classifyVersions prefInfo
              $ map packageVersion pkgs
        versionType NormalVersion = "normal-version"
        versionType DeprecatedVersion = "deprecated-version"
        versionType UnpreferredVersion = "unpreferred-version"
      return . toResponse . object
        $ map (\(i, vs) -> (Text.pack . versionType $ i, array $ map (string . display) vs))
          $ Map.toList classifiedVersions

    handlePackagesDeprecatedGet :: DynamicPath -> ServerPartE Response
    handlePackagesDeprecatedGet _ = do
      deprPkgs <- deprecatedMap <$> queryState preferredState GetPreferredVersions
      return $ toResponse $ array
          [ object
              [ ("deprecated-package", string $ display deprPkg)
              , ("in-favour-of", array [ string $ display pkg
                                       | pkg <- replacementPkgs ])
              ]
          | (deprPkg, replacementPkgs) <- Map.toList deprPkgs ]

    handlePackageDeprecatedGet :: DynamicPath -> ServerPartE Response
    handlePackageDeprecatedGet dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      mdep <- queryState preferredState (GetDeprecatedFor pkgname)
      return $ toResponse $
        object
            [ ("is-deprecated", Bool (isJust mdep))
            , ("in-favour-of", array [ string $ display pkg
                                     | pkg <- fromMaybe [] mdep ])
            ]

    handlePackageDeprecatedPut :: DynamicPath -> ServerPartE Response
    handlePackageDeprecatedPut dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname
      guardAuthorisedAsMaintainerOrTrustee pkgname
      jv <- expectAesonContent
      case jv of
        Object o
          -- FIXME should just be a nested case
          -- or something more human friendly
          | fields <- HashMap.toList o
          , Just (Bool deprecated) <- lookup "is-deprecated" fields
          , Just (Array strs)      <- lookup "in-favour-of"  fields
          -- FIXME Audit this parsing -> PackageName code, suspiciously
          -- reliant on MonomorphismRestriction to resolve ambiguity
          , let asPackage (String s) = simpleParse (Text.unpack s)
                asPackage _          = Nothing
                mpkgs :: [Maybe PackageName]
                mpkgs = map asPackage (Vector.toList strs)
          , all isJust mpkgs
          -> do let deprecatedInfo | deprecated = Just (catMaybes mpkgs)
                                   | otherwise  = Nothing
                updatePackageDeprecation pkgname deprecatedInfo
                ok $ toResponse ()
        _ -> errBadRequest "bad json format or content" []

    updatePackageDeprecation :: MonadIO m => PackageName -> Maybe [PackageName] -> m ()
    updatePackageDeprecation pkgname deprs = liftIO $ do
      updateState preferredState $ SetDeprecatedFor pkgname deprs
      runHook_ deprecatedHook (pkgname, deprs)
      updateDeprecatedTags

    ---------------------------
    -- This is a function used by the HTML feature to select the version to display.
    -- It could be enhanced by displaying a search page in the case of failure,
    -- which is outside of the scope of this feature.
    withPackagePreferred :: PackageId -> (PkgInfo -> [PkgInfo] -> ServerPartE a) -> ServerPartE a
    withPackagePreferred pkgid func = do
      pkgIndex <- queryGetPackageIndex
      case PackageIndex.lookupPackageName pkgIndex (packageName pkgid) of
            []   ->  packageError [MText $ "No such package in package index. ", MLink "Search for related terms instead?" $ "/packages/search?terms=" ++ (display $ pkgName pkgid)]
            pkgs  | pkgVersion pkgid == Version [] [] -> queryState preferredState (GetPreferredInfo $ packageName pkgid) >>= \info -> do
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

    withPackageVersion :: PackageId -> (PkgInfo -> ServerPartE a) -> ServerPartE a
    withPackageVersion pkgid func = do
        pkgIndex <- queryGetPackageIndex
        guard (packageVersion pkgid /= Version [] [])
        case PackageIndex.lookupPackageName pkgIndex (packageName pkgid) of
            []   ->  packageError [MText $ "No such package in package index. ", MLink "Search for related terms instead?" $ "/packages/search?terms=" ++ (display $ pkgName pkgid)]
            pkg -> case find ((== packageVersion pkgid) . packageVersion) pkg of
                Nothing  -> packageError [MText $ "No such package version for " ++ display (packageName pkgid)]
                Just pkg' -> func pkg'
      where packageError = errNotFound "Package not found"

    withPackagePreferredPath :: DynamicPath -> (PkgInfo -> [PkgInfo] -> ServerPartE a) -> ServerPartE a
    withPackagePreferredPath dpath func = do
      pkgid <- packageInPath dpath
      withPackagePreferred pkgid func

    putPreferred :: PackageName -> ServerPartE ()
    putPreferred pkgname = do
      pkgs <- lookupPackageName pkgname
      guardAuthorisedAsMaintainerOrTrustee pkgname
      (prefs, deprs) <- lookPrefRangeDeprecatedVersions pkgs

      prefinfo <- updateState preferredState (SetPreferredInfo pkgname prefs deprs)
      updateIndexPackagePreferredVersions pkgname prefinfo
      where
        lookPrefRangeDeprecatedVersions pkgs = do
          prefStrs <- lines <$> look "preferred"
          deprStrs <- looks "deprecated"
          either preferredError return $ do
            prefs <- mapM simpleParse prefStrs ?! rangeFormatMsg
            deprs <- mapM simpleParse deprStrs ?! "Version could not be parsed."
            guard (all (`elem` map packageVersion pkgs) deprs)
              ?! "A selected version does not exist"
            return (prefs, deprs)

        preferredError detail =
          errBadRequest "Setting the preferred ranges failed" [MText detail]

        rangeFormatMsg = "The expected format of the preferred ranges field is "
                      ++ "one version range per line, e.g. '<2.3 || 3.*' "
                      ++ "(the same as the .cabal version range syntax, see "
                      ++ "the Cabal documentation)."

    updateIndexPackagePreferredVersions :: MonadIO m => PackageName -> PreferredInfo -> m ()
    updateIndexPackagePreferredVersions pkgname prefinfo = do
      now <- liftIO $ getCurrentTime
      let prefEntryName = display pkgname </> "preferred-versions"
          prefContent   = fromMaybe "" $
                          formatSinglePreferredVersions pkgname prefinfo
      updateArchiveIndexEntry prefEntryName (BS.pack prefContent) now

    putDeprecated :: PackageName -> ServerPartE Bool
    putDeprecated pkgname = do
      guardValidPackageName pkgname
      guardAuthorisedAsMaintainerOrTrustee pkgname
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
            void $ updateState preferredState $ SetDeprecatedFor pkgname deprs
            runHook_ deprecatedHook (pkgname, deprs)
            liftIO $ updateDeprecatedTags

    renderPrefInfo :: PreferredInfo -> PreferredRender
    renderPrefInfo pref = PreferredRender {
        rendSumRange = maybe "-any" display $ sumRange pref,
        rendRanges   = map display $ preferredRanges pref,
        rendVersions = deprecatedVersions pref
    }

    doPreferredRender :: PackageName -> ServerPartE PreferredRender
    doPreferredRender pkgname = do
      guardValidPackageName pkgname
      pref <- queryState preferredState $ GetPreferredInfo pkgname
      return $ renderPrefInfo pref

    doDeprecatedRender :: PackageName -> ServerPartE (Maybe [PackageName])
    doDeprecatedRender pkgname = do
      guardValidPackageName pkgname
      queryState preferredState $ GetDeprecatedFor pkgname

    doPreferredsRender :: MonadIO m => m [(PackageName, PreferredRender)]
    doPreferredsRender = queryState preferredState GetPreferredVersions >>=
        return . map (second renderPrefInfo) . Map.toList . preferredMap

    doDeprecatedsRender :: MonadIO m => m [(PackageName, [PackageName])]
    doDeprecatedsRender = queryState preferredState GetPreferredVersions >>=
        return . Map.toList . deprecatedMap

    makeGlobalPreferredVersions :: (Functor m, MonadIO m) => m String
    makeGlobalPreferredVersions = do
      prefs <- preferredMap <$> queryState preferredState GetPreferredVersions
      return $! formatGlobalPreferredVersions (Map.toList prefs)

    formatSinglePreferredVersions :: PackageName -> PreferredInfo -> Maybe String
    formatSinglePreferredVersions pkgname pref =
      display . Dependency pkgname <$> sumRange pref

    formatGlobalPreferredVersions :: [(PackageName, PreferredInfo)] -> String
    formatGlobalPreferredVersions =
        unlines . (topText++)
                . catMaybes . map (uncurry formatSinglePreferredVersions)
      where
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

    -- One-off complex migration
    ephemeralPrefsMigration = do
      PreferredVersions {migratedEphemeralPrefs, preferredMap}
        <- queryState preferredState GetPreferredVersions
      unless migratedEphemeralPrefs $
        logTiming verbosity "preferred-versions migration" $ do
          sequence_
            [ updateIndexPackagePreferredVersions pkgname prefinfo
            | (pkgname, prefinfo) <- Map.toList preferredMap ]
          updateState preferredState SetMigratedEphemeralPrefs

{------------------------------------------------------------------------------
  Some aeson auxiliary functions
------------------------------------------------------------------------------}

array :: [Value] -> Value
array = Array . Vector.fromList

object :: [(Text.Text, Value)] -> Value
object = Object . HashMap.fromList

string :: String -> Value
string = String . Text.pack
