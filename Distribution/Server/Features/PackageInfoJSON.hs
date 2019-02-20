{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Server.Features.PackageInfoJSON (
  PackageInfoJSONFeature(..)
  , PackageInfoJSONResource(..)
  , initPackageInfoJSONFeature

  , PackageBasicDescription(..)
  , PackageVersions(..)
  ) where

import Prelude ()
import Distribution.Server.Prelude

import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS (toStrict)
import qualified Data.Text                  as T
import qualified Data.Vector                as Vector

import           Distribution.License                         (licenseToSPDX)
import           Distribution.Package                         (PackageIdentifier(..),
                                                               PackageName, packageName,
                                                               packageVersion)
import qualified Distribution.Parsec.Common                   as Parsec
import qualified Distribution.PackageDescription.Parsec       as PkgDescr
import qualified Distribution.Types.GenericPackageDescription as PkgDescr
import qualified Distribution.Types.PackageDescription        as PkgDescr
import           Distribution.Version                         (nullVersion)

import           Distribution.Server.Framework                  ((</>))
import qualified Distribution.Server.Framework                  as Framework
import           Distribution.Server.Features.Core              (CoreFeature(..),
                                                                 CoreResource(..),
                                                                 isPackageChangeAny)
import qualified Distribution.Server.Features.PreferredVersions as Preferred
import           Distribution.Server.Packages.Types             (CabalFileText(..), pkgMetadataRevisions)
import           Distribution.Server.Framework.BackupRestore    (RestoreBackup(..))

import           Distribution.Server.Features.PackageInfoJSON.State (PackageBasicDescription(..),
                                                                     PackageVersions(..),
                                                                     PackageInfoState(..),
                                                                     GetPackageInfo(..),
                                                                     ReplacePackageInfo(..),
                                                                     GetDescriptionFor(..),
                                                                     SetDescriptionFor(..),
                                                                     GetVersionsFor(..),
                                                                     SetVersionsFor(..),
                                                                     initialPackageInfoState
                                                                    )


data PackageInfoJSONFeature = PackageInfoJSONFeature {
    packageInfoJSONFeatureInterface :: Framework.HackageFeature
}


instance Framework.IsHackageFeature PackageInfoJSONFeature where
    getFeatureInterface = packageInfoJSONFeatureInterface


data PackageInfoJSONResource = PackageInfoJSONResource {
    packageJSONResource        :: Framework.Resource,
    packageVersionJSONResource :: Framework.Resource
}


-- | Initializing our feature involves adding JSON variants to the
-- endpoints that serve basic information about a package-version,
-- and a packages version deprecation status.
-- Aditionally we set up caching for these endpoints,
-- and attach a package change hook that invalidates the cache
-- line for a package when in changes
initPackageInfoJSONFeature
  :: Framework.ServerEnv
  -> IO (CoreFeature -> Preferred.VersionsFeature -> IO PackageInfoJSONFeature)
initPackageInfoJSONFeature env = do
  packageInfoState <- packageInfoStateComponent False (Framework.serverStateDir env)
  return $ \core preferred -> do

    let coreR = coreResource core
        info = "Get basic package information"
        vInfo = "Get basic package information at a specific metadata revision"

        jsonResources = [
          (Framework.extendResource (corePackagePage coreR)) {
                Framework.resourceDesc = [(Framework.GET, info)]
              , Framework.resourceGet  =
                  [("json", servePackageBasicDescription coreR
                            preferred packageInfoState)]
              }
          , (Framework.extendResource (coreCabalFileRev coreR)) {
                Framework.resourceDesc = [(Framework.GET, vInfo)]
              , Framework.resourceGet  =
                  [("json", servePackageBasicDescription coreR
                     preferred packageInfoState)]
              }
          ]

        -- When a package is modified in any way, delet all its
        -- PackageInfoState cache lines.
        -- They will be recalculated next time the endpoint
        -- is hit
        postInit = Framework.registerHookJust
                   (packageChangeHook core)
                   isPackageChangeAny $ \(pkgid, _) -> do

          Framework.updateState packageInfoState $
            SetDescriptionFor (pkgid, Nothing) Nothing
          Framework.updateState packageInfoState $
            SetVersionsFor (packageName pkgid) Nothing

    return $ PackageInfoJSONFeature {
      packageInfoJSONFeatureInterface =
          (Framework.emptyHackageFeature "package-info-json")
            { Framework.featureDesc      = "Provide JSON endpoints for basic package descriptions"
            , Framework.featureResources = jsonResources
            , Framework.featureCaches    = []
            , Framework.featurePostInit  = postInit
            , Framework.featureState     =
                [Framework.abstractAcidStateComponent packageInfoState]
            }
      }


-- | Pure function for extrcacting basic package info from a Cabal file
getBasicDescription
  :: CabalFileText
  -> Int
     -- ^ Metadata revision. This will be added to the resulting
     --   @PackageBasicDescription@
  -> Either String PackageBasicDescription
getBasicDescription (CabalFileText cf) metadataRev =
  let parseResult = PkgDescr.parseGenericPackageDescription (BS.toStrict cf)
  in case PkgDescr.runParseResult parseResult of
    (_, Right pkg) -> let
      pkgd                  = PkgDescr.packageDescription pkg
      pbd_author            = T.pack $ PkgDescr.author pkgd
      pbd_copyright         = T.pack $ PkgDescr.copyright pkgd
      pbd_synopsis          = T.pack $ PkgDescr.synopsis pkgd
      pbd_description       = T.pack $ PkgDescr.description pkgd
      pbd_license           = either id licenseToSPDX $
                                PkgDescr.licenseRaw pkgd
      pbd_homepage          = T.pack $ PkgDescr.homepage pkgd
      pbd_metadata_revision = metadataRev
      in
      return $ PackageBasicDescription {..}
    (_, Left e) -> Left $ "Could not parse cabal file: "
                   <> unlines (Parsec.showPError "" <$> snd e)


-- | Get a JSON @PackageBasicDescription@ for a particular
--   package/version/metadata-revision
--      OR
--   A listing of versions and their deprecation states
servePackageBasicDescription
  :: CoreResource
  -> Preferred.VersionsFeature
  -> Framework.StateComponent Framework.AcidState PackageInfoState
  -> Framework.DynamicPath
     -- ^ URI specifying a package and version `e.g. lens or lens-4.11`
  -> Framework.ServerPartE Framework.Response
servePackageBasicDescription resource preferred packageInfoState dpath = do

  let metadataRev :: Maybe Int = lookup "revision" dpath >>= Framework.fromReqURI

  pkgid@(PackageIdentifier name version) <- packageInPath resource dpath
  guardValidPackageName resource name

  if (version /= nullVersion)
    then lookupOrInsertDescr pkgid metadataRev
    else lookupOrInsertVersions name

  where

    lookupOrInsertDescr
      :: PackageIdentifier
      -> Maybe Int
      -> Framework.ServerPartE Framework.Response
    lookupOrInsertDescr pkgid metadataRev = do
      cachedDescr <- Framework.queryState packageInfoState $
                     GetDescriptionFor (pkgid, metadataRev)
      descr :: PackageBasicDescription <- case cachedDescr of
        Just d  -> return d
        Nothing -> do
          d <- getPackageDescr pkgid metadataRev
          Framework.updateState packageInfoState $
            SetDescriptionFor (pkgid, metadataRev) (Just d)
          return d
      return $ Framework.toResponse $ Aeson.toJSON descr

    getPackageDescr pkgid metadataRev = do
      guardValidPackageId resource pkgid
      pkg <- lookupPackageId resource pkgid

      let metadataRevs = fst <$> pkgMetadataRevisions pkg
          nMetadata    = Vector.length metadataRevs
          metadataInd  = fromMaybe (nMetadata - 1) metadataRev

      when (metadataInd < 0 || metadataInd >= nMetadata)
        (Framework.errNotFound "Revision not found"
         [Framework.MText
           $ "There are " <> show nMetadata <> " metadata revisions. Index "
           <> show metadataInd <> " is out of bounds."]
        )

      let cabalFile = metadataRevs Vector.! metadataInd
          pkgDescr  = getBasicDescription cabalFile metadataInd
      case pkgDescr of
        Left e  -> Framework.errInternalError [Framework.MText e]
        Right d -> return d

    lookupOrInsertVersions
      :: PackageName
      -> Framework.ServerPartE Framework.Response
    lookupOrInsertVersions pkgname = do
      cachedVersions <- Framework.queryState packageInfoState $
                        GetVersionsFor pkgname
      vers :: PackageVersions <- case cachedVersions of
        Just vs  -> return vs
        Nothing -> do
          vs <- getVersionListing pkgname
          Framework.updateState packageInfoState $
            SetVersionsFor pkgname (Just vs)
          return vs
      return $ Framework.toResponse $ Aeson.toJSON vers

    getVersionListing name = do
      pkgs <- lookupPackageName resource name
      prefInfo <- Preferred.queryGetPreferredInfo preferred name
      return
        . PackageVersions
        . Preferred.classifyVersions prefInfo
        $ fmap packageVersion pkgs


-- | Our backup doesn't produce any entries, and backup restore
--   returns an empty state. Our responses are cheap enough to
--   compute that we would rather regenerate them by need than
--   deal with the complexity persisting backups in
--   yet-another-format
packageInfoStateComponent
  :: Bool
  -> FilePath
  -> IO (Framework.StateComponent Framework.AcidState PackageInfoState)
packageInfoStateComponent freshDB stateDir = do
  st <- Framework.openLocalStateFrom
        (stateDir </> "db" </> "PackageInfoJSON")
        (initialPackageInfoState freshDB)
  return Framework.StateComponent {
      stateDesc    = "Preferred package versions"
    , stateHandle  = st
    , getState     = Framework.query st GetPackageInfo
    , putState     = Framework.update st . ReplacePackageInfo
    , resetState   = packageInfoStateComponent True
    , backupState  = \_ -> return []
    , restoreState = nullRestore (initialPackageInfoState True)
    }
  where

    nullRestore :: PackageInfoState -> RestoreBackup PackageInfoState
    nullRestore st = RestoreBackup {
      restoreEntry = \_ -> nullRestore <$> pure (initialPackageInfoState True)
      , restoreFinalize = return st
      }
