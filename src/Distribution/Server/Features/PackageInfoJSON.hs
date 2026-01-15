{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import           Data.Aeson                 ((.=))
import qualified Data.Aeson.Key             as Key
import qualified Data.ByteString.Lazy.Char8 as BS (toStrict)
import qualified Data.Map.Strict      as Map
import qualified Data.Text                  as T
import qualified Data.Vector                as Vector

import           Distribution.License                         (licenseToSPDX)
import           Distribution.Package                         (PackageIdentifier(..),
                                                               packageVersion)
import qualified Distribution.Parsec                          as Parsec
import qualified Distribution.PackageDescription.Parsec       as PkgDescr
import           Distribution.Text                            (display)
import qualified Distribution.Types.GenericPackageDescription as PkgDescr
import qualified Distribution.Types.PackageDescription        as PkgDescr
import qualified Distribution.Pretty       as Pretty
import           Distribution.SPDX.License                    (License)
import           Distribution.Version                         (nullVersion, Version)

import qualified Distribution.Server.Framework                as Framework
import           Distribution.Server.Features.Core            (CoreFeature(..),
                                                               CoreResource(..))
import qualified Distribution.Server.Features.PreferredVersions as Preferred
import           Distribution.Server.Packages.Types           (CabalFileText(..), pkgMetadataRevisions)

import Distribution.Utils.ShortText (fromShortText)
import Data.Foldable (toList)
import Data.Traversable (for)
import qualified Data.List as List
import Data.Time (UTCTime)
import Distribution.Server.Users.Types (UserName (..), UserInfo(..))
import Distribution.Server.Features.Users (UserFeature(lookupUserInfo))

data PackageBasicDescription = PackageBasicDescription
  { pbd_license           :: !License
  , pbd_copyright         :: !T.Text
  , pbd_synopsis          :: !T.Text
  , pbd_description       :: !T.Text
  , pbd_author            :: !T.Text
  , pbd_homepage          :: !T.Text
  , pbd_metadata_revision :: !Int
  , pbd_uploaded_at       :: !UTCTime
  } deriving (Eq, Show)



-- | Data type used in the `/package/:packagename` JSON endpoint
data PackageBasicDescriptionDTO = PackageBasicDescriptionDTO
  { license           :: !License
  , copyright         :: !T.Text
  , synopsis          :: !T.Text
  , description       :: !T.Text
  , author            :: !T.Text
  , homepage          :: !T.Text
  , metadata_revision :: !Int
  , uploaded_at       :: !UTCTime
  , uploader          :: !UserName
  } deriving (Eq, Show)

instance Aeson.ToJSON PackageBasicDescriptionDTO where
  toJSON PackageBasicDescriptionDTO {..} =
    Aeson.object
      [ Key.fromString "license"           .= Pretty.prettyShow license
      , Key.fromString "copyright"         .= copyright
      , Key.fromString "synopsis"          .= synopsis
      , Key.fromString "description"       .= description
      , Key.fromString "author"            .= author
      , Key.fromString "homepage"          .= homepage
      , Key.fromString "metadata_revision" .= metadata_revision
      , Key.fromString "uploaded_at"       .= uploaded_at
      , Key.fromString "uploader"          .= uploader
      ]


-- | An index of versions for one Hackage package
--   and their preferred/deprecated status
newtype PackageVersions = PackageVersions {
  unPackageVersions :: [(Version, Preferred.VersionStatus)]
  } deriving (Eq, Show)

-- | This encoding of @PackageVersions@ is used in the
-- `/package/$package` endpoint (when the URI doesn't specify)
-- a version. Any change here is an API change.
instance Aeson.ToJSON PackageVersions where
  toJSON (PackageVersions p) =
    Aeson.toJSON
    $ Map.mapKeys display
    $ fmap encodeStatus
    $ Map.fromList p
    where
      encodeStatus = \case
        Preferred.NormalVersion      -> "normal"
        Preferred.DeprecatedVersion  -> "deprecated"
        Preferred.UnpreferredVersion -> "unpreferred"



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
initPackageInfoJSONFeature
  :: Framework.ServerEnv
  -> IO (CoreFeature -> Preferred.VersionsFeature -> UserFeature -> IO PackageInfoJSONFeature)
initPackageInfoJSONFeature _env = do
  return $ \core preferred userFeature -> do

    let coreR = coreResource core
        info = "Get basic package information: \
               \The response contains a JSON object where the keys are version numbers as strings, \
               \and the values are whether the version is preferred or not"
        vInfo = "Get basic package information at a specific metadata revision"

        jsonResources = [
          (Framework.extendResource (corePackagePage coreR)) {
                Framework.resourceDesc = [(Framework.GET, info)]
              , Framework.resourceGet  =
                  [("json", servePackageBasicDescription coreR userFeature
                            preferred)]
              }
          , (Framework.extendResource (coreCabalFileRev coreR)) {
                Framework.resourceDesc = [(Framework.GET, vInfo)]
              , Framework.resourceGet  =
                  [("json", servePackageBasicDescription coreR userFeature
                     preferred)]
              }
          ]

    return $ PackageInfoJSONFeature {
      packageInfoJSONFeatureInterface =
          (Framework.emptyHackageFeature "package-info-json")
            { Framework.featureDesc      = "Provide JSON endpoints for basic package descriptions"
            , Framework.featureResources = jsonResources
            , Framework.featureCaches    = []
            , Framework.featurePostInit  = pure ()
            , Framework.featureState     = []
            }
      }


-- | Pure function for extracting basic package info from a Cabal file
getBasicDescription
  :: UTCTime
    -- ^ Time of upload
  -> CabalFileText
  -> Int
     -- ^ Metadata revision. This will be added to the resulting
     --   @PackageBasicDescription@
  -> Either String PackageBasicDescription
getBasicDescription uploadedAt (CabalFileText cf) metadataRev =
  let parseResult = PkgDescr.parseGenericPackageDescription (BS.toStrict cf)
  in case PkgDescr.runParseResult parseResult of
    (_, Right pkg) -> let
      pkgd                  = PkgDescr.packageDescription pkg
      pbd_author            = T.pack . fromShortText $ PkgDescr.author pkgd
      pbd_copyright         = T.pack . fromShortText $ PkgDescr.copyright pkgd
      pbd_synopsis          = T.pack . fromShortText $ PkgDescr.synopsis pkgd
      pbd_description       = T.pack . fromShortText $ PkgDescr.description pkgd
      pbd_license           = either id licenseToSPDX $
                                PkgDescr.licenseRaw pkgd
      pbd_homepage          = T.pack . fromShortText $ PkgDescr.homepage pkgd
      pbd_metadata_revision = metadataRev
      pbd_uploaded_at       = uploadedAt
      in
      return $ PackageBasicDescription {..}
    (_, Left (_, perrs)) ->
      let errs = List.intersperse '\n' $ mconcat $ for (toList perrs) $ \err -> Parsec.showPError "" err
       in Left $ "Could not parse cabal file: "
                   <> errs

basicDescriptionToDTO :: UserName -> PackageBasicDescription -> PackageBasicDescriptionDTO
basicDescriptionToDTO uploader d =
  PackageBasicDescriptionDTO
    { license = d.pbd_license
    , copyright = d.pbd_copyright
    , synopsis = d.pbd_synopsis
    , description = d.pbd_description
    , author = d.pbd_author
    , homepage = d.pbd_homepage
    , metadata_revision = d.pbd_metadata_revision
    , uploaded_at = d.pbd_uploaded_at
    , uploader
    }

-- | Get a JSON @PackageBasicDescription@ for a particular
--   package/version/metadata-revision
--      OR
--   A listing of versions and their deprecation states
servePackageBasicDescription
  :: CoreResource
  -> UserFeature
  -> Preferred.VersionsFeature
  -> Framework.DynamicPath
     -- ^ URI specifying a package and version `e.g. lens or lens-4.11`
  -> Framework.ServerPartE Framework.Response
servePackageBasicDescription resource userFeature preferred dpath = do

  let metadataRev :: Maybe Int = lookup "revision" dpath >>= Framework.fromReqURI

  pkgid@(PackageIdentifier name version) <- packageInPath resource dpath
  guardValidPackageName resource name

  if version /= nullVersion
    then fetchDescr pkgid metadataRev
    else Framework.toResponse . Aeson.toJSON <$> getVersionListing name

  where

    fetchDescr
      :: PackageIdentifier
      -> Maybe Int
      -> Framework.ServerPartE Framework.Response
    fetchDescr pkgid metadataRev = do
      guardValidPackageId resource pkgid
      pkg <- lookupPackageId resource pkgid

      let metadataRevs = fst <$> pkgMetadataRevisions pkg
          uploadInfos  = snd <$> pkgMetadataRevisions pkg
          nMetadata    = Vector.length metadataRevs
          metadataInd  = fromMaybe (nMetadata - 1) metadataRev
      descr <- getPackageDescr metadataInd nMetadata metadataRevs uploadInfos
      return $ Framework.toResponse $ Aeson.toJSON descr

    getPackageDescr metadataInd nMetadata metadataRevs uploadInfos = do
      when (metadataInd < 0 || metadataInd >= nMetadata)
        (Framework.errNotFound "Revision not found"
         [Framework.MText
           $ "There are " <> show nMetadata <> " metadata revisions. Index "
           <> show metadataInd <> " is out of bounds."]
        )

      let cabalFile = metadataRevs Vector.! metadataInd
          uploadedAt = fst $ uploadInfos Vector.! metadataInd
          uploaderId = snd $ uploadInfos Vector.! metadataInd
      uploader <- userName <$> lookupUserInfo userFeature uploaderId
      let pkgDescr  = getBasicDescription uploadedAt cabalFile metadataInd
      case pkgDescr of
        Left e  -> Framework.errInternalError [Framework.MText e]
        Right d -> do
          let packageInfoDTO =  basicDescriptionToDTO uploader d
          return packageInfoDTO

    getVersionListing name = do
      pkgs <- lookupPackageName resource name
      prefInfo <- Preferred.queryGetPreferredInfo preferred name
      return
        . PackageVersions
        . Preferred.classifyVersions prefInfo
        $ fmap packageVersion pkgs
