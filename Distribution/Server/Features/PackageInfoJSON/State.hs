{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Distribution.Server.Features.PackageInfoJSON.State where

import           Control.Arrow        (first, second)
import           Control.Applicative  ((<|>))
import           Control.Monad.Reader (ask, asks)
import qualified Control.Monad.State  as State
import qualified Data.Aeson           as Aeson
import           Data.Aeson           ((.=), (.:))
import           Data.Acid            (Query, Update, makeAcidic)
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.Map.Strict      as Map
import           Data.Monoid          (Sum(..))
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           Data.SafeCopy        (SafeCopy(..), base, contain,
                                       deriveSafeCopy)
import           Data.Serialize       (Get, get, getListOf, getTwoOf, put,
                                       putListOf, putTwoOf)
import           Data.Typeable        (Typeable)
import           Data.Word            (Word8)
import           Distribution.License (licenseToSPDX)
import           Distribution.Text    (display, simpleParse)
import           GHC.Generics         (Generic)

import           Distribution.SPDX.License (License)
import           Distribution.Package      (PackageIdentifier, PackageName)
import           Distribution.Version      (Version, mkVersion, versionNumbers)
import qualified Distribution.Pretty       as Pretty
import qualified Distribution.Parsec.Class as Parsec

import qualified Distribution.Server.Features.PreferredVersions as Preferred
import           Distribution.Server.Framework.MemSize          (MemSize,
                                                                 memSize,
                                                                 memSize7)


-- | Basic information about a package. These values are
--   used in the `/package/:packagename` JSON endpoint
data PackageBasicDescription = PackageBasicDescription
  { pbd_license           :: !License
  , pbd_copyright         :: !T.Text
  , pbd_synopsis          :: !T.Text
  , pbd_description       :: !T.Text
  , pbd_author            :: !T.Text
  , pbd_homepage          :: !T.Text
  , pbd_metadata_revision :: !Int
  } deriving (Eq, Show, Generic)

instance SafeCopy PackageBasicDescription where

  putCopy PackageBasicDescription{..} = contain $ do
    put (Pretty.prettyShow pbd_license)
    put $ T.encodeUtf8 pbd_copyright
    put $ T.encodeUtf8 pbd_synopsis
    put $ T.encodeUtf8 pbd_description
    put $ T.encodeUtf8 pbd_author
    put $ T.encodeUtf8 pbd_homepage
    put pbd_metadata_revision

  getCopy = contain $ do
    licenseStr <- get
    case Parsec.eitherParsec licenseStr of
      Left e -> fail $ unwords ["Could not parse", licenseStr, "as license:" , e]
      Right pbd_license -> do
        pbd_copyright         <- T.decodeUtf8 <$> get
        pbd_synopsis          <- T.decodeUtf8 <$> get
        pbd_description       <- T.decodeUtf8 <$> get
        pbd_author            <- T.decodeUtf8 <$> get
        pbd_homepage          <- T.decodeUtf8 <$> get
        pbd_metadata_revision <- get
        return PackageBasicDescription{..}


-- | Aeson instances are used for building the package-description
--   endpoint. Any changes will impact the API endpoint.
instance Aeson.ToJSON PackageBasicDescription where
  toJSON PackageBasicDescription {..} =
    Aeson.object
      [ T.pack "license"           .= Pretty.prettyShow pbd_license
      , T.pack "copyright"         .= pbd_copyright
      , T.pack "synopsis"          .= pbd_synopsis
      , T.pack "description"       .= pbd_description
      , T.pack "author"            .= pbd_author
      , T.pack "homepage"          .= pbd_homepage
      , T.pack "metadata_revision" .= pbd_metadata_revision
      ]


instance Aeson.FromJSON PackageBasicDescription where
  parseJSON = Aeson.withObject "PackageBasicDescription" $ \obj -> do
    pbd_version'     <- obj .: T.pack "license"
    let parseEitherLicense t =
          Parsec.simpleParsec t <|> fmap licenseToSPDX (simpleParse t)
    case parseEitherLicense pbd_version' of
      Nothing -> fail $ concat ["Could not parse version: \"", pbd_version', "\""]
      Just pbd_license -> do
        pbd_copyright         <- obj .: T.pack "copyright"
        pbd_synopsis          <- obj .: T.pack "synopsis"
        pbd_description       <- obj .: T.pack "description"
        pbd_author            <- obj .: T.pack "author"
        pbd_homepage          <- obj .: T.pack "homepage"
        pbd_metadata_revision <- obj .: T.pack "metadata_revision"
        return $
          PackageBasicDescription {..}

-- | An index of versions for one hackage package
--   and their preferred/deprecated status
newtype PackageVersions = PackageVersions {
  unPackageVersions :: [(Version, Preferred.VersionStatus)]
  } deriving (Eq, Show)

instance SafeCopy PackageVersions where

  putCopy (PackageVersions vs) =
    contain
    $ putListOf (putTwoOf put put)
    $ first versionNumbers . second statusTag <$> vs
    where
      statusTag = \case
        Preferred.NormalVersion      -> 0 :: Word8
        Preferred.DeprecatedVersion  -> 1
        Preferred.UnpreferredVersion -> 2

  getCopy = contain $
    fmap PackageVersions $ getListOf $ getTwoOf getVersion getStatus
    where
      getVersion = mkVersion <$> getListOf get
      getStatus  = (get :: Get Word8) >>= \case
        0 -> return Preferred.NormalVersion
        1 -> return Preferred.DeprecatedVersion
        2 -> return Preferred.UnpreferredVersion
        n -> fail $ "Unsupported tag for VersionStatus: " ++ show n


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


instance Aeson.FromJSON PackageVersions where
  parseJSON = Aeson.withObject "PackageVersions" $ \obj ->
    fmap PackageVersions
    $ traverse (parsePair)
    $ HashMap.toList obj
    where
      parsePair (vStr, vStatus) =
        (,) <$> parseVersion vStr <*> parseStatus vStatus

      parseVersion verText =
        let verString = T.unpack verText
        in case simpleParse verString of
             Just ver -> return ver
             Nothing  -> fail $ concat ["Could not parse \""
                                       , verString ++ "\" as Version. "
                                       , "expected \"a.b.c\" form"]

      parseStatus (Aeson.String s) = case T.unpack s of
        "normal"      -> return Preferred.NormalVersion
        "deprecated"  -> return Preferred.DeprecatedVersion
        "unpreferred" -> return Preferred.UnpreferredVersion
        other         -> fail $ concat ["Could not parse \"" ++ other
                         ++ "\" as status. Expected \"normal\""
                         ++ "\"deprecated\" or \"unpreferred\""]
      parseStatus _   = fail "Expected a string"

data PackageInfoState = PackageInfoState {
    descriptions          :: !(Map.Map (PackageIdentifier, Maybe Int) PackageBasicDescription)
  , versions              :: !(Map.Map PackageName PackageVersions)
  , migratedEphemeralData :: Bool
  } deriving (Show, Typeable, Eq)

getDescriptionFor
  :: (PackageIdentifier, Maybe Int)
  -> Query PackageInfoState (Maybe PackageBasicDescription)
getDescriptionFor pkgId = asks $ Map.lookup pkgId . descriptions

getVersionsFor
  :: PackageName
  -> Query PackageInfoState (Maybe PackageVersions)
getVersionsFor pkgName = asks $ Map.lookup pkgName . versions

setDescriptionFor
  :: (PackageIdentifier, Maybe Int)
  -> Maybe PackageBasicDescription
  -> Update PackageInfoState ()
setDescriptionFor  pkgId descr = State.modify $ \p ->
  case descr of
    Just d  -> p {descriptions = Map.alter (const (Just d)) pkgId (descriptions p)}
    Nothing -> p {descriptions = Map.filterWithKey (\pkgId' _ -> fst pkgId' /= fst pkgId) (descriptions p)}

setVersionsFor
  :: PackageName
  -> Maybe PackageVersions
  -> Update PackageInfoState ()
setVersionsFor  pkgName vs = State.modify $ \p ->
  p { versions = Map.alter (const vs) pkgName (versions p) }

getPackageInfo :: Query PackageInfoState PackageInfoState
getPackageInfo = ask

replacePackageInfo :: PackageInfoState -> Update PackageInfoState ()
replacePackageInfo = State.put

makeAcidic ''PackageInfoState ['getDescriptionFor
                              ,'getVersionsFor
                              ,'setDescriptionFor
                              ,'setVersionsFor
                              ,'getPackageInfo
                              ,'replacePackageInfo
                              ]

deriveSafeCopy 0 'base ''PackageInfoState

instance MemSize PackageBasicDescription where
  memSize PackageBasicDescription{..} =
    memSize7 (Pretty.prettyShow pbd_license) pbd_copyright pbd_synopsis
             pbd_description pbd_author pbd_homepage pbd_metadata_revision

instance MemSize PackageVersions where
  memSize (PackageVersions ps) = getSum $
    foldMap (\(v,_) -> Sum (memSize v) `mappend` Sum (memSize (0 :: Word))) ps

instance MemSize PackageInfoState where
  memSize (PackageInfoState {..}) = memSize descriptions + memSize versions


initialPackageInfoState :: Bool -> PackageInfoState
initialPackageInfoState freshDB = PackageInfoState
  { descriptions          = mempty
  , versions              = mempty
  , migratedEphemeralData = freshDB
  }
