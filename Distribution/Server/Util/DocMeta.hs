{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Distribution.Server.Util.DocMeta (
    DocMeta(..)
  , QuickJumpVersion
  , loadTarDocMeta
  , parseDocMeta
  , packageDocMetaTarPath
  ) where

import Control.Monad (mzero)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy
import Data.TarIndex (TarIndex)
import qualified Data.TarIndex as TarIndex
import qualified Data.Text as Text
import Distribution.Package
import Distribution.Text (display, simpleParse)
import Distribution.Version (Version)
import System.FilePath ((</>))

import Distribution.Server.Util.ServeTarball (loadTarEntry)

newtype JsonVersion = JV Version

instance FromJSON JsonVersion where
  parseJSON = withText "Version" $ \s ->
    case simpleParse (Text.unpack s) of
      Just version -> return (JV version)
      Nothing      -> mzero

type QuickJumpVersion = Int

data DocMeta = DocMeta {
  docMetaHaddockVersion   :: !Version,
  docMetaQuickJumpVersion :: !(Maybe QuickJumpVersion)
}

instance FromJSON DocMeta where
  parseJSON = withObject "DocMeta" $ \o -> do
    JV docMetaHaddockVersion <- o .:  "haddock_version"
    docMetaQuickJumpVersion  <- o .:? "quickjump_version"
    return DocMeta{..}

loadTarDocMeta :: MonadIO m => FilePath -> TarIndex -> PackageId -> m (Maybe DocMeta)
loadTarDocMeta tarball docIndex pkgid =
  case TarIndex.lookup docIndex docMetaPath of
    Just (TarIndex.TarFileEntry docMetaEntryOff) -> do
      docMetaEntryContent <- liftIO (loadTarEntry tarball docMetaEntryOff)
      case docMetaEntryContent of
        Right (_, docMetaContent) ->
          return (parseDocMeta docMetaContent)
        Left _ -> return Nothing
    _ -> return Nothing
  where
    docMetaPath = packageDocMetaTarPath pkgid

parseDocMeta :: Data.ByteString.Lazy.ByteString -> Maybe DocMeta
parseDocMeta = decode

packageDocMetaTarPath :: PackageId -> FilePath
packageDocMetaTarPath pkgid =
  display pkgid ++ "-docs" </> "meta.json"
