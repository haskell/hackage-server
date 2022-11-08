{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
module Distribution.Server.Features.Browse.Options (BrowseOptions(..), Column(..), Direction(..), IsSearch(..), NormalColumn(..), columnToTemplateName, directionToTemplateName, parseSearchQuery) where

import Data.Aeson ((.:), FromJSON(parseJSON), withObject, withText)
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text as T

import Distribution.Server.Features.Browse.Parsers (Filter, conditions, condsToFiltersAndTerms)

data IsSearch = IsSearch | IsNotSearch

data NormalColumn = Name | Downloads | Rating | Description | Tags | LastUpload | Maintainers | PackageRank
  deriving (Show, Eq)

data Column = DefaultColumn | NormalColumn NormalColumn
  deriving (Show, Eq)

data Direction = Ascending | Descending
  deriving (Show, Eq)

data BrowseOptions = BrowseOptions
  { boPage :: Word
  , boSortColumn :: Column
  , boSortDirection :: Direction
  , boSearchQuery :: T.Text
  }

instance FromJSON Column where
  parseJSON =
    withText "Column"
      \case
        "default" -> pure DefaultColumn
        "name" -> pure $ NormalColumn Name
        "downloads" -> pure $ NormalColumn Downloads
        "rating" -> pure $ NormalColumn Rating
        "description" -> pure $ NormalColumn Description
        "tags" -> pure $ NormalColumn Tags
        "lastUpload" -> pure $ NormalColumn LastUpload
        "maintainers" -> pure $ NormalColumn Maintainers
        "packageRank" -> pure $ NormalColumn PackageRank
        t -> fail $ "Column invalid: " ++ T.unpack t

columnToTemplateName :: Column -> String
columnToTemplateName = \case
  DefaultColumn -> "default"
  NormalColumn Name -> "name"
  NormalColumn Downloads -> "downloads"
  NormalColumn Rating -> "rating"
  NormalColumn Description -> "description"
  NormalColumn Tags -> "tags"
  NormalColumn LastUpload -> "lastUpload"
  NormalColumn Maintainers -> "maintainers"
  NormalColumn PackageRank -> "packageRank"

instance FromJSON Direction where
  parseJSON =
    withText "Direction"
      \case
        "ascending" -> pure Ascending
        "descending" -> pure Descending
        t -> fail $ "Direction invalid: " ++ T.unpack t

directionToTemplateName :: Direction -> String
directionToTemplateName = \case
  Ascending -> "ascending"
  Descending -> "descending"

parseSearchQuery :: MonadFail m => T.Text -> m ([Filter], [String])
parseSearchQuery searchQuery = do
  -- Search query parsing should never fail
  Right conds <- pure (parseOnly conditions searchQuery)
  pure (condsToFiltersAndTerms conds)

instance FromJSON BrowseOptions where
  parseJSON = withObject "BrowseOptions" \o ->
    BrowseOptions
      <$> o .: "page"
      <*> o .: "sortColumn"
      <*> o .: "sortDirection"
      <*> o .: "searchQuery"
