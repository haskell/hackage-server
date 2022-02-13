{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
module Distribution.Server.Features.Browse.Options (BrowseOptions(..), Column(..), Direction(..), IsSearch(..), NormalColumn(..), Sort(..)) where

import Data.Aeson ((.:), FromJSON(parseJSON), withObject, withText)
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text as T

import Distribution.Server.Features.Browse.Parsers (Filter, conditions, condsToFiltersAndTerms)

data IsSearch = IsSearch | IsNotSearch

data NormalColumn = Name | Downloads | Rating | Description | Tags | LastUpload | Maintainers
  deriving (Show, Eq)

data Column = DefaultColumn | NormalColumn NormalColumn
  deriving (Show, Eq)

data Direction = Ascending | Descending
  deriving (Show, Eq)

data Sort = Sort
  { sortColumn :: Column
  , sortDirection :: Direction
  }
  deriving (Show, Eq)

data BrowseOptions = BrowseOptions
  { boPage :: Word
  , boSort :: Sort
  , boFilters :: [Filter]
  , boSearchTerms :: [String]
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
        t -> fail $ "Column invalid: " ++ T.unpack t

instance FromJSON Direction where
  parseJSON =
    withText "Direction"
      \case
        "ascending" -> pure Ascending
        "descending" -> pure Descending
        t -> fail $ "Direction invalid: " ++ T.unpack t

instance FromJSON Sort where
  parseJSON = withObject "Sort" \o ->
    Sort
      <$> o .: "column"
      <*> o .: "direction"

parse :: MonadFail m => T.Text -> m ([Filter], [String])
parse searchQuery = do
  -- Search query parsing should never fail
  Right conds <- pure (parseOnly conditions searchQuery)
  pure (condsToFiltersAndTerms conds)

instance FromJSON BrowseOptions where
  parseJSON = withObject "BrowseOptions" \o -> do
    (page, sort, searchQuery) <-
      (,,)
        <$> o .: "page"
        <*> o .: "sort"
        <*> o .: "searchQuery"
    -- The use of monad here won't make us suffer from
    -- sequentiality since the parse should never fail.
    (filters, terms) <- parse searchQuery
    pure (BrowseOptions page sort filters terms)
