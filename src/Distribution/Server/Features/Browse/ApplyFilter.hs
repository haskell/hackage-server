{-# LANGUAGE RecordWildCards, NamedFieldPuns, TypeApplications #-}
module Distribution.Server.Features.Browse.ApplyFilter (applyFilter) where

import Control.Monad (filterM)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time.Clock (UTCTime(utctDay), diffUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)

import Data.Aeson (Value, (.=), object)
import qualified Data.Aeson.Key as Key

import qualified Data.Set as S

import Distribution.Server.Features.Browse.Options (BrowseOptions(..), Direction(..), Column(..), Sort(..), NormalColumn(..), IsSearch(..))
import Distribution.Server.Features.Browse.Parsers (DeprecatedOption(..), Filter(..), operatorToFunction)
import Distribution.Server.Features.Core (CoreResource, corePackageNameUri)
import Distribution.Server.Features.Distro (DistroFeature(DistroFeature, queryPackageStatus))
import Distribution.Server.Features.Distro.Types (DistroName(..))
import Distribution.Server.Features.PackageList(PackageItem(..))
import Distribution.Server.Features.Tags (Tag(..), TagsResource, tagUri)
import Distribution.Server.Features.Users (UserResource, userPageUri)
import Distribution.Server.Users.Types (UserName)
import Distribution.Text (display)

applyFilter :: UTCTime -> IsSearch -> CoreResource -> UserResource -> TagsResource -> DistroFeature -> BrowseOptions -> [PackageItem] -> IO [Value]
applyFilter now isSearch coreResource userResource tagsResource DistroFeature{queryPackageStatus} browseOptions items = do
 packages <- filterM filterForItem items
 pure $
   map packageIndexInfoToValue $
   sort isSearch (boSort browseOptions) packages
 where
  packageIndexInfoToValue :: PackageItem -> Value
  packageIndexInfoToValue PackageItem{..} =
    object
      [ Key.fromString "name" .= renderPackage itemName
      , Key.fromString "downloads" .= itemDownloads
      , Key.fromString "votes" .= itemVotes
      , Key.fromString "description" .= itemDesc
      , Key.fromString "tags" .= map renderTag (S.toAscList itemTags)
      , Key.fromString "lastUpload" .= iso8601Show itemLastUpload
      , Key.fromString "maintainers" .= map renderUser itemMaintainer
      ]
  renderTag :: Tag -> Value
  renderTag tag =
    object
      [ Key.fromString "uri" .= tagUri tagsResource "" tag
      , Key.fromString "display" .= display tag
      ]
  renderUser :: UserName -> Value
  renderUser user =
    object
      [ Key.fromString "uri" .= userPageUri userResource "" user
      , Key.fromString "display" .= display user
      ]
  renderPackage pkg =
    object
      [ Key.fromString "uri" .= corePackageNameUri coreResource "" pkg
      , Key.fromString "display" .= display pkg
      ]

  includeItem :: PackageItem -> Filter -> IO Bool
  includeItem PackageItem{ itemDownloads }  (DownloadsFilter ( op, sndParam))    = pure $ operatorToFunction op (fromIntegral @Int @Word itemDownloads) sndParam
  includeItem PackageItem{ itemVotes }      (RatingFilter (op, sndParam) )       = pure $ operatorToFunction op itemVotes sndParam
  includeItem PackageItem{ itemLastUpload } (LastUploadFilter (op, sndParam))    = pure $ operatorToFunction op (utctDay itemLastUpload) sndParam
  includeItem PackageItem{ itemTags }       (TagFilter tagStr)                   = pure $ any (\tag -> display tag == tagStr) itemTags
  includeItem PackageItem{ itemMaintainer } (MaintainerFilter maintainerStr)     = pure $ any (\user -> display user == maintainerStr) itemMaintainer
  includeItem PackageItem{ itemLastUpload } (AgeLastULFilter (op, sndParam))     = pure $ operatorToFunction op (diffUTCTime now itemLastUpload) sndParam
  includeItem PackageItem{ itemDeprecated } (DeprecatedFilter OnlyDeprecated)    = pure $ not (null itemDeprecated)
  includeItem PackageItem{ itemDeprecated } (DeprecatedFilter ExcludeDeprecated) = pure $ null itemDeprecated
  includeItem _                             (DeprecatedFilter Don'tCareAboutDeprecated) = pure True
  includeItem PackageItem{ itemName }       (DistroFilter distroStr)             = elem (DistroName distroStr) . map fst <$> queryPackageStatus itemName
  includeItem packageItem (Not filt) = not <$> includeItem packageItem filt

  filtersWithoutDefaults = boFilters browseOptions

  filtersWithDefaults =
    -- The lack of other filters means we don't care.
    -- But deprecated packages are excluded by default.
    -- So we check if the user has overriden the default filter.
    case [ x | DeprecatedFilter x <- filtersWithoutDefaults ] of
      [] -> DeprecatedFilter ExcludeDeprecated : filtersWithoutDefaults
      _  -> filtersWithoutDefaults

  filterForItem :: PackageItem -> IO Bool
  filterForItem item =
    and <$> traverse (includeItem item) filtersWithDefaults

sort :: IsSearch -> Sort -> [PackageItem] -> [PackageItem]
sort isSearch Sort {sortColumn, sortDirection} =
  case sortColumn of
    DefaultColumn ->
      case isSearch of
        IsSearch -> id
        IsNotSearch -> id
    NormalColumn normalColumn ->
      let
        comparer =
          case normalColumn of
            Name -> comparing itemName
            Downloads -> comparing itemDownloads
            Rating -> comparing itemVotes
            Description -> comparing itemDesc
            Tags -> comparing (S.toAscList . itemTags)
            LastUpload -> comparing itemLastUpload
            Maintainers -> comparing itemMaintainer
       in sortBy (maybeReverse comparer)
  where
    maybeReverse =
      case sortDirection of
        Ascending -> id
        Descending -> flip
