{-# LANGUAGE RecordWildCards, NamedFieldPuns, TypeApplications #-}
module Distribution.Server.Features.Browse.ApplyFilter (applyFilter) where

import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time.Clock (UTCTime(utctDay), diffUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)

import Data.Aeson (Value, (.=), object)
import qualified Data.Text as T
import qualified Data.Set as S

import Distribution.Server.Features.Browse.Options (BrowseOptions(..), Direction(..), Column(..), Sort(..), NormalColumn(..), IsSearch(..))
import Distribution.Server.Features.Browse.Parsers (DeprecatedOption(..), Filter(..), operatorToFunction)
import Distribution.Server.Features.Core (CoreResource, corePackageNameUri)
import Distribution.Server.Features.PackageList(PackageItem(..))
import Distribution.Server.Features.Tags (Tag(..), TagsResource, tagUri)
import Distribution.Server.Features.Users (UserResource, userPageUri)
import Distribution.Server.Users.Types (UserName)
import Distribution.Text (display)

applyFilter :: UTCTime -> IsSearch -> CoreResource -> UserResource -> TagsResource -> BrowseOptions -> [PackageItem] -> [Value]
applyFilter now isSearch coreResource userResource tagsResource browseOptions items =
 map packageIndexInfoToValue $
 sort isSearch (boSort browseOptions) $
 filterPackages now (boFilters browseOptions) $
 items
 where
  packageIndexInfoToValue :: PackageItem -> Value
  packageIndexInfoToValue PackageItem{..} =
    object
      [ T.pack "name" .= renderPackage itemName
      , T.pack "downloads" .= itemDownloads
      , T.pack "votes" .= itemVotes
      , T.pack "description" .= itemDesc
      , T.pack "tags" .= map renderTag (S.toAscList itemTags)
      , T.pack "lastUpload" .= iso8601Show itemLastUpload
      , T.pack "maintainers" .= map renderUser itemMaintainer
      ]
  renderTag :: Tag -> Value
  renderTag tag =
    object
      [ T.pack "uri" .= tagUri tagsResource "" tag
      , T.pack "display" .= display tag
      ]
  renderUser :: UserName -> Value
  renderUser user =
    object
      [ T.pack "uri" .= userPageUri userResource "" user
      , T.pack "display" .= display user
      ]
  renderPackage pkg =
    object
      [ T.pack "uri" .= corePackageNameUri coreResource "" pkg
      , T.pack "display" .= display pkg
      ]

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

includeItem :: UTCTime -> PackageItem -> Filter -> Bool
includeItem _   PackageItem{ itemDownloads }  (DownloadsFilter ( op, sndParam))    = operatorToFunction op (fromIntegral @Int @Word itemDownloads) sndParam
includeItem _   PackageItem{ itemVotes }      (RatingFilter (op, sndParam) )       = operatorToFunction op itemVotes sndParam
includeItem _   PackageItem{ itemLastUpload } (LastUploadFilter (op, sndParam))    = operatorToFunction op (utctDay itemLastUpload) sndParam
includeItem _   PackageItem{ itemTags }       (TagFilter tagStr)                   = any (\tag -> display tag == tagStr) itemTags
includeItem _   PackageItem{ itemMaintainer } (MaintainerFilter maintainerStr)     = any (\user -> display user == maintainerStr) itemMaintainer
includeItem now PackageItem{ itemLastUpload } (AgeLastULFilter (op, sndParam))     = operatorToFunction op (diffUTCTime now itemLastUpload) sndParam
includeItem _   PackageItem{ itemDeprecated } (DeprecatedFilter OnlyDeprecated)    = not (null itemDeprecated)
includeItem _   PackageItem{ itemDeprecated } (DeprecatedFilter ExcludeDeprecated) = null itemDeprecated
includeItem _   _                             (DeprecatedFilter Don'tCareAboutDeprecated) = True
includeItem now packageItem (Not filt) = not (includeItem now packageItem filt)

filterPackages :: UTCTime -> [Filter] -> [PackageItem] -> [PackageItem]
filterPackages now filtersWithoutDefaults = filter filterForItem
  where
    filterForItem :: PackageItem -> Bool
    filterForItem item = all (includeItem now item) filtersWithDefaults
    filtersWithDefaults =
      -- The lack of other filters means we don't care.
      -- But deprecated packages are excluded by default.
      -- So we check if the user has overriden the default filter.
      case [ x | DeprecatedFilter x <- filtersWithoutDefaults ] of
        [] -> DeprecatedFilter ExcludeDeprecated : filtersWithoutDefaults
        _  -> filtersWithoutDefaults
