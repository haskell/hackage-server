{-# LANGUAGE RecordWildCards, NamedFieldPuns, TypeApplications #-}
module Distribution.Server.Features.Browse.ApplyFilter (applyFilter) where

import Control.Monad (filterM)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Time.Clock (UTCTime(utctDay), diffUTCTime)

import qualified Data.Set as S

import Distribution.Server.Features.Browse.Options (Direction(..), Column(..), NormalColumn(..), IsSearch(..))
import Distribution.Server.Features.Browse.Parsers (DeprecatedOption(..), Filter(..), operatorToFunction)
import Distribution.Server.Features.Distro (DistroFeature(DistroFeature, queryPackageStatus))
import Distribution.Server.Features.Distro.Types (DistroName(..))
import Distribution.Server.Features.PackageList(PackageItem(..))
import Distribution.Text (display)

applyFilter :: UTCTime -> IsSearch -> DistroFeature -> Column -> Direction -> [Filter] -> [PackageItem] -> IO [PackageItem]
applyFilter now isSearch DistroFeature{queryPackageStatus} column direction filtersWithoutDefaults items = do
 packages <- filterM filterForItem items
 pure (sort isSearch column direction packages)
 where

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

sort :: IsSearch -> Column -> Direction -> [PackageItem] -> [PackageItem]
sort isSearch sortColumn sortDirection =
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
            PackageRank -> comparing itemPackageRank
       in sortBy (maybeReverse comparer)
  where
    maybeReverse =
      case sortDirection of
        Ascending -> id
        Descending -> flip
