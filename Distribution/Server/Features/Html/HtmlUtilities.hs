{-# LANGUAGE RecursiveDo, FlexibleContexts, RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.Html.HtmlUtilities (
  HtmlUtilities(..),
  htmlUtilities
  ) where

import Text.XHtml.Strict
import qualified Data.Set as Set
import Distribution.Server.Features.Tags
import Distribution.Server.Features.Core
import Distribution.Text (display)
import Data.List (intersperse, intercalate)
import Data.Set (Set)
import Data.Maybe (fromMaybe)
import Distribution.Server.Features.PackageList
import Distribution.Server.Pages.Util (packageType)
import Distribution.Package

data HtmlUtilities = HtmlUtilities {
    packageLink :: PackageId -> Html
  , packageNameLink :: PackageName -> Html
  , renderItem :: PackageItem -> Html
  , makeRow :: PackageItem -> Html
  , renderTags :: Set Tag -> [Html]
  , renderReviewTags :: Set Tag -> (Set Tag, Set Tag) -> PackageName -> [Html]
  , renderDeps :: PackageName -> [PackageName] -> Html
  }

htmlUtilities :: CoreFeature -> TagsFeature -> HtmlUtilities
htmlUtilities CoreFeature{coreResource}
              TagsFeature{tagsResource} = HtmlUtilities{..}
  where
    packageLink :: PackageId -> Html
    packageLink pkgid = anchor ! [href $ corePackageIdUri cores "" pkgid] << display pkgid

    packageNameLink :: PackageName -> Html
    packageNameLink pkgname = anchor ! [href $ corePackageNameUri cores "" pkgname] << display pkgname

    makeRow :: PackageItem -> Html
    makeRow item = tr << [ td $ packageNameLink $ itemName item
                         , td $ toHtml $ show $ itemDownloads item
                         , td $ toHtml $ show $ itemVotes item
                         , td $ toHtml $ itemDesc item
                         , td $ toHtml $ show $ itemRevDepsCount item
                         , td $ " (" +++ renderTags (itemTags item) +++ ")"
                         , td $ toHtml $ itemMaintainer item
                         ]


    renderItem :: PackageItem -> Html
    renderItem item = li ! classes <<
          [ packageNameLink pkgname
          , toHtml $ " " ++ ptype
                         ++ ": " ++ itemDesc item
          , " (" +++ renderTags (itemTags item) +++ ")"
          ]
      where pkgname = itemName item
            ptype = packageType (itemHasLibrary item) (itemNumExecutables item)
                                (itemNumTests item) (itemNumBenchmarks item)
            classes = case classList of [] -> []; _ -> [theclass $ unwords classList]
            classList = (case itemDeprecated item of Nothing -> []; _ -> ["deprecated"])

    renderTags :: Set Tag -> [Html]
    renderTags tags = intersperse (toHtml ", ")
        (map (\tg -> anchor ! [href $ tagUri tagsResource "" tg] << display tg)
          $ Set.toList tags)

    -- The page displayed at /package/:package/tags
    renderReviewTags :: Set Tag -> (Set Tag, Set Tag) -> PackageName -> [Html]
    renderReviewTags currTags revTags pkgname=
        let toStr = concat . intersperse ", " . map display . Set.toList
            tagsStr = toStr currTags
            addns = toStr $ fst revTags
            delns = toStr $ snd revTags
            disp = thediv ! [theclass "box"] << [ paragraph << [bold $ toHtml "Current Tags: ", toHtml tagsStr, br]
                                                , paragraph << [bold $ toHtml "Additions to be reviewed: ", toHtml $ if (addns /= "") then addns else "None", br]
                                                , paragraph << [bold $ toHtml "Deletions to be reviewed: ", toHtml $ if (delns /= "") then delns else "None", br]
                                                ]
        in
            [ big $ bold $ toHtml $ display pkgname
            , disp
            , anchor ![href $ "tags/edit" ] << "Propose a tag?", toHtml " or "
            , toHtml "return to ", packageNameLink pkgname, br
            ]
    renderDeps :: PackageName -> [PackageName] -> Html
    renderDeps pkg deps = summary +++ detailsLink
      where
        summary = intersperse (toHtml ", ") $ map packageNameLink deps
        detailsLink = thespan ! [thestyle "font-size: small"]
                        << (" [" +++ anchor ! [href detailURL] << "details" +++ "]")
        detailURL = "/package/" ++ (unPackageName pkg) ++ "/reverse"

    cores = coreResource
