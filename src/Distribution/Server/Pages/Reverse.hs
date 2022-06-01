{-# LANGUAGE NamedFieldPuns, RecordWildCards, NamedFieldPuns, BlockArguments #-}
module Distribution.Server.Pages.Reverse (
    ReverseHtmlUtil(..)
  , reverseHtmlUtil
  , LatestOrOld(..)
  ) where

import Distribution.Server.Features.ReverseDependencies
import Distribution.Server.Features.PreferredVersions


import Distribution.Package
import Distribution.Text (display)
import Distribution.Version

import           Data.Function ((&))
import qualified Data.Map as Map
import           Data.Set (Set, toList)

import Text.XHtml.Strict

data LatestOrOld
  = OnlyLatest
  | OnlyOlder
  deriving Eq

data ReverseHtmlUtil = ReverseHtmlUtil {
    reversePackageRender :: PackageId -> (PackageId -> String) -> LatestOrOld -> ReversePageRender -> [Html]
  , reverseFlatRender :: PackageName -> (PackageName -> String) -> ReverseCount -> [(PackageName, Int)] -> [Html]
  , reverseVerboseRender :: PackageName -> [Version] -> (PackageId -> String) -> ReverseCount -> (Map.Map Version (Set PackageIdentifier)) -> [Html]
  , reversePackagesRender :: (PackageName -> String) -> Int -> [(PackageName, ReverseCount)] -> [Html]
  }

reverseHtmlUtil :: ReverseFeature -> ReverseHtmlUtil
reverseHtmlUtil ReverseFeature{reverseResource} = ReverseHtmlUtil{..}
  where
    reversePackageRender :: PackageId -- ^ The package whose information is displayed.
                         -> (PackageId -> String) -- ^ Generating URIs for package pages.
                         -> LatestOrOld -- ^ Whether the ReverseDisplay was rendered for recent (OnlyLatest) or older (OnlyOlder) versions of the package.
                         -> ReversePageRender -- ^ Obtained from a ReverseDisplay-rendering function.
                         -> [Html]
    reversePackageRender pkgid packageLink isRecent (ReversePageRender renders (count, count') total) =
        let allCounts = count + count'
            firstTh = toHtml ("Depend on the " <> (if packageVersion pkgid == nullVersion then "latest" else "given") <> " version")
        in h2 << (display pkgid ++ ": " ++ display allCounts ++ " reverse dependencies"):
          [ if isRecent == OnlyLatest
               then toHtml "No version specified, so showing reverse dependencies for latest version."
               else toHtml ""
          , table ! [ theclass "fancy" ]
                  << [ tr << [ th << firstTh, th << toHtml "Depend on other versions", th << toHtml "Total" ]
                     , tr << [ td << toHtml (display count), td << toHtml (display count'), td << toHtml (display total) ]]
          , reverseTable
          ]
      where
        reverseTable = thediv << table << reverseTableRows
        reverseTableRows =
            tr ! [theclass "fancy"] << [ th << "Package name", th << "Version", th << "Reverse dependencies" ] :
            [ tr ! [theclass (if odd n then "odd" else "even")] <<
                [ td << anchor ! [href $ packageLink $ PackageIdentifier (packageName pkg) $ nullVersion ] << display (packageName pkg)
                , td << anchor ! (renderStatus status ++ [href $ packageLink pkg]) << display (packageVersion pkg)
                , td << [ toHtml $ (show count) ++ " (", anchor ! [href $ reverseFlatUri reverseResource "" $ packageName pkg] << "view", toHtml ")" ] ]
            | (ReverseRender pkg status count, n) <- zip renders [(1::Int)..] ]

        renderStatus (Just DeprecatedVersion) = [theclass "deprecated"]
        renderStatus (Just UnpreferredVersion) = [theclass "unpreferred"]
        renderStatus _ = []

    renderCount ReverseCount{totalCount, directCount} =
      table ! [ theclass "fancy" ]
            << [ tr << [ th << firstTh, th << secondTh, th << toHtml "Total" ]
               , tr << [ td << toHtml (display directCount), td << toHtml (display (totalCount - directCount)), td << toHtml (display totalCount) ]]
      where
        firstTh = toHtml "Direct reverse dependencies"
        secondTh = toHtml "Indirect reverse dependencies"

    reverseFlatRender :: PackageName -> (PackageName -> String) -> ReverseCount -> [(PackageName, Int)] -> [Html]
    reverseFlatRender pkgname packageLink revCount pairs =
      h2 << (display pkgname ++ ": " ++ "total reverse dependencies"):renderCount revCount:[reverseTable]
      where

        toPackage pkg = anchor ! [href $ packageLink pkg] << display pkg

        reverseTable = thediv << table << reverseTableRows
        reverseTableRows =
            (tr  ! [theclass "fancy"] << [ th << "Package name", th << "Total reverse dependencies" ]) :
            [ tr ! [theclass (if odd n then "odd" else "even")] <<
                [ td << toPackage pkg
                , td << [ toHtml $ (show count) ++ " (", anchor ! [href $ reverseFlatUri reverseResource "" pkg] << "view", toHtml ")" ]
                ]
            | ((pkg, count), n) <- zip pairs [(1::Int)..] ]

    -- /package/:package/reverse/verbose
    reverseVerboseRender :: PackageName -> [Version] -> (PackageId -> String) -> ReverseCount -> (Map.Map Version (Set PackageIdentifier)) -> [Html]
    reverseVerboseRender pkgname allVersions packageLink revCount versions =
        h2 << (display pkgname ++ ": reverse dependency statistics"):
      [ renderCount revCount
      , versionTable
      , if length allVersions > limitVersions
           -- Why the oldest? Such that the package can be cached indefinitely without having to get invalidated.
           then thediv << [toHtml "Only showing the oldest ", toHtml (display limitVersions), toHtml " versions."]
           else mempty
      ]

      where
        limitVersions = 10

        versionTable = thediv << (table ! [theclass "fancy"]) << versionTableRows
        versionTableRows =
            (tr << [ th << "Version", th << "Reverse dependencies" ]) :
            [ tr ! [theclass (if odd n then "odd" else "even")] <<
                [ td << anchor ! [href $ packageLink pkgid ] << display version
                , td << [ row ]
                ]
            | (version, n) <- take limitVersions $ zip allVersions [(1::Int)..]
            , let
                pkgid = PackageIdentifier pkgname version
                mkListOfLinks :: Set PackageIdentifier -> [Html]
                mkListOfLinks pkgIdSet =
                  toList pkgIdSet &
                    map
                      \revDepPkgId ->
                        li <<
                          anchor ! [href $ packageLink revDepPkgId] << display revDepPkgId
                row :: Html
                row =
                  Map.lookup version versions &
                    maybe
                      (toHtml "This version has no reverse dependencies.")
                      ((ulist <<) . mkListOfLinks)
            ]

    -- /packages/reverse
    reversePackagesRender :: (PackageName -> String) -> Int -> [(PackageName, ReverseCount)] -> [Html]
    reversePackagesRender packageLink pkgCount namesWithCounts =
            h2 << "Reverse dependencies" :
          [ paragraph << [ "Hackage has " ++ show pkgCount ++ " packages. Here are all the packages that have package that depend on them:"]
          , reverseTable ]
      where
        reverseTable = thediv << table << reverseTableRows
        reverseTableRows =
            (tr ! [theclass "fancy"] << [ th << "Package name", th << "Total", th << "Direct" ]) :
            [ tr ! [theclass (if odd n then "odd" else "even")] <<
                [ td << anchor ! [href $ packageLink pkgname ] << display pkgname
                , td << [ toHtml $ show totalCount ++ " (", anchor ! [href $ reverseVerboseUri reverseResource "" pkgname ] << "view", toHtml ")" ]
                , td << [ toHtml $ show directCount ++ " (", anchor ! [href $ reverseNameUri reverseResource "" pkgname ] << "view", toHtml ")" ]
                ] -- and, indirect is total-direct, if those are ever explicitly served
            | ((pkgname, ReverseCount{directCount, totalCount}), n) <- zip namesWithCounts [(1::Int)..], totalCount /= 0 ]
