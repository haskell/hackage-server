{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Pages.Reverse (
    ReverseHtmlUtil(..),
    reverseHtmlUtil
  ) where

import Distribution.Server.Features.ReverseDependencies
import Distribution.Server.Features.PreferredVersions


import Distribution.Package
import Distribution.Text (display)
import Distribution.Version

import qualified Data.Map as Map

import Text.XHtml.Strict
data ReverseHtmlUtil = ReverseHtmlUtil {
    reversePackageRender :: PackageId -> (PackageId -> String) -> Bool -> ReversePageRender -> [Html]
  , reverseFlatRender :: PackageName -> (PackageName -> String) -> ReverseCount -> [(PackageName, Int)] -> [Html]
  , reverseStatsRender :: PackageName -> [Version] -> (PackageId -> String) -> ReverseCount -> [Html]
  , reversePackagesRender :: (PackageName -> String) -> Int -> [(PackageName, Int, Int)] -> [Html]
  , reversePackageSummary :: PackageId -> (Int, Int) -> (String, Html)
  }

reverseHtmlUtil :: ReverseFeature -> ReverseHtmlUtil
reverseHtmlUtil ReverseFeature{reverseResource} = ReverseHtmlUtil{..}
  where
    reversePackageRender :: PackageId -- ^ The package whose information is displayed.
                         -> (PackageId -> String) -- ^ Generating URIs for package pages.
                         -> Bool -- ^ Whether the ReverseDisplay was rendered for recent (True) or older (False) versions of the package.
                         -> ReversePageRender -- ^ Obtained from a ReverseDisplay-rendering function.
                         -> [Html]
    reversePackageRender pkgid packageLink isRecent (ReversePageRender renders counts total) =
        let packageAnchor = anchor ! [href $ packageLink pkgid] << display pkgid
            hasVersion = packageVersion pkgid /= Version [] []
            pkgname = packageName pkgid
            statLinks = paragraph <<
              [ toHtml "Check out the "
              , anchor ! [href $ reverseStatsUri reverseResource "" pkgname] << "statistics for specific versions"
              , toHtml $ " of " ++ display pkgid ++ " and its "
              , anchor ! [href $ reverseAllUri reverseResource "" pkgname] << "indirect dependencies", toHtml "." ]
            versionBox = if hasVersion && total /= allCounts
                then thediv ! [theclass "notification"] << [toHtml $ "These statistics only apply to this version of " ++ display pkgname ++ ". See also ",  anchor ! [href $ reverseNameUri reverseResource "" pkgname] << [toHtml "packages which depend on ", emphasize << "any", toHtml " version"], toHtml $ " (all " ++ show total ++ " of them)."]
                else noHtml
            allCounts = uncurry (+) counts
            otherCount = case total - allCounts of
                diff | diff > 0 -> paragraph << [show diff ++ " packages depend on versions of " ++ display pkgid ++ " other than this one."]
                _ -> noHtml
            (pageText, nonPageText) = (if isRecent then id else uncurry $ flip (,)) (recentText, nonRecentText)
            otherLink = if isRecent then reverseOldUri reverseResource "" pkgid else reverseUri reverseResource "" pkgid
        in h2 << (display pkgid ++ ": " ++ num allCounts "reverse dependencies" "reverse dependency"):versionBox:case counts of
        (0, 0) ->
          [ paragraph << [toHtml "No packages depend on ",
                          packageAnchor, toHtml "."]
          ]
        (0, count) ->
          paragraph << [toHtml "No packages depend on ",
                          if hasVersion then noHtml else toHtml "some version of ",
                          packageAnchor,
                          toHtml $ pageText 0 ++ " However, ",
                          altVersions count nonPageText otherLink,
                          toHtml "."] : [otherCount, statLinks]
        (count, count') ->
          [ (paragraph<<) $ [ mainVersions count pageText packageAnchor, toHtml " (listed below)." ]
            ++ if count' > 0 then [ toHtml " Additionally, "
                                  , altVersions count' nonPageText otherLink
                                  , toHtml $ ". That's " ++ show (count+count') ++ " in total." ]
                             else []
          ] ++ (if isRecent then [] else [paragraph << oldText]) ++ [otherCount, statLinks, reverseTable]
      where
        mainVersions count textFunc pkgLink = toHtml
           [ toHtml $ num count "packages depend on " "package depends on "
           , pkgLink
           , toHtml $ textFunc count
           ]
        altVersions count textFunc altLink = toHtml
           [ anchor ! [href altLink] << num count "packages" "package"
           , toHtml  $ num' count " depend on " " depends on " ++ display pkgid ++ textFunc count
           ]
        recentText count    = ' ':num' count "in their latest versions" "in its latest version"
        nonRecentText count = ' ':num' count "only in older or deprecated versions" "only in an older or deprecated version"
        oldText = "The latest version of each package below, which doesn't depend on " ++ display pkgid ++ ", is linked from the first column. The version linked from the second column is the one which has a dependency on " ++ display pkgid ++", but it's no longer the preferred installation candidate. Note that packages which depend on versions of " ++ display pkgid ++ " not uploaded to Hackage are treated as not depending on it at all."

        reverseTable = thediv << table << reverseTableRows
        reverseTableRows =
            tr ! [theclass "fancy"] << [ th << "Package name", th << "Version", th << "Reverse dependencies" ] :
            [ tr ! [theclass (if odd n then "odd" else "even")] <<
                [ td << anchor ! [href $ packageLink $ PackageIdentifier (packageName pkg) $ Version [] [] ] << display (packageName pkg)
                , td << anchor ! (renderStatus status ++ [href $ packageLink pkg]) << display (packageVersion pkg)
                , td << [ toHtml $ (show count) ++ " (", anchor ! [href $ reverseNameUri reverseResource "" $ packageName pkg] << "view", toHtml ")" ] ]
            | (ReverseRender pkg status count, n) <- zip renders [(1::Int)..] ]

        renderStatus (Just DeprecatedVersion) = [theclass "deprecated"]
        renderStatus (Just UnpreferredVersion) = [theclass "unpreferred"]
        renderStatus _ = []

    reverseFlatRender :: PackageName -> (PackageName -> String) -> ReverseCount -> [(PackageName, Int)] -> [Html]
    reverseFlatRender pkgname packageLink  (ReverseCount total flat _) pairs =
      h2 << (display pkgname ++ ": " ++ num flat "total reverse dependencies" "reverse dependency"):case (total, flat) of
        (0, 0) -> [paragraph << [toHtml "No packages depend on ", toPackage pkgname]]
        _ ->
          [ paragraph << if total == flat
            then [toHtml "All packages which use ", toPackage pkgname, toHtml " depend on it ", anchor ! [href $ reverseNameUri reverseResource "" pkgname] << "directly", toHtml $ ". " ++ onlyPackage total]
            else [toPackage pkgname, toHtml " has ", anchor ! [href $ reverseNameUri reverseResource "" pkgname] << num total "packages" "package", toHtml $ " which directly " ++ num' total "depend" "depends" ++ " on it, but there are more packages which depend on ", emphasize << "those", toHtml $ " packages. If you flatten the tree of reverse dependencies, you'll find " ++ show flat ++ " packages which use " ++ display pkgname ++ ", and " ++ show (flat-total) ++ " which do so without depending directly on it. All of these packages are listed below."]
          , paragraph << [toHtml "See also the ", anchor ! [href $ reverseStatsUri reverseResource "" pkgname] << "statistics for specific versions", toHtml $ " of " ++ display pkgname ++ "."]
          , reverseTable
          ]
      where
        toPackage pkg = anchor ! [href $ packageLink pkg] << display pkg

        onlyPackage count = if count == 1 then "There's only one:" else "There are " ++ show count ++ ":"

        reverseTable = thediv << table << reverseTableRows
        reverseTableRows =
            (tr  ! [theclass "fancy"] << [ th << "Package name", th << "Total reverse dependencies" ]) :
            [ tr ! [theclass (if odd n then "odd" else "even")] <<
                [ td << toPackage pkg
                , td << [ toHtml $ (show count) ++ " (", anchor ! [href $ reverseAllUri reverseResource "" pkg] << "view", toHtml ")" ]
                ]
            | ((pkg, count), n) <- zip pairs [(1::Int)..] ]

    -- /package/:package/reverse/summary
    reverseStatsRender :: PackageName -> [Version] -> (PackageId -> String) -> ReverseCount -> [Html]
    reverseStatsRender pkgname allVersions packageLink (ReverseCount total flat versions) =
        h2 << (display pkgname ++ ": reverse dependency statistics"):
      [ case total of
            0 -> paragraph << [ toHtml "No packages depend on ", thisPackage, toHtml "." ]
            _ -> toHtml
                [ paragraph << [ anchor ! [href $ reverseNameUri reverseResource "" pkgname] << num total "packages" "package"
                               , toHtml $ num' total " depend" " depends"
                               , toHtml " directly on ", thisPackage, toHtml "." ]
                , paragraph << [ toHtml $ num (flat-total) "packages depend" "package depends" ++ " indirectly on " ++ display pkgname ++ "." ]
                , paragraph << [ anchor ! [href $ reverseAllUri reverseResource "" pkgname] << num flat "packages" "package"
                               , toHtml $ num' flat " depend" " depends" ++ " on " ++ display pkgname ++ " in total."
                               ]
                ]
      , versionTable ]
      where
        toPackage pkgid = anchor ! [href $ packageLink pkgid] << display pkgid
        thisPackage = toPackage (PackageIdentifier pkgname $ Version [] [])

        versionTable = thediv << table << versionTableRows
        versionTableRows =
            (tr ! [theclass "fancy"] << [ th << "Version", th << "Reverse dependency count" ]) :
            [ tr ! [theclass (if odd n then "odd" else "even")] <<
                [ td << anchor ! [href $ packageLink pkgid ] << display version
                , td << [ toHtml $ show (Map.findWithDefault 0 version versions) ++ " ("
                        , anchor ! [href $ reverseUri reverseResource "" pkgid] << "view", toHtml ")" ]
                ]
            | (version, n) <- zip allVersions [(1::Int)..], let pkgid = PackageIdentifier pkgname version ]

    num, num' :: Int -> String -> String -> String
    num  n plural singular  = show n ++ " " ++ num' n plural singular
    num' n plural singular = if n == 1 then singular else plural

    -- /packages/reverse
    reversePackagesRender :: (PackageName -> String) -> Int -> [(PackageName, Int, Int)] -> [Html]
    reversePackagesRender packageLink pkgCount triples =
            h2 << "Reverse dependencies" :
          [ paragraph << [ "Hackage has " ++ show pkgCount ++ " packages. Here are all the packages that have package that depend on them:"]
          , reverseTable ]
      where
        reverseTable = thediv << table << reverseTableRows
        reverseTableRows =
            (tr ! [theclass "fancy"] << [ th << "Package name", th << "Total", th << "Direct" ]) :
            [ tr ! [theclass (if odd n then "odd" else "even")] <<
                [ td << anchor ! [href $ packageLink pkgname ] << display pkgname
                , td << [ toHtml $ show flat ++ " (", anchor ! [href $ reverseStatsUri reverseResource "" pkgname ] << "view", toHtml ")" ]
                , td << [ toHtml $ show total ++ " (", anchor ! [href $ reverseNameUri reverseResource "" pkgname ] << "view", toHtml ")" ]
                ] -- and, indirect is flat-total, if those are ever explicitly served
            | ((pkgname, total, flat), n) <- zip triples [(1::Int)..], flat /= 0 ]

    reversePackageSummary :: PackageId -> (Int, Int) -> (String, Html)
    reversePackageSummary pkgid (direct, version) = (,) "Reverse dependencies" $
      if direct == 0
        then toHtml "None"
        else toHtml [ anchor ! [href $ reverseUri reverseResource "" pkgid ] << show version
                    , toHtml " for ", toHtml . display $ packageVersion pkgid
                    , toHtml " and "
                    , anchor ! [href $ reverseNameUri reverseResource "" (packageName pkgid) ] << show direct
                    , toHtml " total"]
