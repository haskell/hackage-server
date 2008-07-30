-- Takes a reversed log file on the standard input and outputs web page.

module Distribution.Server.Pages.Recent (
    recentPage,
--    recentFeed,
  ) where

import Distribution.Server.Types
         ( PkgInfo(..) )
import Distribution.Server.Pages.Template
         ( hackagePageWith )

import Distribution.Package
         ( PackageIdentifier )
import Distribution.Text
         ( display )

import Text.XHtml
import Data.Time.Format
         ( formatTime )
import System.Locale
         ( defaultTimeLocale )

-- | Takes a list of package info, in reverse order by timestamp.
--
recentPage :: [PkgInfo] -> Html
recentPage pkgs =
  let log_rows = map makeRow (take 20 pkgs)
      docBody = [h2 << "Recent additions",
	  table ! [align "center"] << log_rows]
      rss_link = thelink ! [rel "alternate",
                            thetype "application/rss+xml",
                            title "HackageDB RSS Feed",
                            href rssFeedURL] << noHtml
   in hackagePageWith [rss_link] "recent additions" docBody

makeRow :: PkgInfo -> Html
makeRow PkgInfo { pkgInfoId = pkgid
                , pkgUploadTime = time, pkgUploadUser = user } =
  tr <<
    [td ! [align "right"] <<
	    [toHtml (formatTime defaultTimeLocale "%c" time), nbsp, nbsp],
     td ! [align "left"] << user,
     td ! [align "left"] <<
	    [nbsp, nbsp, anchor ! [href (packageURL pkgid)] << display pkgid]]
  where nbsp = primHtmlChar "nbsp"

-- | URL describing a package.
packageURL :: PackageIdentifier -> URL
packageURL pkgid = "/packages/" ++ display pkgid

rssFeedURL :: URL
rssFeedURL = "/packages/recent.rss"
