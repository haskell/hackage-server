-- Takes a reversed log file on the standard input and outputs web page.

module Distribution.Server.Pages.Recent (
    recentPage,
    recentFeed,
  ) where

import Distribution.Server.Types
         ( PkgInfo(..) )
import Distribution.Server.Pages.Template
         ( hackagePageWith )

import Distribution.Package
         ( PackageIdentifier, packageName, packageVersion )
import Distribution.PackageDescription
         ( GenericPackageDescription(packageDescription)
         , PackageDescription(synopsis)  )
import Distribution.Text
         ( display )

import qualified Text.XHtml as XHtml
import Text.XHtml
         ( Html, URL, (<<), (!) )
import qualified Text.RSS as RSS
import Text.RSS
         ( RSS(RSS) )
import Network.URI
         ( URI(..), URIAuth(..), uriToString )
import Data.Time.Clock
         ( UTCTime )
import Data.Time.Clock.POSIX
         ( utcTimeToPOSIXSeconds )
import Data.Time.Format
         ( formatTime )
import System.Locale
         ( defaultTimeLocale )
import System.Time --sadly have to use the old-time lib as well as new :-(
         ( CalendarTime, ClockTime(TOD), toUTCTime )

-- | Takes a list of package info, in reverse order by timestamp.
--
recentPage :: [PkgInfo] -> Html
recentPage pkgs =
  let log_rows = map makeRow (take 20 pkgs)
      docBody = [XHtml.h2 << "Recent additions",
	  XHtml.table ! [XHtml.align "center"] << log_rows]
      rss_link = XHtml.thelink ! [XHtml.rel "alternate",
                                  XHtml.thetype "application/rss+xml",
                                  XHtml.title "HackageDB RSS Feed",
                                  XHtml.href rssFeedURL] << XHtml.noHtml
   in hackagePageWith [rss_link] "recent additions" docBody

makeRow :: PkgInfo -> Html
makeRow PkgInfo { pkgInfoId = pkgid
                , pkgUploadTime = time, pkgUploadUser = user } =
  XHtml.tr <<
    [XHtml.td ! [XHtml.align "right"] <<
	    [XHtml.toHtml (showTime time), nbsp, nbsp],
     XHtml.td ! [XHtml.align "left"] << user,
     XHtml.td ! [XHtml.align "left"] <<
	    [nbsp, nbsp, XHtml.anchor !
                           [XHtml.href (packageURL pkgid)] << display pkgid]]
  where nbsp = XHtml.primHtmlChar "nbsp"

convertTime :: UTCTime -> CalendarTime
convertTime utc = toUTCTime (TOD (floor (utcTimeToPOSIXSeconds utc)) 0)

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%c"

-- | URL describing a package.
packageURL :: PackageIdentifier -> URL
packageURL pkgid = "/packages/" ++ display pkgid

rssFeedURL :: URL
rssFeedURL = "/packages/recent.rss"

recentAdditionsURL :: URL
recentAdditionsURL = "/packages/recent.html"

recentFeed :: UTCTime -> [PkgInfo] -> RSS
recentFeed now pkgs = RSS
  "Recent additions"
  (hackageURI recentAdditionsURL)
  desc
  (channel now)
  (map releaseItem (take 20 pkgs))
  where
    desc = "The 20 most recent additions to HackageDB, the Haskell package database."

hackageURI :: String -> URI
hackageURI path =
	URI "http:" (Just (URIAuth "" "hackage.haskell.org" "")) path "" ""

channel :: UTCTime -> [RSS.ChannelElem]
channel now =
  [ RSS.Language "en"
  , RSS.ManagingEditor email
  , RSS.WebMaster email
  , RSS.ChannelPubDate now'
  , RSS.LastBuildDate	now'
  , RSS.Generator "rss-feed"
  ]
  where
    email = "Ross Paterson <ross@soi.city.ac.uk>"
    now'  = convertTime now

releaseItem :: PkgInfo -> [RSS.ItemElem]
releaseItem PkgInfo { pkgInfoId = pkgId, pkgDesc = pkg
                    , pkgUploadTime = time, pkgUploadUser = user } =
  [ RSS.Title title
  , RSS.Link uri
  , RSS.Guid True (uriToString id uri "")
  , RSS.PubDate (convertTime time)
  , RSS.Description desc
  ]
  where
    uri   = hackageURI (packageURL pkgId)
    title = packageName pkgId ++ " " ++ display (packageVersion pkgId)
    body  = synopsis (packageDescription pkg)
    desc  = "<i>Added by " ++ user ++ ", " ++ showTime time ++ ".</i>"
	 ++ if null body then "" else "<p>" ++ body
