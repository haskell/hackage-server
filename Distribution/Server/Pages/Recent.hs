-- Takes a reversed log file on the standard input and outputs web page.

module Distribution.Server.Pages.Recent (
    recentPage,
    recentFeed,
  ) where

import Distribution.Server.Packages.Types
         ( PkgInfo(..) )
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Users (Users)
import Distribution.Server.Pages.Template
         ( hackagePageWith )

import Distribution.Package
         ( PackageIdentifier, packageName, packageVersion, PackageName(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(packageDescription)
         , PackageDescription(synopsis)  )
import Distribution.Text
         ( display )

import qualified Text.XHtml.Strict as XHtml
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


-- | Takes a list of package info, in reverse order by timestamp.
--
recentPage :: Users -> [PkgInfo] -> Html
recentPage users pkgs =
  let log_rows = map (makeRow users) (take 20 pkgs)
      docBody = [XHtml.h2 << "Recent additions",
	  XHtml.table ! [XHtml.align "center"] << log_rows]
      rss_link = XHtml.thelink ! [XHtml.rel "alternate",
                                  XHtml.thetype "application/rss+xml",
                                  XHtml.title "HackageDB RSS Feed",
                                  XHtml.href rssFeedURL] << XHtml.noHtml
   in hackagePageWith [rss_link] "recent additions" docBody

makeRow :: Users -> PkgInfo -> Html
makeRow users PkgInfo {
      pkgInfoId = pkgid
    , pkgUploadData = (time, userId)
  } =
  XHtml.tr <<
    [XHtml.td ! [XHtml.align "right"] <<
	    [XHtml.toHtml (showTime time), nbsp, nbsp],
     XHtml.td ! [XHtml.align "left"] << display user,
     XHtml.td ! [XHtml.align "left"] <<
	    [nbsp, nbsp, XHtml.anchor !
                           [XHtml.href (packageURL pkgid)] << display pkgid]]
  where nbsp = XHtml.primHtmlChar "nbsp"
        user = Users.idToName users userId

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%c"

-- | URL describing a package.
packageURL :: PackageIdentifier -> URL
packageURL pkgid = "/package/" ++ display pkgid

rssFeedURL :: URL
rssFeedURL = "/recent.rss"

recentAdditionsURL :: URL
recentAdditionsURL = "/recent.html"

recentFeed :: Users -> URIAuth -> UTCTime -> [PkgInfo] -> RSS
recentFeed users host now pkgs = RSS
  "Recent additions"
  (hackageURI host recentAdditionsURL)
  desc
  (channel now)
  [ releaseItem users host pkg | pkg <- take 20 pkgs ]
  where
    desc = "The 20 most recent additions to HackageDB, the Haskell package database."

hackageURI :: URIAuth -> String -> URI
hackageURI host path =
	URI "http:" (Just host) path "" ""

channel :: UTCTime -> [RSS.ChannelElem]
channel now =
  [ RSS.Language "en"
  , RSS.ManagingEditor email
  , RSS.WebMaster email
  , RSS.ChannelPubDate now
  , RSS.LastBuildDate	now
  , RSS.Generator "rss-feed"
  ]
  where
    email = "duncan@haskell.org (Duncan Coutts)"

releaseItem :: Users -> URIAuth -> PkgInfo -> [RSS.ItemElem]
releaseItem users host PkgInfo {
      pkgInfoId = pkgId
    , pkgDesc = pkg
    , pkgUploadData = (time, userId)
  } =
  [ RSS.Title title
  , RSS.Link uri
  , RSS.Guid True (uriToString id uri "")
  , RSS.PubDate time
  , RSS.Description desc
  ]
  where
    uri   = hackageURI host (packageURL pkgId)
    title = unPackageName (packageName pkgId) ++ " " ++ display (packageVersion pkgId)
    body  = synopsis (packageDescription pkg)
    desc  = "<i>Added by " ++ display user ++ ", " ++ showTime time ++ ".</i>"
	 ++ if null body then "" else "<p>" ++ body
    user = Users.idToName users userId

unPackageName :: PackageName -> String
unPackageName (PackageName name) = name
