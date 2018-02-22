-- Takes a reversed log file on the standard input and outputs web page.

module Distribution.Server.Pages.Recent (
    recentPage,
    recentFeed,
    revisionsPage,
    recentRevisionsFeed
  ) where

import Distribution.Server.Packages.Types
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Users (Users)
import Distribution.Server.Pages.Template
         ( hackagePageWithHead )

import Distribution.Package
         ( PackageIdentifier, packageName, packageVersion )
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
         ( URI(..), uriToString )
import Data.Time.Clock
         ( UTCTime, addUTCTime )
import Data.Time.Format
         ( formatTime )
import Data.Time.Locale.Compat
         ( defaultTimeLocale )

-- | Takes a list of package info, in reverse order by timestamp.
--
recentPage :: Users -> [PkgInfo] -> Html
recentPage users pkgs =
  let log_rows = map (makeRow users) (take 20 pkgs)
      docBody = [XHtml.h2 << "Recent additions",
                 XHtml.table ! [XHtml.align "center"] << log_rows,
                 XHtml.anchor ! [XHtml.href recentRevisionsURL] << XHtml.toHtml "Recent revisions"]
      rss_link = XHtml.thelink ! [XHtml.rel "alternate",
                                  XHtml.thetype "application/rss+xml",
                                  XHtml.title "Hackage RSS Feed",
                                  XHtml.href rssFeedURL] << XHtml.noHtml
   in hackagePageWithHead [rss_link] "recent additions" docBody

revisionsPage :: Users -> [PkgInfo] -> Html
revisionsPage users pkgs =
  let log_rows = map (makeRevisionRow users) (take 40 pkgs)
      docBody = [XHtml.h2 << "Recent cabal metadata revisions",
          XHtml.table ! [XHtml.align "center"] << log_rows]
      rss_link = XHtml.thelink ! [XHtml.rel "alternate",
                                  XHtml.thetype "application/rss+xml",
                                  XHtml.title "Hackage Revisions RSS Feed",
                                  XHtml.href revisionsRssFeedURL] << XHtml.noHtml
   in hackagePageWithHead [rss_link] "recent revisions" docBody

makeRow :: Users -> PkgInfo -> Html
makeRow users pkginfo =
  XHtml.tr <<
    [XHtml.td ! [XHtml.align "right"] <<
            [XHtml.toHtml (showTime time), nbsp, nbsp],
     XHtml.td ! [XHtml.align "left"] << display user,
     XHtml.td ! [XHtml.align "left"] <<
            [nbsp, nbsp, XHtml.anchor !
                           [XHtml.href (packageURL pkgid)] << display pkgid]]
  where
    nbsp = XHtml.primHtmlChar "nbsp"
    user = Users.userIdToName users userId

    (time, userId) = pkgOriginalUploadInfo pkginfo
    pkgid = pkgInfoId pkginfo

makeRevisionRow :: Users -> PkgInfo -> Html
makeRevisionRow users pkginfo =
  XHtml.tr <<
    [XHtml.td ! [XHtml.align "right"] <<
            [XHtml.toHtml (showTime time), nbsp, nbsp],
     XHtml.td ! [XHtml.align "left"] << [XHtml.toHtml ("#" ++ show (pkgNumRevisions pkginfo - 1)), nbsp, nbsp],
     XHtml.td ! [XHtml.align "left"] << display user,
     XHtml.td ! [XHtml.align "left"] <<
                  [nbsp, nbsp, XHtml.anchor !
                           [XHtml.href (packageURL pkgid ++ "/revisions")] << display pkgid]]
  where
    nbsp = XHtml.primHtmlChar "nbsp"
    user = Users.userIdToName users userId

    (time, userId) = pkgLatestUploadInfo pkginfo
    pkgid = pkgInfoId pkginfo

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%c"

-- | URL describing a package.
packageURL :: PackageIdentifier -> URL
packageURL pkgid = "/package/" ++ display pkgid

rssFeedURL :: URL
rssFeedURL = "/recent.rss"

recentAdditionsURL :: URL
recentAdditionsURL = "/recent.html"

revisionsRssFeedURL :: URL
revisionsRssFeedURL = "/packages/recent/revisions.rss"

recentRevisionsURL :: URL
recentRevisionsURL = "/packages/recent/revisions.html"


recentFeed :: Users -> URI -> UTCTime -> [PkgInfo] -> RSS
recentFeed users hostURI now pkgs = RSS
  "Recent additions"
  (hostURI { uriPath = recentAdditionsURL})
  desc
  (channel now)
  (map (releaseItem users hostURI) pkgList)
  where
    desc = "The 20 most recent additions to Hackage (or last 48 hours worth, whichever is greater), the Haskell package database."
    twoDaysAgo = addUTCTime (negate $ 60 * 60 * 48) now
    pkgListTwoDays = takeWhile (\p -> pkgLatestUploadTime p > twoDaysAgo) pkgs
    pkgList = if (length pkgListTwoDays > 20) then pkgListTwoDays else take 20 pkgs


recentRevisionsFeed :: Users -> URI -> UTCTime -> [PkgInfo] -> RSS
recentRevisionsFeed users hostURI now pkgs = RSS
  "Recent revisions"
  (hostURI { uriPath = recentRevisionsURL})
  desc
  (channel now)
  (map (revisionItem users hostURI) pkgList)
  where
    desc = "The 40 most recent revisions to cabal metadata in Hackage (or last 48 hours worth, whichever is greater), the Haskell package database."
    twoDaysAgo = addUTCTime (negate $ 60 * 60 * 48) now
    pkgListTwoDays = takeWhile (\p -> pkgLatestUploadTime p > twoDaysAgo) pkgs
    pkgList = if (length pkgListTwoDays > 40) then pkgListTwoDays else take 40 pkgs

channel :: UTCTime -> [RSS.ChannelElem]
channel now =
  [ RSS.Language "en"
  , RSS.ManagingEditor email
  , RSS.WebMaster email
  , RSS.ChannelPubDate now
  , RSS.LastBuildDate   now
  , RSS.Generator "rss-feed"
  ]
  where
    email = "admin@hackage.haskell.org" --TODO: make this configurable

releaseItem :: Users -> URI -> PkgInfo -> [RSS.ItemElem]
releaseItem users hostURI pkgInfo =
  [ RSS.Title title
  , RSS.Link uri
  , RSS.Guid True (uriToString id uri "")
  , RSS.PubDate time
  , RSS.Description desc
  ]
  where
    uri   = hostURI { uriPath = packageURL pkgId }
    title = display (packageName pkgId) ++ " " ++ display (packageVersion pkgId)
    body  = synopsis (packageDescription (pkgDesc pkgInfo))
    desc  = "<i>Added by " ++ display user ++ ", " ++ showTime time ++ ".</i>"
         ++ if null body then "" else "<p>" ++ body
    user = Users.userIdToName users userId

    (time, userId) = pkgOriginalUploadInfo pkgInfo
    pkgId = pkgInfoId pkgInfo

revisionItem :: Users -> URI -> PkgInfo -> [RSS.ItemElem]
revisionItem users hostURI pkgInfo =
  [ RSS.Title title
  , RSS.Link uri
  , RSS.Guid True (uriToString id guid "")
  , RSS.PubDate time
  , RSS.Description desc
  ]
  where
    uri   = hostURI { uriPath = packageURL pkgId  ++ "/revisions"}
    guid  = hostURI { uriPath = packageURL pkgId  ++ "/revision/" ++ show revision }
    title = display (packageName pkgId) ++ " " ++ display (packageVersion pkgId)
    body  = "Revision #" ++ show revision
    desc  = "<i>Revised by " ++ display user ++ ", " ++ showTime time ++ ".</i>"
         ++ if null body then "" else "<p>" ++ body
    user = Users.userIdToName users userId
    revision = pkgNumRevisions pkgInfo - 1

    (time, userId) = pkgLatestUploadInfo pkgInfo
    pkgId = pkgInfoId pkgInfo
