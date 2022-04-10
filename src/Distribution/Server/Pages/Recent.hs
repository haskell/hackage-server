-- Takes a reversed log file on the standard input and outputs web page.

module Distribution.Server.Pages.Recent (
    recentPage,
    recentFeed,
    revisionsPage,
    recentRevisionsFeed,
    recentPaging,
    revisionsPaging,
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
import Distribution.Utils.ShortText (fromShortText)

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
         ( defaultTimeLocale, formatTime )
import Data.Maybe
         ( listToMaybe)
import Distribution.Server.Util.Paging (PaginatedConf (PaginatedConf), paginate, totalPages)

-- | Takes a list of package info, in reverse order by timestamp.

recentPaging :: PaginatedConf -> Users -> [PkgInfo] -> Html
recentPaging conf users pkgs =
  let log_rows = makeRow users <$> paginate conf pkgs
      docBody =
        [ XHtml.h2 << "Recent additions",
          XHtml.table ! [XHtml.align "center"] << log_rows,
          paginator conf recentURL,
          XHtml.anchor ! [XHtml.href recentRevisionsURL] << XHtml.toHtml "Recent revisions"
        ]
      rss_link =
        XHtml.thelink
          ! [ XHtml.rel "alternate",
              XHtml.thetype "application/rss+xml",
              XHtml.title "Hackage RSS Feed",
              XHtml.href rssFeedURL
            ]
          << XHtml.noHtml
   in hackagePageWithHead [rss_link] "recent additions" docBody


recentPage :: Users -> [PkgInfo] -> Html
recentPage = recentPaging (PaginatedConf 1 25 1000)


paginator :: PaginatedConf -> URL -> Html 
paginator pc@(PaginatedConf currPage _ totalAmount) baseUrl = 
  let 
    total = totalPages pc
    infoText = "Showing " ++ show currPage ++ " to " ++ show total ++ " of " ++ show totalAmount ++ " entries"
    info = XHtml.thediv << infoText
    next = XHtml.anchor ! [XHtml.href (paginateURLNext pc baseUrl)] << "Next"
    prev = XHtml.anchor ! [XHtml.href (paginateURLPrev pc baseUrl)] << "Previous"
    wrapper = XHtml.thediv ! [XHtml.theclass "paginator"] << (prev <> next)
  in XHtml.thediv ! [XHtml.identifier "paginatorContainer"] << mconcat [info, wrapper]


-- Should actually check if next exists
paginateURLNext,paginateURLPrev :: PaginatedConf -> URL -> URL
paginateURLNext (PaginatedConf cp _ _) url = url <> "?page=" ++ (show . succ) cp
paginateURLPrev (PaginatedConf cp _ _) url = url <> "?page=" ++ (show . pred) cp

revisionsPaging :: PaginatedConf -> Users -> [PkgInfo] -> Html
revisionsPaging conf users pkgs =
  let log_rows = map (makeRevisionRow users) (paginate conf pkgs)
      docBody =
        [ XHtml.h2 << "Recent cabal metadata revisions",
          XHtml.table ! [XHtml.align "center"] << log_rows,
          paginator conf recentRevisionsURL
        ]
      rss_link =
        XHtml.thelink
          ! [ XHtml.rel "alternate",
              XHtml.thetype "application/rss+xml",
              XHtml.title "Hackage Revisions RSS Feed",
              XHtml.href revisionsRssFeedURL
            ]
          << XHtml.noHtml
   in hackagePageWithHead [rss_link] "recent revisions" docBody

-- Remove Later
revisionsPage :: Users -> [PkgInfo] -> Html
revisionsPage = revisionsPaging (PaginatedConf 1 50 1000)

makeRow :: Users -> PkgInfo -> Html
makeRow users pkginfo =
  XHtml.tr <<
    [XHtml.td ! [XHtml.align "right"] <<
            [showTimeHtml time, nbsp, nbsp],
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
            [showTimeHtml time, nbsp, nbsp],
     XHtml.td ! [XHtml.align "left"] << display user,
     XHtml.td ! [XHtml.align "left"] <<
                  [nbsp, nbsp, XHtml.anchor !
                           [XHtml.href (packageURL pkgid ++ "/revisions")] << revlabel]]
  where
    nbsp = XHtml.primHtmlChar "nbsp"
    user = Users.userIdToName users userId

    (time, userId) = pkgLatestUploadInfo pkginfo
    pkgid = pkgInfoId pkginfo
    revno = "-r" ++ show (pkgNumRevisions pkginfo - 1)
    revlabel = [XHtml.toHtml (display pkgid), XHtml.toHtml revno]

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

showTimeHtml :: UTCTime -> Html
showTimeHtml t = XHtml.thespan ! [XHtml.title $ formatTime defaultTimeLocale "%c" t ]
    << [ XHtml.toHtml (showTime t) ]

-- | URL describing a package.
packageURL :: PackageIdentifier -> URL
packageURL pkgid = "/package/" ++ display pkgid

rssFeedURL :: URL
rssFeedURL = "/recent.rss"

recentURL :: URL
recentURL = "/packages/recent.html"

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
  (channel updated)
  (map (releaseItem users hostURI) pkgList)
  where
    desc = "The 20 most recent additions to Hackage (or last 48 hours worth, whichever is greater), the Haskell package database."
    twoDaysAgo = addUTCTime (negate $ 60 * 60 * 48) now
    pkgListTwoDays = takeWhile (\p -> pkgLatestUploadTime p > twoDaysAgo) pkgs
    pkgList = if length pkgListTwoDays > 20 then pkgListTwoDays else take 20 pkgs
    updated = maybe now (fst . pkgOriginalUploadInfo) (listToMaybe pkgList)

recentRevisionsFeed :: Users -> URI -> UTCTime -> [PkgInfo] -> RSS
recentRevisionsFeed users hostURI now pkgs = RSS
  "Recent revisions"
  (hostURI { uriPath = recentRevisionsURL})
  desc
  (channel updated)
  (map (revisionItem users hostURI) pkgList)
  where
    desc = "The 40 most recent revisions to cabal metadata in Hackage (or last 48 hours worth, whichever is greater), the Haskell package database."
    twoDaysAgo = addUTCTime (negate $ 60 * 60 * 48) now
    pkgListTwoDays = takeWhile (\p -> pkgLatestUploadTime p > twoDaysAgo) pkgs
    pkgList = if length pkgListTwoDays > 40 then pkgListTwoDays else take 40 pkgs
    updated = maybe now (fst . pkgOriginalUploadInfo) (listToMaybe pkgList)

channel :: UTCTime -> [RSS.ChannelElem]
channel updated =
  [ RSS.Language "en"
  , RSS.ManagingEditor email
  , RSS.WebMaster email
  , RSS.ChannelPubDate updated
  , RSS.LastBuildDate updated
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
    body  = fromShortText $ synopsis (packageDescription (pkgDesc pkgInfo))
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
