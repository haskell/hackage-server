-- Takes a reversed log file on the standard input and outputs web page.
{-# LANGUAGE NamedFieldPuns #-}

module Distribution.Server.Pages.Recent (
    recentPage,
    recentFeed,
    revisionsPage,
    recentRevisionsFeed,
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
import Text.XHtml ( Html, URL, (<<), (!) )
import qualified Text.RSS as RSS
import Text.RSS ( RSS(RSS) )
import Network.URI ( URI(..), uriToString )
import Data.Time.Clock ( UTCTime )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Maybe ( listToMaybe, fromMaybe)
import Distribution.Server.Util.Paging (PaginatedConfiguration(..), hasNext,
  hasPrev, nextURL, pageIndexRange, paginate, prevURL, toURL, allPagedURLs, pagingInfo)

-- | Takes a list of package info, in reverse order by timestamp.

recentPage :: PaginatedConfiguration -> Users -> [PkgInfo] -> Html
recentPage conf users pkgs =
  let log_rows = makeRow users <$> paginate conf pkgs
      docBody =
        [ XHtml.h2 << "Recent additions",
          pageSizeForm recentURL,
          XHtml.table ! [XHtml.align "center"] << log_rows,
          paginator conf recentURL,
          XHtml.anchor ! [XHtml.href recentRevisionsURL] << XHtml.toHtml "Recent revisions"
        ]
      rss_link =
        XHtml.thelink
          ! [ XHtml.rel "alternate",
              XHtml.thetype "application/rss+xml",
              XHtml.title "Hackage RSS Feed",
              XHtml.href $ toURL rssFeedURL conf
            ]
          << XHtml.noHtml
   in hackagePageWithHead [rss_link] "recent additions" docBody


pageSizeForm :: URL -> Html
pageSizeForm base = 
  let pageSizeLabel = XHtml.label ! [XHtml.thefor "pageSize"] << "Page Size: "
      pageSizeInput = XHtml.input ! [XHtml.thetype "number", XHtml.name "pageSize", XHtml.strAttr "min" "0"]
      submitButton = XHtml.button ! [XHtml.thetype "submit"] << "Submit"
      theForm = XHtml.form ! [XHtml.action base, XHtml.method "GET"]
  in theForm << (pageSizeLabel <> pageSizeInput <> submitButton)


paginator :: PaginatedConfiguration -> URL -> Html 
paginator pc@PaginatedConfiguration{currPage} baseUrl = 
  let 
    info = XHtml.thediv << pagingInfo pc

    next = XHtml.anchor ! [XHtml.href (fromMaybe "" (nextURL baseUrl pc)) | hasNext pc] << "Next" 
    prev = XHtml.anchor ! [XHtml.href (fromMaybe "" (prevURL baseUrl pc)) | hasPrev pc] << "Previous"
      

    pagedURLS = zip [1..] (allPagedURLs baseUrl pc)
    pagedLinks = (\(x,y) -> XHtml.anchor ! [XHtml.href y, 
      if currPage == x then XHtml.theclass "current" else noAttr ] << show x) <$> pagedURLS

    wrapper = XHtml.thediv ! [XHtml.theclass "paginator"] << 
      (prev <> reducePagedLinks pc pagedLinks <> next)


  in XHtml.thediv ! [XHtml.identifier "paginatorContainer"] << mconcat [info, wrapper]

noAttr :: XHtml.HtmlAttr
noAttr = XHtml.theclass ""

-- | Generates a list of links of the current possible paging links, recreates the functionality of the paging links on the search page
reducePagedLinks :: PaginatedConfiguration -> [Html] -> Html
reducePagedLinks PaginatedConfiguration{currPage} xs
  | length xs <= 5 = mconcat xs -- Do Nothing
  | currPage >= (length xs - 3) = mconcat  . keepLastPages .fillFirst $ xs -- Beginning ellipses
  | currPage < 5 = mconcat . keepFirstPages . fillLast $ xs -- Ending ellipses
  | otherwise = mconcat . keepMiddlePages . fillLast . fillFirst $ xs -- Begin and End ellipses
  where filler = XHtml.thespan << "..."
        fillFirst x = insertAt 1 filler x
        fillLast x = insertAt (pred . length $ x) filler x
        keepFirstPages x = case splitAt (length x - 2) x of (hts, hts') -> take 5 hts ++ hts'
        keepLastPages x = case splitAt 2 x of (hts, hts') -> hts ++ takeLast 5 hts'
        keepMiddlePages x = 
          case splitAt currPage x of (hts, hts') -> take 2 hts ++ [last hts] ++ take 2 hts' 
                                      ++ takeLast 2 hts'
                                      
insertAt :: Int -> a -> [a] -> [a]
insertAt n a x = case splitAt n x of (hts, hts') -> hts ++ [a] ++ hts'

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

revisionsPage :: PaginatedConfiguration -> Users -> [PkgInfo] -> Html
revisionsPage conf users pkgs =
  let log_rows = map (makeRevisionRow users) (paginate conf pkgs)
      docBody =
        [ XHtml.h2 << "Recent cabal metadata revisions",
          pageSizeForm recentRevisionsURL,
          XHtml.table ! [XHtml.align "center"] << log_rows,
          paginator conf recentRevisionsURL
        ]
      rss_link =
        XHtml.thelink
          ! [ XHtml.rel "alternate",
              XHtml.thetype "application/rss+xml",
              XHtml.title "Hackage Revisions RSS Feed",
              XHtml.href $ toURL revisionsRssFeedURL conf
            ]
          << XHtml.noHtml
   in hackagePageWithHead [rss_link] "recent revisions" docBody

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


recentFeed :: PaginatedConfiguration -> Users -> URI -> UTCTime -> [PkgInfo] -> RSS
recentFeed conf users hostURI now pkgs = RSS
  "Recent additions"
  (hostURI { uriPath = recentAdditionsURL})
  desc
  (channel updated)
  (map (releaseItem users hostURI) pkgList)
  where
    (start,end) = pageIndexRange conf
    desc = "Showing " ++ show start ++ " - " ++ show end ++ " most recent additions to Hackage, the Haskell package database."
    pkgList = paginate conf pkgs
    updated = maybe now (fst . pkgOriginalUploadInfo) (listToMaybe pkgList)

recentRevisionsFeed :: PaginatedConfiguration -> Users -> URI -> UTCTime -> [PkgInfo] -> RSS
recentRevisionsFeed conf users hostURI now pkgs = RSS
  "Recent revisions"
  (hostURI { uriPath = recentRevisionsURL})
  desc
  (channel updated)
  (map (revisionItem users hostURI) pkgList)
  where
    (start, end) = pageIndexRange conf
    desc = "Showing " ++ show start ++ " - " ++ show end ++ " most recent revisions to cabal metadata in Hackage, the Haskell package database."
    pkgList = paginate conf pkgs
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
