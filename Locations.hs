-- Adjustable locations of files and scripts
module Locations where

import PublicFile

import Text.XHtml		( URL )

-------------------------------------------------------------------------
-- Locations on the filesystem of the server, also available as URLs

docRoot :: PublicFile
docRoot = PublicFile "/srv/www/hackage.haskell.org/public_html" ""

-- Package archive directory
archiveDir :: PublicFile
archiveDir = docRoot `slash` "packages/archive"

-------------------------------------------------------------------------
-- Locations on the filesystem of the server

-- Directory containing auxiliary programs
binDir :: FilePath
binDir = "/srv/www/hackage.haskell.org/public_html/packages/bin"

-- Program to be run after an upload
postUploadHook :: FilePath
postUploadHook = binDir ++ "/post-upload-hook"

-------------------------------------------------------------------------
-- URLs, without the server name

stylesheetURL :: URL
stylesheetURL = "/packages/hackage.css"

-- URL of the package list
pkgListURL :: URL
pkgListURL = webURL (archiveDir `slash` "pkg-list.html")

-- URL of the upload form
introductionURL :: URL
introductionURL = "/packages/hackage.html"

-- URL of the search page
searchURL :: URL
searchURL = "/packages/search.html"

-- URL of the advanced search page
advancedSearchURL :: URL
advancedSearchURL = "/packages/advancedSearch.html"

-- URL of the upload form
uploadURL :: URL
uploadURL = "/packages/upload.html"

-- URL about user accounts, including the form to change passwords
accountsURL :: URL
accountsURL = "/packages/accounts.html"

-- URL of the CGI script to show details of a package
pkgScriptURL :: URL
pkgScriptURL = "/cgi-bin/hackage-scripts/package"

-- URL of the list of recent additions to the database
recentAdditionsURL :: URL
recentAdditionsURL = "/packages/archive/recent.html"

-- URL of the RSS feed of recent additions to the database
rssFeedURL :: URL
rssFeedURL = "/packages/archive/recent.rss"

cabalLogoURL :: URL
cabalLogoURL = "/images/Built-with-Cabal-light.png"

-- global URLs
cabalHomeURL :: URL
cabalHomeURL = "http://www.haskell.org/cabal"
