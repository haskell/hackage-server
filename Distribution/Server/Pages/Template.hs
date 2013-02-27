-- Common wrapper for HTML pages
module Distribution.Server.Pages.Template
    ( hackagePage
    , hackagePageWith
    , haddockPage
    ) where

import Data.Monoid
import System.FilePath.Posix ( (</>) )

import Text.XHtml.Strict    hiding ( p, name )

-- | Create top-level HTML document by wrapping the Html with boilerplate.
hackagePage :: String -> [Html] -> Html
hackagePage = hackagePageWith []

hackagePageWith :: [Html] -> String -> [Html] -> Html
hackagePageWith links heading docs = toHtml [header << docHead, body << docBody]
  where
    docHead =
        thetitle << ("HackageDB: " ++ heading) :
        thelink ! [rel "stylesheet", href stylesheetURL,
            thetype "text/css"] << noHtml :
        -- if NameSearch is enabled
        thelink ! [rel "search", href "/opensearch.xml", title "Hackage",
           thetype "application/opensearchdescription+xml"] << noHtml :
        links
    docBody = [thediv ! [theclass "header"] << docHeader,
        thediv ! [identifier "content"] << docs]
    docHeader = [h1 << hackageTitle,
        table ! [theclass "navigation"] << navigation]
    hackageTitle = "hackageDB :: [Package]"
    navigation = tr << [td << (anchor ! [href url] << lab) |
                (lab, url) <- navigationBar]

-- | Wrapper for pages with haddock styling
haddockPage :: HTML doc => String -> doc -> Html
haddockPage pkgname doc = toHtml [header << docHead, body << doc]
  where docHead = [
                meta ! [httpequiv "Content-type",
                        content "text/html; charset=ISO-8859-1"],
                thetitle << ("HackageDB: " ++ pkgname),
                haddockThemesLinks,
                script ! [thetype "text/javascript",
                        src haddockJSURL] << noHtml,
                script ! [thetype "text/javascript"] <<
                        "window.onload = function() {pageLoad();};"]

haddockThemesLinks :: Html
haddockThemesLinks =
    case haddockThemes of
      [] -> mempty
      (x:xs) ->
          first x `mappend` rest xs

 where
   first (name, url) =
       thelink ! [rel "stylesheet", thetype "text/css",
                      href url, title name] << noHtml
   rest xs =
       mconcat $ flip map xs $ \(name, url) ->
           thelink ! [rel "alternate stylesheet", thetype "text/css",
                          href url, title name] << noHtml

navigationBar :: [(String, URL)]
navigationBar =
    [ ("Introduction",  introductionURL)
    , ("Packages",      pkgListURL)
    -- , ("Search", searchURL)
    , ("What's new",    recentAdditionsURL)
    , ("Upload",        uploadURL)
    , ("User accounts", accountsURL)
    , ("Admin",         adminURL)
    ]

stylesheetURL :: URL
stylesheetURL = "/static/hackage.css"

-- URL of the package list
pkgListURL :: URL
pkgListURL = "/packages/"

-- URL of the upload form
introductionURL :: URL
introductionURL = "/"

-- URL of the upload form
uploadURL :: URL
uploadURL = "/upload"

-- URL about user accounts, including the form to change passwords
accountsURL :: URL
accountsURL = "/accounts"

-- URL of the admin front end
adminURL :: URL
adminURL = "/admin"

-- URL of the list of recent additions to the database
recentAdditionsURL :: URL
recentAdditionsURL = "/recent"

-- URL of haddock specifgic HTML
haddockJSURL :: URL
haddockJSURL = "/static/haddock/haddock-util.js"

-- | Haddock themes we have avaliable, name and path
haddockThemes :: [(String, String)]
haddockThemes =
    [ ("Ocean", haddockThemesDir </> "ocean.css")
    , ("Classic", haddockThemesDir </> "xhaddock.css")
    ]

-- | URL directory of haddock theme CSS files
haddockThemesDir :: URL
haddockThemesDir = "/static/haddock"
