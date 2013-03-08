-- Common wrapper for HTML pages
module Distribution.Server.Pages.Template
    ( hackagePage
    , hackagePageWith
    , hackagePageWithHead
    ) where

import Text.XHtml.Strict    hiding ( p, name )

--TODO: replace all this with external templates

-- | Create top-level HTML document by wrapping the Html with boilerplate.
hackagePage :: String -> [Html] -> Html
hackagePage = hackagePageWithHead []

hackagePageWithHead :: [Html] -> String -> [Html] -> Html
hackagePageWithHead headExtra docTitle docContent =
    hackagePageWith headExtra docTitle docSubtitle docContent bodyExtra
  where
    docSubtitle = anchor ! [href introductionURL] << "Hackage :: [Package]"
    bodyExtra   = []

hackagePageWith :: [Html] -> String -> Html -> [Html] -> [Html] -> Html
hackagePageWith headExtra docTitle docSubtitle docContent bodyExtra =
    toHtml [ header << (docHead ++ headExtra)
           , body   << (docBody ++ bodyExtra) ]
  where
    docHead   = [ thetitle << ("Hackage: " ++ docTitle)
                , thelink ! [ rel "stylesheet"
                            , href stylesheetURL
                            , thetype "text/css"] << noHtml
                -- if NameSearch is enabled
                , thelink ! [ rel "search", href "/opensearch.xml"
                            , thetype "application/opensearchdescription+xml"
                            , title "Hackage" ] << noHtml
                ]
    docBody   = [ thediv ! [identifier "page-header"] << docHeader
                , thediv ! [identifier "content"] << docContent ]
    docHeader = [ menubar
                , paragraph ! [theclass "caption"] << docSubtitle ]
    menubar   = ulist ! [theclass "links", identifier "page-menu"]
                  << [ li << (anchor ! [href url] << lab)
                     | (lab, url) <- navigationBar]

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

