-- Common wrapper for HTML pages
module Distribution.Server.Pages.Template
    ( hackagePage
    , hackagePageWith
    , hackagePageWithHead
    ) where

import Text.XHtml.Strict

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
    docHead   = [ thetitle << (docTitle ++ " | Hackage")
                , thelink ! [ rel "stylesheet"
                            , href stylesheetURL
                            , thetype "text/css"] << noHtml
                , meta ! [ name "viewport"
                         , content "width=device-width, initial-scale=1"]
                -- if Search is enabled
                , thelink ! [ rel "search", href "/packages/opensearch.xml"
                            , thetype "application/opensearchdescription+xml"
                            , title "Hackage" ] << noHtml
                ]
    docBody   = [ thediv ! [identifier "page-header"] << docHeader
                , thediv ! [identifier "content"] << docContent ]
    docHeader = [ navigationBar
                , paragraph ! [theclass "caption"] << docSubtitle ]

navigationBar :: Html
navigationBar =
    ulist ! [theclass "links", identifier "page-menu"]
      <<  map (li <<)
          [ anchor ! [href introductionURL] << "Home"
          , form   ! [action "/packages/search", theclass "search", method "get"]
                  << [ button ! [thetype "submit"] << "Search", spaceHtml
                     , input  ! [thetype "text", name "terms" ] ]
          , anchor ! [href pkgListURL] << "Browse"
          , anchor ! [href recentAdditionsURL] << "What's new"
          , anchor ! [href uploadURL]   << "Upload"
          , anchor ! [href accountsURL] << "User accounts"
          ]

stylesheetURL :: URL
stylesheetURL = "/static/hackage.css"

-- URL of the package list
pkgListURL :: URL
pkgListURL = "/packages/browse"

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
--
-- TODO: Currently unused.
_adminURL :: URL
_adminURL = "/admin"

-- URL of the list of recent additions to the database
recentAdditionsURL :: URL
recentAdditionsURL = "/packages/recent"
