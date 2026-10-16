-- Common wrapper for HTML pages
module Distribution.Server.Pages.Template
    ( hackagePage
    , hackagePageWith
    , hackagePageWithHead
    , syntaxHighlightingHead
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
    docSubtitle = anchor ! [href introductionURL, theclass "caption"] << "Hackage :: [Package]"
    bodyExtra   = []

hackagePageWith :: [Html] -> String -> Html -> [Html] -> [Html] -> Html
hackagePageWith headExtra docTitle docSubtitle docContent bodyExtra =
    toHtml [ header << (docHead ++ headExtra)
           , body   << (docBody ++ bodyExtra) ]
  where
    docHead   = [ thetitle << (docTitle ++ " | Hackage")
                , thelink ! [ rel "stylesheet"
                            , href googleFontURL] << noHtml
                , thelink ! [ rel "stylesheet"
                            , href stylesheetURL
                            , thetype "text/css"] << noHtml
                , thelink ! [ rel "icon"
                            , href faviconURL
                            , thetype "image/png"] << noHtml
                , meta ! [ name "viewport"
                         , content "width=device-width, initial-scale=1"]
                , (script noHtml) ! [ src "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js", thetype "text/javascript"]
                -- if Search is enabled
                , thelink ! [ rel "search", href "/packages/opensearch.xml"
                            , thetype "application/opensearchdescription+xml"
                            , title "Hackage" ] << noHtml
                ]
    docBody   = [ thediv  ! [identifier "page-header"] << docHeader
                , thediv  ! [identifier "content"] << docContent ]
    docHeader = [ docSubtitle
                , navigationBar
                ]

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


-- | Stylesheets and scripts that highlight fenced code blocks in rendered
-- Markdown.
syntaxHighlightingHead :: [Html]
syntaxHighlightingHead =
    [ cdnStylesheet "themes/prism-solarizedlight.min.css"
        "sha384-k3ZUhhfN2mU6+BTQVzpopVVQOC5ZJ4WNroBrXNNQqawO4e3kOWFCYCT77lDQupo2"
        "(prefers-color-scheme: light)"
    , cdnStylesheet "themes/prism-tomorrow.min.css"
        "sha384-wFjoQjtV1y5jVHbt0p35Ui8aV8GVpEZkyF99OXWqP/eNJDU93D3Ugxkoyh6Y2I4A"
        "(prefers-color-scheme: dark)"
    , (script noHtml) ! [strAttr "defer" "defer", src "/static/prism-setup.js"]
    , cdnScript "components/prism-core.min.js"
        "sha384-zLRFO4dwowZvh8kzutOb5AWhH7f39HeJp+N7PtHF1SQtTBnifRx0AtmvTYs3F4YV"
    , cdnScript "plugins/autoloader/prism-autoloader.min.js"
        "sha384-Uq05+JLko69eOiPr39ta9bh7kld5PKZoU+fF7g0EXTAriEollhZ+DrN8Q/Oi8J2Q"
    ]
  where
    prismURL path = "https://cdn.jsdelivr.net/npm/prismjs@1.30.0/" ++ path
    cdnStylesheet path hash media =
        thelink ! [ rel "stylesheet"
                  , href (prismURL path)
                  , strAttr "integrity" hash
                  , strAttr "crossorigin" "anonymous"
                  , strAttr "media" media] << noHtml
    cdnScript path hash =
        (script noHtml) ! [ strAttr "defer" "defer"
                          , src (prismURL path)
                          , strAttr "integrity" hash
                          , strAttr "crossorigin" "anonymous"]

googleFontURL :: URL
googleFontURL = "https://fonts.googleapis.com/css?family=PT+Sans:400,400i,700"

stylesheetURL :: URL
stylesheetURL = "/static/hackage.css"

faviconURL :: URL
faviconURL = "/static/favicon.png"

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
