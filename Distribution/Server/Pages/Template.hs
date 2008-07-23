-- Common wrapper for HTML pages
module Distribution.Server.Pages.Template (hackagePage, hackagePageWith) where

import Text.XHtml		hiding ( p )

-- | Create top-level HTML document by wrapping the Html with boilerplate.
hackagePage :: String -> [Html] -> Html
hackagePage = hackagePageWith []

hackagePageWith :: [Html] -> String -> [Html] -> Html
hackagePageWith links heading docs = toHtml [header << docHead, body << docBody]
  where docHead =
		meta ! [httpequiv "Content-type",
			content "text/html; charset=ISO-8859-1"] :
		thetitle << ("HackageDB: " ++ heading) :
		thelink ! [rel "stylesheet", href stylesheetURL,
			thetype "text/css"] << noHtml :
		links
	docBody = [thediv ! [theclass "header"] << docHeader,
		thediv ! [theclass "content"] << docs]
	docHeader = [h1 << hackageTitle,
		table ! [theclass "navigation"] << navigation]
	hackageTitle = "hackageDB :: [Package]"
	navigation = tr << [td << (anchor ! [href url] << lab) |
				(lab, url) <- navigationBar]

navigationBar :: [(String, URL)]
navigationBar =
	[ ("Introduction",	introductionURL)
	, ("Packages",		pkgListURL)
	-- , ("Search",            searchURL)
	, ("What's new",	recentAdditionsURL)
	, ("Upload",		uploadURL)
	, ("User accounts",	accountsURL)
	]

stylesheetURL :: URL
stylesheetURL = "/hackage.css"

-- URL of the package list
pkgListURL :: URL
pkgListURL = "/packages/"

-- URL of the upload form
introductionURL :: URL
introductionURL = "/"

-- URL of the upload form
uploadURL :: URL
uploadURL = "/upload.html"

-- URL about user accounts, including the form to change passwords
accountsURL :: URL
accountsURL = "/accounts.html"

-- URL of the list of recent additions to the database
recentAdditionsURL :: URL
recentAdditionsURL = "/recent.html"
