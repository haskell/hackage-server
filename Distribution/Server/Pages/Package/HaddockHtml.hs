-- stolen from Haddock's Util.hs and Doc.hs
module Distribution.Server.Pages.Package.HaddockHtml where

import Distribution.Server.Prelude

import Text.XHtml.Strict        hiding (p)
import Network.URI              (escapeURIString, isUnreserved)
import Distribution.ModuleName
import Distribution.Text        (simpleParse)
import Documentation.Haddock.Types

-- C.f. haddock-api's "Haddock.Backends.Xhtml.DocMarkup"
htmlMarkup :: (ModuleName -> Maybe URL) -> DocMarkupH mod String Html
htmlMarkup modResolv = Markup {
  markupEmpty         = noHtml,
  markupString        = toHtml,
  markupParagraph     = paragraph,
  markupAppend        = (+++),
  markupIdentifier    = thecode . toHtml,
  markupIdentifierUnchecked = const (thecode $ toHtml "FIXME"), -- should never happen
  markupModule        = mkModLink,
  markupWarning       = thediv ! [theclass "warning"],
  markupEmphasis      = emphasize,
  markupBold          = strong,
  markupMonospaced    = thecode,
  markupUnorderedList = unordList,
  markupOrderedList   = ordList,
  markupDefList       = defList,
  markupCodeBlock     = pre,
  markupHyperlink     = \(Hyperlink url mLabel) -> anchor ! [href url] << fromMaybe url (fmap showHtmlFragment mLabel),
  markupAName         = \aname -> namedAnchor aname << toHtml "",
  markupPic           = \(Picture uri mtitle) -> image ! ([src uri] ++ fromMaybe [] (return . title <$> mtitle)),
  markupMathInline    = \mathjax -> toHtml ("\\(" ++ mathjax ++ "\\)"),
  markupMathDisplay   = \mathjax -> toHtml ("\\[" ++ mathjax ++ "\\]"),
  markupProperty      = pre . toHtml,
  markupExample       = examplesToHtml,
  markupHeader        = \(Header l t) -> makeHeader l t,
  markupTable         = \(Table h r) -> makeTable h r
  }
  where
    makeHeader :: Int -> Html -> Html
    makeHeader 1 mkup = h2 mkup
    makeHeader 2 mkup = h3 mkup
    makeHeader 3 mkup = h4 mkup
    makeHeader 4 mkup = h5 mkup
    makeHeader _ mkup = h6 mkup

    examplesToHtml l = pre (concatHtml $ map exampleToHtml l) ! [theclass "screen"]

    exampleToHtml (Example expression result) = htmlExample
      where
        htmlExample = htmlPrompt +++ htmlExpression +++ toHtml (unlines result)
        htmlPrompt = (thecode . toHtml $ ">>> ") ! [theclass "prompt"]
        htmlExpression = (strong . thecode . toHtml $ expression ++ "\n") ! [theclass "userinput"]

    mkModLink :: ModLink Html -> Html
    mkModLink (ModLink s _lbl) = fromMaybe (thecode . toHtml $ s) $ do
        modname <- simpleParse s
        modUrl  <- modResolv modname
        let lnk = anchor ! [href modUrl] << s
        pure (thespan ! [theclass "module"] << lnk)

    makeTable :: [TableRow Html] -> [TableRow Html] -> Html
    makeTable hs bs = table (concatHtml (hs' ++ bs'))
      where
        hs' | null hs   = []
            | otherwise = [thead (concatHtml (map (makeTableRow th) hs))]

        bs' = [tbody (concatHtml (map (makeTableRow td) bs))]

    makeTableRow :: (Html -> Html) -> TableRow Html -> Html
    makeTableRow thr (TableRow cs) = tr (concatHtml (map (makeTableCell thr) cs))

    makeTableCell :: (Html -> Html) -> TableCell Html -> Html
    makeTableCell thr (TableCell i j c) = thr c ! (i' ++ j')
      where
        i' = if i == 1 then [] else [ colspan i ]
        j' = if j == 1 then [] else [ rowspan j ]


namedAnchor :: String -> Html -> Html
namedAnchor n = anchor ! [name (escapeStr n)]

escapeStr :: String -> String
escapeStr = escapeURIString isUnreserved
