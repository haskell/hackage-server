-- stolen from Haddock's Util.hs and Doc.hs
module Distribution.Server.Pages.Package.HaddockHtml where

import Distribution.Server.Pages.Package.HaddockTypes
import Data.Char                (isSpace)
import Data.Maybe               (fromMaybe)
import Text.XHtml.Strict        hiding (p)
import Network.URI              (escapeURIString, isUnreserved)

markup :: DocMarkup id a -> Doc id -> a
markup m DocEmpty               = markupEmpty m
markup m (DocAppend d1 d2)      = markupAppend m (markup m d1) (markup m d2)
markup m (DocString s)          = markupString m s
markup m (DocParagraph d)       = markupParagraph m (markup m d)
markup m (DocIdentifier x)      = markupIdentifier m x
markup m (DocModule mod0)       = markupModule m mod0
markup m (DocEmphasis d)        = markupEmphasis m (markup m d)
markup m (DocMonospaced d)      = markupMonospaced m (markup m d)
markup m (DocUnorderedList ds)  = markupUnorderedList m (map (markup m) ds)
markup m (DocOrderedList ds)    = markupOrderedList m (map (markup m) ds)
markup m (DocDefList ds)        = markupDefList m (map (markupPair m) ds)
markup m (DocCodeBlock d)       = markupCodeBlock m (markup m d)
markup m (DocHyperlink l)       = markupHyperlink m l
markup m (DocAName ref)         = markupAName m ref
markup m (DocPic img)           = markupPic m img

markupPair :: DocMarkup id a -> (Doc id, Doc id) -> (a, a)
markupPair m (a,b) = (markup m a, markup m b)


-- | The identity markup
idMarkup :: DocMarkup a (Doc a)
idMarkup = Markup {
  markupEmpty         = DocEmpty,
  markupString        = DocString,
  markupParagraph     = DocParagraph,
  markupAppend        = DocAppend,
  markupIdentifier    = DocIdentifier,
  markupModule        = DocModule,
  markupEmphasis      = DocEmphasis,
  markupMonospaced    = DocMonospaced,
  markupUnorderedList = DocUnorderedList,
  markupOrderedList   = DocOrderedList,
  markupDefList       = DocDefList,
  markupCodeBlock     = DocCodeBlock,
  markupHyperlink     = DocHyperlink,
  markupAName         = DocAName,
  markupPic           = DocPic
  }

htmlMarkup :: DocMarkup String Html
htmlMarkup = Markup {
  markupEmpty         = noHtml,
  markupString        = toHtml,
  markupParagraph     = paragraph,
  markupAppend        = (+++),
  markupIdentifier    = tt . toHtml . init . tail,
  markupModule        = tt . toHtml,
  markupEmphasis      = emphasize . toHtml,
  markupMonospaced    = tt . toHtml,
  markupUnorderedList = ulist . concatHtml . map (li <<),
  markupOrderedList   = olist . concatHtml . map (li <<),
  markupDefList       = dlist . concatHtml . map markupDef,
  markupCodeBlock     = pre,
  markupHyperlink     = \(Hyperlink url mLabel) -> anchor ! [href url] << toHtml (fromMaybe url mLabel),
  markupAName         = \aname -> namedAnchor aname << toHtml "",
  markupPic           = \path -> image ! [src path]
  }
  where markupDef (a,b) = dterm << a +++ ddef << b

namedAnchor :: String -> Html -> Html
namedAnchor n = anchor ! [name (escapeStr n)]

escapeStr :: String -> String
escapeStr = escapeURIString isUnreserved

-- -----------------------------------------------------------------------------
-- ** Smart constructors

-- used to make parsing easier; we group the list items later
docAppend :: Doc id -> Doc id -> Doc id
docAppend (DocUnorderedList ds1) (DocUnorderedList ds2)
  = DocUnorderedList (ds1++ds2)
docAppend (DocUnorderedList ds1) (DocAppend (DocUnorderedList ds2) d)
  = DocAppend (DocUnorderedList (ds1++ds2)) d
docAppend (DocOrderedList ds1) (DocOrderedList ds2)
  = DocOrderedList (ds1++ds2)
docAppend (DocOrderedList ds1) (DocAppend (DocOrderedList ds2) d)
  = DocAppend (DocOrderedList (ds1++ds2)) d
docAppend (DocDefList ds1) (DocDefList ds2)
  = DocDefList (ds1++ds2)
docAppend (DocDefList ds1) (DocAppend (DocDefList ds2) d)
  = DocAppend (DocDefList (ds1++ds2)) d
docAppend DocEmpty d = d
docAppend d DocEmpty = d
docAppend d1 d2
  = DocAppend d1 d2


-- again to make parsing easier - we spot a paragraph whose only item
-- is a DocMonospaced and make it into a DocCodeBlock
docParagraph :: Doc id -> Doc id
docParagraph (DocMonospaced p)
  = DocCodeBlock (docCodeBlock p)
docParagraph (DocAppend (DocString s1) (DocMonospaced p))
  | all isSpace s1
  = DocCodeBlock (docCodeBlock p)
docParagraph (DocAppend (DocString s1)
    (DocAppend (DocMonospaced p) (DocString s2)))
  | all isSpace s1 && all isSpace s2
  = DocCodeBlock (docCodeBlock p)
docParagraph (DocAppend (DocMonospaced p) (DocString s2))
  | all isSpace s2
  = DocCodeBlock (docCodeBlock p)
docParagraph p
  = DocParagraph p


-- Drop trailing whitespace from @..@ code blocks.  Otherwise this:
--
--    -- @
--    -- foo
--    -- @
--
-- turns into (DocCodeBlock "\nfoo\n ") which when rendered in HTML
-- gives an extra vertical space after the code block.  The single space
-- on the final line seems to trigger the extra vertical space.
--
docCodeBlock :: Doc id -> Doc id
docCodeBlock (DocString s)
  = DocString (reverse $ dropWhile (`elem` " \t") $ reverse s)
docCodeBlock (DocAppend l r)
  = DocAppend l (docCodeBlock r)
docCodeBlock d = d
