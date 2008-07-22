-- stolen from Haddock's HsSyn.lhs and HaddockHtml.hs
module Distribution.Server.Pages.Package.HaddockHtml where

import Data.Char		(isSpace)
import Text.XHtml		hiding (p)
import Network.URI              (escapeURIString, isUnreserved)

data GenDoc id
  = DocEmpty 
  | DocAppend (GenDoc id) (GenDoc id)
  | DocString String
  | DocParagraph (GenDoc id)
  | DocIdentifier id
  | DocModule String
  | DocEmphasis (GenDoc id)
  | DocMonospaced (GenDoc id)
  | DocUnorderedList [GenDoc id]
  | DocOrderedList [GenDoc id]
  | DocDefList [(GenDoc id, GenDoc id)]
  | DocCodeBlock (GenDoc id)
  | DocURL String
  | DocPic String
  | DocAName String
  deriving (Eq, Show)

type Doc = GenDoc String

-- | DocMarkup is a set of instructions for marking up documentation.
-- In fact, it's really just a mapping from 'GenDoc' to some other
-- type [a], where [a] is usually the type of the output (HTML, say).

data DocMarkup id a = Markup {
  markupEmpty         :: a,
  markupString        :: String -> a,
  markupParagraph     :: a -> a,
  markupAppend        :: a -> a -> a,
  markupIdentifier    :: id -> a,
  markupModule        :: String -> a,
  markupEmphasis      :: a -> a,
  markupMonospaced    :: a -> a,
  markupUnorderedList :: [a] -> a,
  markupOrderedList   :: [a] -> a,
  markupDefList       :: [(a,a)] -> a,
  markupCodeBlock     :: a -> a,
  markupURL	      :: String -> a,
  markupPic           :: String -> a,
  markupAName	      :: String -> a
  }

markup :: DocMarkup id a -> GenDoc id -> a
markup m DocEmpty		= markupEmpty m
markup m (DocAppend d1 d2)	= markupAppend m (markup m d1) (markup m d2)
markup m (DocString s)		= markupString m s
markup m (DocParagraph d)	= markupParagraph m (markup m d)
markup m (DocIdentifier i)	= markupIdentifier m i
markup m (DocModule mod0)	= markupModule m mod0
markup m (DocEmphasis d)	= markupEmphasis m (markup m d)
markup m (DocMonospaced d)	= markupMonospaced m (markup m d)
markup m (DocUnorderedList ds)	= markupUnorderedList m (map (markup m) ds)
markup m (DocOrderedList ds)	= markupOrderedList m (map (markup m) ds)
markup m (DocDefList ds)        = markupDefList m (map (markupPair m) ds)
markup m (DocCodeBlock d)	= markupCodeBlock m (markup m d)
markup m (DocURL url)		= markupURL m url
markup m (DocPic url)           = markupPic m url
markup m (DocAName ref)		= markupAName m ref

markupPair :: DocMarkup id a -> (GenDoc id, GenDoc id) -> (a, a)
markupPair m (a,b) = (markup m a, markup m b)

-- | The identity markup
idMarkup :: DocMarkup a (GenDoc a)
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
  markupURL	      = DocURL,
  markupPic           = DocPic,
  markupAName	      = DocAName
  }

htmlMarkup :: DocMarkup String Html
htmlMarkup = Markup {
  markupParagraph     = paragraph,
  markupEmpty         = toHtml "",
  markupString        = toHtml,
  markupAppend        = (+++),
  markupIdentifier    = tt . toHtml . init . tail,
  markupModule        = tt . toHtml,
  markupEmphasis      = emphasize . toHtml,
  markupMonospaced    = tt . toHtml,
  markupUnorderedList = ulist . concatHtml . map (li <<),
  markupOrderedList   = olist . concatHtml . map (li <<),
  markupDefList       = dlist . concatHtml . map markupDef,
  markupCodeBlock     = pre,
  markupURL           = \url -> anchor ! [href url] << toHtml url,
  markupPic           = \url -> image ! [src url],
  markupAName         = \aname -> namedAnchor aname << toHtml ""
  }
  where markupDef (a,b) = dterm << a +++ ddef << b

namedAnchor :: String -> Html -> Html
namedAnchor n = anchor ! [name (escapeStr n)]

escapeStr :: String -> String
escapeStr = escapeURIString isUnreserved

-- -----------------------------------------------------------------------------
-- ** Smart constructors

-- used to make parsing easier; we group the list items later
docAppend :: Doc -> Doc -> Doc
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
docParagraph :: Doc -> Doc
docParagraph (DocMonospaced p)
  = DocCodeBlock p
docParagraph (DocAppend (DocString s1) (DocMonospaced p))
  | all isSpace s1
  = DocCodeBlock p
docParagraph (DocAppend (DocString s1)
		(DocAppend (DocMonospaced p) (DocString s2)))
  | all isSpace s1 && all isSpace s2
  = DocCodeBlock p
docParagraph (DocAppend (DocMonospaced p) (DocString s2))
  | all isSpace s2
  = DocCodeBlock p
docParagraph p
  = DocParagraph p
