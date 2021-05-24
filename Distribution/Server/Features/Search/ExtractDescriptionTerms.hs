{-# LANGUAGE BangPatterns, NamedFieldPuns, GeneralizedNewtypeDeriving #-}

module Distribution.Server.Features.Search.ExtractDescriptionTerms (
    extractSynopsisTerms,
    extractDescriptionTerms,
    extraStems
  ) where

import Distribution.Server.Prelude

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char
import qualified NLP.Tokenize as NLP
import qualified NLP.Snowball as NLP
import qualified Data.Foldable as F
import Data.List (intercalate)

import qualified Documentation.Haddock.Markup as Haddock
import Documentation.Haddock.Types

import qualified Distribution.Server.Pages.Package.HaddockParse as Haddock (parse)

extraStems :: [Text] -> Text -> [Text]
extraStems ss x = x : mapMaybe (`T.stripSuffix` x) ss

extractSynopsisTerms :: [Text] -> Set Text -> String -> [Text]
extractSynopsisTerms ss stopWords =
      concatMap (extraStems ss) --note this adds extra possible stems, it doesn't delete any given one.
    . NLP.stems NLP.English
    . filter (`Set.notMember` stopWords)
    . map (T.toCaseFold . T.pack)
    . concatMap splitTok
    . filter (not . ignoreTok)
    . NLP.tokenize


ignoreTok :: String -> Bool
ignoreTok = all isPunctuation

splitTok :: String -> [String]
splitTok tok =
    case go tok of
      toks@(_:_:_) -> tok:toks
      toks         -> toks
  where
    go remaining =
      case break (\c -> c == ')' || c == '-' || c == '/') remaining of
        ([],      _:trailing) -> go trailing
        (leading, _:trailing) -> leading : go trailing
        ([],      [])         -> []
        (leading, [])         -> leading : []


extractDescriptionTerms :: [Text] -> Set Text -> String -> [Text]
extractDescriptionTerms ss stopWords =
      concatMap (extraStems ss)
    . NLP.stems NLP.English
    . filter (`Set.notMember` stopWords)
    . map (T.toCaseFold . T.pack)
    . maybe
        [] --TODO: something here
        (  filter (not . ignoreTok)
         . NLP.tokenize
         . intercalate " " . Haddock.markup termsMarkup)
    . Haddock.parse

termsMarkup :: DocMarkupH () String [String]
termsMarkup = Markup {
  markupEmpty         = [],
  markupString        = \s -> [s],
  markupParagraph     = id,
  markupAppend        = (++),
  markupIdentifier    = \s -> [s],
  markupIdentifierUnchecked = const [], -- TODO
  markupModule        = const [], -- i.e. filter these out
  markupWarning       = id,
  markupEmphasis      = id,
  markupBold          = id,
  markupMonospaced    = \s -> if length s > 1 then [] else s,
  markupUnorderedList = concat,
  markupOrderedList   = concat,
  markupDefList       = concatMap (\(d,t) -> d ++ t),
  markupCodeBlock     = const [],
  markupTable         = concat . F.toList,
  markupHyperlink     = \(Hyperlink _url mLabel) -> fromMaybe [] mLabel,
                        --TODO: extract main part of hostname
  markupAName         = const [],
  markupPic           = const [],
  markupMathInline    = const [],
  markupMathDisplay   = const [],
  markupProperty      = const [],
  markupExample       = const [],
  markupHeader        = \(Header _lvl title) -> title
  }

{-
-------------------
-- Main experiment
--

main = do
    pkgsFile <- readFile "pkgs"
    let mostFreq :: [String]
        pkgs     :: [PackageDescription]
        (mostFreq, pkgs) = read pkgsFile

    stopWordsFile <- T.readFile "stopwords.txt"
--    wordsFile <- T.readFile "/usr/share/dict/words"
--    let ws = Set.fromList (map T.toLower $ T.lines wordsFile)


    print "reading file"
    evaluate (length mostFreq + length pkgs)
    print "done"

    let stopWords = Set.fromList $ T.lines stopWordsFile
    print stopWords

    sequence_
      [ putStrLn $ display (packageName pkg) ++ ": "
                ++ --intercalate ", "
                   (description pkg) ++ "\n"
                ++ intercalate ", "
                   (map T.unpack $ extractDescriptionTerms stopWords (description pkg)) ++ "\n"
      | pkg <- pkgs
      , let pkgname = display (packageName pkg) ]
-}
