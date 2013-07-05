{-# LANGUAGE BangPatterns, NamedFieldPuns, GeneralizedNewtypeDeriving #-}

module Distribution.Server.Features.Search.ExtractDescriptionTerms (
    extractSynopsisTerms,
    extractDescriptionTerms
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char
import qualified NLP.Tokenize as NLP
import qualified NLP.Snowball as NLP

import Distribution.Server.Pages.Package.HaddockHtml  as Haddock
import qualified Distribution.Server.Pages.Package.HaddockParse as Haddock
import qualified Distribution.Server.Pages.Package.HaddockLex   as Haddock


extractSynopsisTerms :: Set Text -> String -> [Text]
extractSynopsisTerms stopWords =
      NLP.stems NLP.English
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


extractDescriptionTerms :: Set Text -> String -> [Text]
extractDescriptionTerms stopWords =
      NLP.stems NLP.English
    . filter (`Set.notMember` stopWords)
    . map (T.toCaseFold . T.pack)
    . either
        (const []) -- --TODO: something here
        (  filter (not . ignoreTok)
         . NLP.tokenize
         . concat . markup termsMarkup)
    . Haddock.parseHaddockParagraphs
    . Haddock.tokenise


termsMarkup :: DocMarkup String [String]
termsMarkup = Markup {
  markupEmpty         = [],
  markupString        = \s -> [s],
  markupParagraph     = id,
  markupAppend        = (++),
  markupIdentifier    = const [], -- i.e. filter these out
  markupModule        = const [], -- i.e. filter these out
  markupEmphasis      = id,
  markupMonospaced    = const [],
  markupUnorderedList = concat,
  markupOrderedList   = concat,
  markupDefList       = concatMap (\(d,t) -> d ++ t),
  markupCodeBlock     = const [],
  markupURL           = const [], --TODO: extract main part of hostname
  markupPic           = const [],
  markupAName         = const []
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
