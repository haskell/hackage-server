{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Distribution.Server.Features.Search.PkgSearch (
    PkgSearchEngine,
    initialPkgSearchEngine,
    defaultSearchRankParameters,
    PkgDocField(..),
  ) where

import Distribution.Server.Features.Search.SearchEngine
import Distribution.Server.Features.Search.ExtractNameTerms
import Distribution.Server.Features.Search.ExtractDescriptionTerms

import Data.Ix
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import NLP.Snowball

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Text (display)


type PkgSearchEngine = SearchEngine PackageDescription PackageName PkgDocField

data PkgDocField = NameField
                 | SynopsisField
                 | DescriptionField
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

initialPkgSearchEngine :: PkgSearchEngine
initialPkgSearchEngine =
    initSearchEngine pkgSearchConfig defaultSearchRankParameters

pkgSearchConfig :: SearchConfig PackageDescription PackageName PkgDocField
pkgSearchConfig =
    SearchConfig {
      documentKey           = packageName,
      extractDocumentTerms  = extractTokens,
      transformQueryTerm    = normaliseQueryToken
  }
  where
    extractTokens :: PackageDescription -> PkgDocField -> [Text]
    extractTokens pkg NameField        = extractPackageNameTerms           (display $ packageName pkg)
    extractTokens pkg SynopsisField    = extractSynopsisTerms    stopWords (synopsis    pkg)
    extractTokens pkg DescriptionField = extractDescriptionTerms stopWords (description pkg)

    normaliseQueryToken :: Text -> PkgDocField -> Text
    normaliseQueryToken tok =
      let tokFold = T.toCaseFold tok
          tokStem = stem English tokFold
       in \field -> case field of
                      NameField        -> tokFold
                      SynopsisField    -> tokStem
                      DescriptionField -> tokStem

defaultSearchRankParameters :: SearchRankParameters PkgDocField
defaultSearchRankParameters =
    SearchRankParameters {
      paramK1,
      paramB,
      paramFieldWeights,
      paramResultsetSoftLimit = 200,
      paramResultsetHardLimit = 400
    }
  where
    paramK1 :: Float
    paramK1 = 1.5

    paramB :: PkgDocField -> Float
    paramB NameField        = 0.9
    paramB SynopsisField    = 0.5
    paramB DescriptionField = 0.5

    paramFieldWeights :: PkgDocField -> Float
    paramFieldWeights NameField        = 20
    paramFieldWeights SynopsisField    = 5
    paramFieldWeights DescriptionField = 1


stopWords :: Set Term
stopWords =
  Set.fromList
    ["haskell","library","simple","using","interface","functions",
     "implementation","package","support","'s","based","for","a","and","the",
     "to","of","with","in","an","on","from","that","as","into","by","is",
     "some","which","or","like","your","other","can","at","over","be","it",
     "within","their","this","but","are","get","one","all","you","so","only",
     "now","how","where","when","up","has","been","about","them","then","see",
     "no","do","than","should","out","off","much","if","i","have","also"]


{-
-------------------
-- Main experiment
--

main :: IO ()
main = do
    pkgsFile <- readFile "pkgs2"
    let pkgs     :: [PackageDescription]
        pkgs = map read (lines pkgsFile)

--    print "reading file"
--    evaluate (length mostFreq + length pkgs)
--    print "done"

    stopWordsFile <- T.readFile "stopwords.txt"

    let stopWords = Set.fromList (T.lines stopWordsFile)

    print "forcing pkgs..."
    evaluate (foldl' (\a p -> seq p a) () pkgs `seq` Set.size stopWords)

    let config = pkgSearchConfig stopWords
        searchengine = insertDocs pkgs $ initSearchEngine config

    print "constructing index..."
    printTiming "done" $
      evaluate searchengine
    print ("search engine invariant", invariant searchengine)

--    print [ avgFieldLength ctx s | s <- [minBound..maxBound] ]
    
--    print $ take 100 $ sortBy (flip compare) $ map Set.size $ Map.elems (termMap searchindex)
--    T.putStr $ T.unlines $ Map.keys (termMap searchindex)
--    let SearchEngine{searchIndex=SearchIndex{termMap, termIdMap, docKeyMap, docIdMap}} = searchengine
--    print (Map.size termMap, IntMap.size termIdMap, Map.size docKeyMap, IntMap.size docIdMap)

    let loop = do
          putStr "search term> "
          hFlush stdout 
          t <- getLine
          unless (null t) $ do
            let terms = stems English
                      . map (T.toCaseFold . T.pack)
                      $ words t

            putStrLn "Ranked results:"
            let rankedResults = query searchengine terms

            putStr $ unlines
              [ {-show rank ++ ": " ++ -}display pkgname
              | ({-rank, -}pkgname) <- take 10 rankedResults ]

            loop
    return ()
    loop

printTiming msg action = do
    t   <- getCurrentTime
    action
    t'  <- getCurrentTime
    print (msg ++ ". time: " ++ show (diffUTCTime t' t))
-}
