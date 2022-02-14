{-# LANGUAGE BangPatterns, NamedFieldPuns, GeneralizedNewtypeDeriving #-}

module Distribution.Server.Features.Search.ExtractNameTerms (
    extractPackageNameTerms,
    extractModuleNameTerms,
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isUpper, isDigit)
import Data.List
import Data.List.Split hiding (Splitter)
import Data.Maybe (maybeToList)

import Data.Functor.Identity
import Control.Monad
import Control.Monad.List
import Control.Monad.Writer
import Control.Monad.State
import Control.Applicative


extractModuleNameTerms :: String -> [Text]
extractModuleNameTerms modname =
  map T.toCaseFold $
  nub $
  map T.pack $
  flip runSplitter modname $ do
    _ <- forEachPart splitDot
    _ <- forEachPart splitCamlCase
    satisfy (not . singleChar)
    get >>= emit

extractPackageNameTerms :: String -> [Text]
extractPackageNameTerms pkgname =
  map T.toCaseFold $
  nub $
  map T.pack $
  flip runSplitter pkgname $ do

    fstComponentHyphen <- forEachPart splitHyphen

    satisfy (`notElem` ["hs", "haskell"])

    _ <- forEachPart stripPrefixH

    fstComponentCaml <- forEachPart splitCamlCase

    fstComponent2 <- forEachPart splitOn2

    when (fstComponentHyphen && fstComponentCaml && fstComponent2) $ do
      forEachPartAndWhole stripPrefix_h
    _ <- forEachPart (maybeToList . stripPrefix "lib")
    _ <- forEachPart (maybeToList . stripSuffix "lib")
    _ <- forEachPart stripSuffixNum
    satisfy (not . singleChar)

    get >>= emit

newtype Split a = Split (StateT String (ListT (WriterT [String] Identity)) a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadState String)

emit :: String -> Split ()
emit x = Split (lift (lift (tell [x])))

forEach :: [a] -> Split a
forEach = msum . map return

runSplitter :: Split () -> String -> [String]
runSplitter (Split m) s = snd (runWriter (runListT (runStateT m s)))

singleChar :: String -> Bool
singleChar [_] = True
singleChar _   = False

satisfy :: (String -> Bool) -> Split ()
satisfy p = get >>= guard . p

forEachPart :: (String -> [String]) -> Split Bool
forEachPart parts = do
  t <- get
  case parts t of
    []             -> return True
    [t'] | t == t' -> return True
    ts             -> do emit t
                         (t', n) <- forEach (zip ts [1::Int ..])
                         put t'
                         return (n==1)

forEachPartAndWhole :: (String -> [String]) -> Split ()
forEachPartAndWhole parts = do
  t <- get
  case parts t of
    []             -> return ()
    ts             -> forEach (t:ts) >>= put


splitDot :: String -> [String]
splitDot = split (dropBlanks $ dropDelims $ whenElt (=='.'))

splitHyphen :: String -> [String]
splitHyphen = rejoinAdjacentParts '-'
            . split (dropBlanks $ dropDelims $ whenElt (=='-'))

splitCamlCase :: String -> [String]
splitCamlCase = rejoinAdjacentParts '-'
              . split (dropInitBlank $ condense $ keepDelimsL $ whenElt isUpper)

-- Given split components like ["foo", "bar", "baz"]
-- recombine adjacent pairs and triples, like ["foo-bar", "bar-baz"]
rejoinAdjacentParts :: Char -> [String] -> [String]
rejoinAdjacentParts joiner parts =
    triples parts ++ pairs parts ++ parts
  where
    -- only makes a difference for 3 components or more
    pairs :: [String] -> [String]
    pairs ts@(_:ts'@(_:_:_)) =
        zipWith (\t t' -> t ++ joiner : t') ts ts'
    pairs ts = ts

     -- only makes a difference for 4 components or more
    triples :: [String] -> [String]
    triples ts@(_:ts'@(_:ts''@(_:_:_))) =
        zipWith3 (\t t' t'' -> t ++ joiner : t' ++ joiner : t'') ts ts' ts''
    triples ts = ts

stripPrefixH :: String -> [String]
stripPrefixH ('H':'S':frag)   | all isUpper frag = [frag]
stripPrefixH "HTTP"                              = []
stripPrefixH ('H':frag@(c:_)) | isUpper c        = [frag]
stripPrefixH _                                   = []

stripPrefix_h :: String -> [String]
stripPrefix_h "http"         = []
stripPrefix_h "html"         = []
stripPrefix_h ('h':'s':frag) = ['s':frag, frag]
stripPrefix_h ('h':frag)     = [frag]
stripPrefix_h _              = []

stripSuffix :: String -> String -> Maybe String
stripSuffix s t = fmap reverse (stripPrefix (reverse s) (reverse t))

stripSuffixNum :: String -> [String]
stripSuffixNum s
  | null rd || null rs' = []
  | otherwise           = [s', d]
  where
    rs        = reverse s
    (rd, rs') = span isDigit rs
    d         = reverse rd
    s'        = reverse rs'

splitOn2 :: String -> [String]
splitOn2 t =
  case break (=='2') t of
    (from@(_:_), '2':to@(c:_))
      | not (isDigit c)
      , not (length from == 1 && length to == 1)
      -> [from, to]
    _ -> []


-------------------
-- experiment
--
{-
main = do
    pkgsFile <- readFile "pkgs3"
    let pkgs     :: [PackageDescription]
        pkgs = map read (lines pkgsFile)

--    print "forcing pkgs..."
--    evaluate (foldl' (\a p -> seq p a) () pkgs)

    sequence_
      [ putStrLn $ display (packageName pkg) ++ ": " ++ display mod ++ " -> "
                ++ intercalate ", " (map T.unpack $ extractModuleNameTerms (display mod))
      | pkg <- pkgs
      , Just lib <- [library pkg]
      , let mods = exposedModules lib
      , mod <- mods ]
-}
