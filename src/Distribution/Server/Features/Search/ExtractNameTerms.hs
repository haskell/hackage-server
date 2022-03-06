{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Distribution.Server.Features.Search.ExtractNameTerms (
    extractPackageNameTerms,
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isUpper, isDigit)
import Data.List
import Data.List.Split hiding (Splitter)
import Data.Maybe (maybeToList)

import Data.Functor.Identity
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Control.Applicative

-- UNUSED:
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

------------------------------------------------------------------------
-- Vendoring deprecated ListT
------------------------------------------------------------------------

-- Monad transformers @ListT@ got removed in @transformers-0.6.0@
-- so we vendor it here.
-- It does not seem worthwhile rewriting this module to not use @ListT@,
-- because:
--
--   - It is entirely undocumented.  It does not specify what the
--     module is trying to achieve.
--
--   - Individual functions are also not documented, neither
--     their invariants nor their expected behavior.
--
--   - The only exported function extractPackageNameTerms
--     seems to be only used in a package search facility.
--     Thus, it is not important from a security perspective.
--
--   - This module might become obsolete once package search
--     is rewritten.
--
-- Andreas Abel, 2022-03-06

newtype ListT m a = ListT { runListT :: m [a] }

-- | Map between 'ListT' computations.
--
-- * @'runListT' ('mapListT' f m) = f ('runListT' m)@
mapListT :: (m [a] -> n [b]) -> ListT m a -> ListT n b
mapListT f m = ListT $ f (runListT m)
{-# INLINE mapListT #-}

instance (Functor m) => Functor (ListT m) where
    fmap f = mapListT $ fmap $ map f
    {-# INLINE fmap #-}

instance (Foldable f) => Foldable (ListT f) where
    foldMap f (ListT a) = foldMap (foldMap f) a
    {-# INLINE foldMap #-}

instance (Traversable f) => Traversable (ListT f) where
    traverse f (ListT a) = ListT <$> traverse (traverse f) a
    {-# INLINE traverse #-}

instance (Applicative m) => Applicative (ListT m) where
    pure a  = ListT $ pure [a]
    {-# INLINE pure #-}
    f <*> v = ListT $ (<*>) <$> runListT f <*> runListT v
    {-# INLINE (<*>) #-}

instance (Applicative m) => Alternative (ListT m) where
    empty   = ListT $ pure []
    {-# INLINE empty #-}
    m <|> n = ListT $ (++) <$> runListT m <*> runListT n
    {-# INLINE (<|>) #-}

instance (Monad m) => Monad (ListT m) where
    m >>= k  = ListT $ do
        a <- runListT m
        b <- mapM (runListT . k) a
        return (concat b)
    {-# INLINE (>>=) #-}

instance (Monad m) => MonadPlus (ListT m) where
    mzero       = ListT $ return []
    {-# INLINE mzero #-}
    m `mplus` n = ListT $ do
        a <- runListT m
        b <- runListT n
        return (a ++ b)
    {-# INLINE mplus #-}

instance MonadTrans ListT where
    lift m = ListT $ do
        a <- m
        return [a]
    {-# INLINE lift #-}
