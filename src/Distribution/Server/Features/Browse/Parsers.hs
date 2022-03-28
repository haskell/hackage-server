{-# LANGUAGE BlockArguments, OverloadedStrings, TupleSections #-}
module Distribution.Server.Features.Browse.Parsers
  ( Condition(..)
  , DeprecatedOption(..)
  , Filter(..)
  , Operator(..)
  , conditions
  , condsToFiltersAndTerms
  , filterOrSearchTerms
  , operatorToFunction
  , searchTerms
  ) where

import Prelude hiding (Ordering(..), filter)
import Control.Applicative ((<|>))
import Control.Monad (guard, join)
import Data.Foldable (asum)
import Data.Time (Day, NominalDiffTime, nominalDay)
import GHC.Float (double2Float)

import Data.Attoparsec.Text
import Data.Attoparsec.Time (day)
import Data.Text (Text)

data DeprecatedOption
  = OnlyDeprecated
  | ExcludeDeprecated
  | Don'tCareAboutDeprecated
  deriving (Show, Eq)

data Filter
  = DownloadsFilter (Operator, Word)
  | RatingFilter (Operator, Float)
  | LastUploadFilter (Operator, Day)
  | AgeLastULFilter (Operator, NominalDiffTime)
  | TagFilter String
  | MaintainerFilter String
  | DeprecatedFilter DeprecatedOption
  | DistroFilter String
  | Not Filter
  deriving (Show, Eq)

data Operator = LT | LTE | GT | GTE | EQ | NEQ
  deriving (Show, Eq)

deprecatedOption :: Parser DeprecatedOption
deprecatedOption =
  asum
    [ "any" *> pure Don'tCareAboutDeprecated
    , ("false" <|> "no") *> pure ExcludeDeprecated
    , ("true" <|> "yes") *> pure OnlyDeprecated
    ]


operatorToFunction :: Ord a => Operator -> a -> (a -> Bool)
operatorToFunction LT  a = (a <)
operatorToFunction LTE a = (a <=)
operatorToFunction GT  a = (a >)
operatorToFunction GTE a = (a >=)
operatorToFunction EQ  a = (a ==)
operatorToFunction NEQ a = (a /=)

data Condition = FilterCond Filter | SearchTermCond String
  deriving (Show, Eq)

condsToFiltersAndTerms :: [Condition] -> ([Filter], [String])
condsToFiltersAndTerms conds =
  ([x | FilterCond x <- conds], [x | SearchTermCond x <- conds])

opAndSndParam :: Ord a => Parser a -> Parser (Operator, a)
opAndSndParam parser = do
  let mkParser op = skipSpace *> fmap (op,) parser
      lt        = "<"  *> mkParser LT
      gt        = ">"  *> mkParser GT
      gte       = ">=" *> mkParser GTE
      lte       = "<=" *> mkParser LTE
      eq        = "="  *> mkParser EQ
      longEq    = "==" *> mkParser EQ
      neq       = "/=" *> mkParser NEQ
      cStyleNeq = "!=" *> mkParser NEQ
   in asum [lt, gt, gte, lte, eq, longEq, neq, cStyleNeq]

allowedAfterOpeningBrace :: AllowNot -> Parser Text
allowedAfterOpeningBrace AllowNot = "not " <|> allowedAfterOpeningBrace DisallowNot
allowedAfterOpeningBrace _ =
  asum
    [ "downloads", "rating", "lastUpload" , "ageOfLastUpload"
    , "tag:", "maintainer:", "deprecated:", "distro:"
    ]

-- Whether the 'not' operator can be used.
-- (used to prevent recursive parsing)
data AllowNot = AllowNot | DisallowNot

filterWith :: AllowNot -> Parser Filter
filterWith allowNot = do
  fieldName <- allowedAfterOpeningBrace allowNot
  if fieldName == "not "
     then Not <$> filterWith DisallowNot
     else do
      skipSpace
      let nonNegativeFloat :: Parser Float
          nonNegativeFloat = do
            float <- double2Float <$> double
            guard $ float >= 0
            pure float
      filt <- case fieldName of
        "downloads" -> DownloadsFilter <$> opAndSndParam decimal
        "rating" -> RatingFilter <$> opAndSndParam nonNegativeFloat
        "lastUpload" -> LastUploadFilter <$> opAndSndParam day
        "ageOfLastUpload" -> AgeLastULFilter <$> opAndSndParam nominalDiffTime
        "tag:" -> TagFilter <$> wordWoSpaceOrParens
        "maintainer:" -> MaintainerFilter <$> wordWoSpaceOrParens
        "deprecated:" -> DeprecatedFilter <$> deprecatedOption
        "distro:" -> DistroFilter <$> wordWoSpaceOrParens
        _ -> fail "Impossible since fieldName possibilities are known at compile time"
      pure filt

filter :: Parser [Condition]
filter = do
  filt <- filterWith AllowNot
  pure [FilterCond filt]

filterOrSearchTerms :: Parser [Condition]
filterOrSearchTerms =
  asum
    [ do
      _ <- "("
      skipSpace
      filt <- filter <|> searchTerms <|> pure []
      skipSpace
      _ <- ")"
      pure filt
    , searchTerms
    ]

searchTerms :: Parser [Condition]
searchTerms = sepBy1 searchTerm (many1 space)

-- The search engine accepts terms with spaces or parenthesis in them also but
-- we do not allow that, just to keep this parser simple.
searchTerm :: Parser Condition
searchTerm = fmap SearchTermCond wordWoSpaceOrParens

wordWoSpaceOrParens :: Parser String
wordWoSpaceOrParens = many1 . satisfy $ notInClass " ()"

conditions :: Parser [Condition]
conditions = fmap join . many' $ skipSpace *> filterOrSearchTerms

nominalDiffTime :: Parser NominalDiffTime
nominalDiffTime = do
  num <- double
  guard (num > 0)
  skipSpace
  lengthSpecifier <- "d" <|> "w" <|> "m" <|> "y"
  let days = realToFrac num * nominalDay
  case lengthSpecifier of
    "d" -> pure days
    "w" -> pure (days * 7)
    "m" -> pure (days * 30.437) -- Average month length
    "y" -> pure (days * 365.25) -- Average year length
    _ -> fail "Impossible since lengthSpecifier possibilities are known at compile time"
