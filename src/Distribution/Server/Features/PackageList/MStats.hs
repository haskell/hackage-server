{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, ConstraintKinds #-}
module Distribution.Server.Features.PackageList.MStats
  ( parseM
  , sumMStat
  , getListsTables
  , getCode
  , getHCode
  , getSections
  , MStats(..)
  ) where


import           Commonmark
import           Commonmark.Extensions
import           Control.Monad.Identity
import qualified Data.ByteString.Lazy          as BS
                                                ( ByteString
                                                , toStrict
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Encoding.Error      as T
                                                ( lenientDecode )

parseM :: BS.ByteString -> FilePath -> Either ParseError [MarkdownStats]
parseM md name = runIdentity
  (commonmarkWith (pipeTableSpec <> defaultSyntaxSpec) name txt)
  where txt = T.decodeUtf8With T.lenientDecode . BS.toStrict $ md

data MStats = MStats Int Int --number of pictures, number of chars
  deriving Show

instance Monoid MStats where
  mempty = MStats 0 0

instance Rangeable MStats where
  ranged = const id

instance HasAttributes MStats where
  addAttributes = const id

instance Semigroup MStats where
  (MStats a b) <> (MStats c d) = MStats (a + c) (b + d)

data MarkdownStats = NotImportant MStats |
                     HCode MStats |
                     Code MStats |
                     Section MStats |
                     Table Int MStats | -- Int of rows
                     PText MStats |
                     List Int MStats -- Int of elements
        deriving (Show)

getCode :: [MarkdownStats] -> (Int, Int) -- number of code blocks, size of code
getCode []                           = (0, 0)
getCode (Code  (MStats codeT _) : xs) = (1, codeT) >< getCode xs
getCode (HCode (MStats codeT _) : xs) = (1, codeT) >< getCode xs
getCode (_                     : xs) = getCode xs

getHCode :: [MarkdownStats] -> (Int, Int) -- number of code blocks, size of code
getHCode []                           = (0, 0)
getHCode (HCode (MStats codeT _) : xs) = (1, codeT) >< getHCode xs
getHCode (_                     : xs) = getHCode xs

getSections :: [MarkdownStats] -> Int -- number of code blocks, size of code
getSections []               = 0
getSections (Section _ : xs) = 1 + getSections xs
getSections (_         : xs) = getSections xs

(><) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(><) (a, b) (c, d) = (a + c, b + d)


sumMStat :: [MarkdownStats] -> MStats
sumMStat []       = mempty
sumMStat (x : xs) = case x of
  (NotImportant a) -> a <> sumMStat xs
  (Section      a) -> a <> sumMStat xs
  (List  _ a     ) -> a <> sumMStat xs
  (Table _ a     ) -> a <> sumMStat xs
  (HCode a       ) -> a <> sumMStat xs
  (Code  a       ) -> a <> sumMStat xs
  (PText a       ) -> a <> sumMStat xs

getListsTables :: [MarkdownStats] -> Int
getListsTables []                 = 0
getListsTables ((List  a _) : ys) = a + getListsTables ys
getListsTables ((Table a _) : ys) = a + getListsTables ys
getListsTables (_           : ys) = getListsTables ys

instance Rangeable [MarkdownStats] where
  ranged = const id

instance HasAttributes [MarkdownStats] where
  addAttributes = const id

instance HasPipeTable MStats [MarkdownStats] where
  pipeTable _ _ rows = [Table (length rows) (mconcat $ mconcat <$> rows)]

instance IsInline MStats where
  lineBreak = MStats 0 1
  softBreak = MStats 0 1
  str t = MStats 0 (T.length t)
  entity t = MStats 0 (T.length t)
  escapedChar _ = MStats 0 1
  emph   = id
  strong = id
  link _ _ a = a
  image _ _ (MStats a b) = MStats (a + 1) b
  code t = MStats 0 (T.length t)
  rawInline _ t = MStats 0 (T.length t)

instance IsBlock MStats [MarkdownStats] where
  paragraph a = [PText a]
  plain a = [PText a]
  thematicBreak = [NotImportant mempty]
  blockQuote    = id
  codeBlock language codeT | language == T.pack "haskell" = [HCode (code codeT)]
                           | otherwise                    = [Code (code codeT)]
  heading _ a = [Section a]
  rawBlock _ _ = [NotImportant mempty]
  referenceLinkDefinition _ _ = [NotImportant mempty]
  list _ _ l = [List (length l + sumLT l) (mconcat $ sumMStat <$> l)]
    where sumLT a = sum (getListsTables <$> a)
