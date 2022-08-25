{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, ConstraintKinds #-}
module Distribution.Server.Features.PackageRank.Parser
  ( parseM
  ) where


import           Commonmark
import           Commonmark.Extensions
import           Control.Monad
import           Control.Monad.Identity
import qualified Data.ByteString.Lazy          as BS
                                                ( ByteString
                                                , toStrict
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.Encoding.Error      as T
                                                ( lenientDecode )
import qualified Data.Text.IO                  as TIO
import qualified Data.Text.Lazy.IO             as TLIO
import           Data.Typeable                  ( Typeable )
import           System.FilePath

type MarkdownRenderable a b
  = (Typeable a, HasPipeTable a b, IsBlock a b, IsInline a)

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

data MarkdownStats = NotImportant |
                     HCode MStats |
                     Code MStats |
                     Section | -- Int?
                     Table Int |
                     PText MStats |
                     List Int
        deriving (Show)

sumMStat []       = mempty
sumMStat (x : xs) = case x of
  NotImportant -> sumMStat xs
  Section      -> sumMStat xs
  (List  a)    -> sumMStat xs
  (Table a)    -> sumMStat xs
  (HCode a)    -> a <> sumMStat xs
  (Code  a)    -> a <> sumMStat xs
  (PText a)    -> a <> sumMStat xs

instance Rangeable [MarkdownStats] where
  ranged = const id

instance HasAttributes [MarkdownStats] where
  addAttributes = const id

instance HasPipeTable MStats [MarkdownStats] where
  pipeTable _ _ rows = [Table $ length rows]

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
  thematicBreak = [NotImportant]
  blockQuote    = id
  codeBlock language codeT | language == T.pack "haskell" = [HCode (code codeT)]
                           | otherwise                    = [Code (code codeT)]
  heading _ _ = [Section]
  rawBlock _ r = [NotImportant]
  referenceLinkDefinition _ _ = [NotImportant]
  list _ _ l = [List (length l + depSum l)]

depSum []                   = 0
depSum ([]            : xs) = depSum xs
depSum ((List a : ys) : xs) = a + depSum (ys : xs)
depSum ((_      : ys) : xs) = depSum (ys : xs)

