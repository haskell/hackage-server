{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Distribution.Server.Features.Tags.Types where

import Distribution.Server.Framework.MemSize

import qualified Distribution.Compat.CharParsing as P
import Distribution.Parsec (Parsec(..), parsecCommaList)
import Distribution.Pretty (Pretty(..))
import qualified Text.PrettyPrint as Disp

import Control.Monad (liftM2)
import qualified Data.Char as Char
import Control.DeepSeq
import Data.SafeCopy (base, deriveSafeCopy)

newtype TagList = TagList [Tag] deriving (Show)

instance Pretty TagList where
    pretty (TagList tags) = Disp.hsep . Disp.punctuate Disp.comma $ map pretty tags
instance Parsec TagList where
    parsec = fmap TagList $ P.spaces >> parsecCommaList parsec

-- A tag is a string describing a package; presently the preferred word-separation
-- character is the dash.
newtype Tag = Tag String deriving (Show, Ord, Eq, NFData, MemSize)

instance Pretty Tag where
    pretty (Tag tag) = Disp.text tag
instance Parsec Tag where
    parsec = do
        -- adding 'many1 $ do' here would allow multiword tags.
        -- spaces aren't very aesthetic in URIs, though.
        strs <- do
            t <- liftM2 (:) (P.satisfy tagInitialChar)
               $ P.munch1 tagLaterChar
            P.spaces
            return t
        return $ Tag strs

tagInitialChar, tagLaterChar :: Char -> Bool
-- reserve + and - first-letters for queries
tagInitialChar c = Char.isAlphaNum c || c `elem` ".#*"
tagLaterChar   c = Char.isAlphaNum c || c `elem` "-+#*."

$(deriveSafeCopy 0 'base ''Tag)
