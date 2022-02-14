{-# LANGUAGE CPP #-}

-- | Module providing standard Haskell vocabulary
--
-- This provides an extended "Prelude" re-exporting the most common
-- @base@ symbols to reduce the @import@-boilerplate as well as
-- unused-import warnings.
module Distribution.Server.Prelude
    ( module X
    , parseTimeMaybe
    , readPTime'
    , isLeft
    ) where

import           Control.Applicative as X
import           Control.Monad       as X
import           Data.Int            as X
import           Data.List           as X (sortBy, sortOn)
import           Data.Maybe          as X
import           Data.Ord            as X (comparing)
import           Data.Semigroup      as X
import           Data.Typeable       as X (Typeable)
import           Data.Word           as X
import           Prelude             as X
import           Control.Monad.IO.Class as X (MonadIO(liftIO))

import           Data.Either                  (isLeft)
import           Text.ParserCombinators.ReadP as ReadP
import           Data.Time.Format             (ParseTime, defaultTimeLocale, parseTimeM, readSTime)

parseTimeMaybe :: ParseTime t => String -> String -> Maybe t
parseTimeMaybe = parseTimeM True defaultTimeLocale

readPTime' :: ParseTime t => String -> ReadP.ReadP t
readPTime' fmt = ReadP.readS_to_P (readSTime True defaultTimeLocale fmt)
