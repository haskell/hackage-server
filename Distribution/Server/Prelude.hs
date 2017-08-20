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
    ) where

import           Control.Applicative as X
import           Control.Monad       as X
import           Data.Int            as X
import           Data.Maybe          as X
import           Data.Semigroup      as X
import           Data.Typeable       as X (Typeable)
import           Data.Word           as X
import           Prelude             as X

-- TODO: move somewhere else
import Data.Time.Locale.Compat (defaultTimeLocale)
-- import Text.ParserCombinators.ReadP (ReadP)
import Distribution.Compat.ReadP as ReadP
#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (ParseTime, parseTimeM, readSTime)
#else
import Data.Time.Format (ParseTime, parseTime, readsTime)
#endif

parseTimeMaybe :: ParseTime t => String -> String -> Maybe t
#if MIN_VERSION_time(1,5,0)
parseTimeMaybe = parseTimeM True defaultTimeLocale
#else
parseTimeMaybe = parseTime defaultTimeLocale
#endif

readPTime' :: ParseTime t => String -> ReadP.ReadP r t
#if MIN_VERSION_time(1,5,0)
readPTime' fmt = ReadP.readS_to_P (readSTime True defaultTimeLocale fmt)
#else
readPTime' fmt = ReadP.readS_to_P (readsTime defaultTimeLocale fmt)
#endif
