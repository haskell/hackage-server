-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.UploadLog
-- Copyright   :  (c) Ross Paterson 2007
--                    Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Support for reading the upload log of the old hackage server.
-----------------------------------------------------------------------------
module Distribution.Server.BulkImport.UploadLog (
    Entry(..),
    read,
    group,
  ) where

import Distribution.Server.Users.Types
         ( UserName )

import Distribution.Package
         ( PackageIdentifier(..))
import Distribution.Text
         ( Text(..), simpleParse )
import Distribution.ParseUtils ( parsePackageNameQ )
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint          as Disp
import Text.PrettyPrint
         ( (<+>) )
import Distribution.Simple.Utils
         ( comparing, equating )

import Data.Time.Clock
         ( UTCTime )
import Data.Time.LocalTime
         ( ZonedTime(..), TimeZone(..), zonedTimeToUTC )
import Data.Time.Format
         ( readsTime, formatTime )
import System.Locale
         ( defaultTimeLocale )
import Data.List
         ( sortBy, groupBy )

import Prelude hiding (read)

data Entry = Entry UTCTime UserName PackageIdentifier
  deriving (Eq, Ord, Show)

instance Text Entry where
  disp (Entry time user pkgid) =
        Disp.text (formatTime defaultTimeLocale "%c" time)
    <+> disp user <+> disp pkgid
  parse = do
    time <- Parse.readS_to_P (readsTime defaultTimeLocale "%c")
    Parse.skipSpaces
    user <- parse
    Parse.skipSpaces
    pkg  <- parsePackageNameQ
    Parse.skipSpaces
    ver  <- parse
    let pkgid = PackageIdentifier pkg ver
    return (Entry (zonedTimeToUTC (fixupTimeZone time)) user pkgid)

-- | Returns a list of log entries, however some packages have been uploaded
-- more than once, so each entry is paired with any older entries for the same
-- package.
--
read :: String -> Either String [Entry]
read = check [] . map parseLine . lines
  where
    check es' []           = Right es'
    check es' (Right e:es) = check (e:es') es
    check _   (Left err:_) = Left err
    parseLine line = maybe (Left err) Right (simpleParse line)
      where err = "Failed to parse log line:\n" ++ show line

group :: [Entry] -> [(Entry, [Entry])]
group =
    map ((\(p:ps) -> (p, ps))
       . sortBy (comparing packageTime))
  . groupBy (equating packageId)
  . sortBy (comparing packageId)
  where
    packageId   (Entry _  _ pkgid) = pkgid
    packageTime (Entry t  _ _)     = t

-- | The time lib doesn't know the time offsets of standard time zones so we
-- have to do it ourselves for a couple zones we're interested in. Sigh.
fixupTimeZone :: ZonedTime -> ZonedTime
fixupTimeZone zt@ZonedTime { zonedTimeZone = tz }
  | timeZoneName tz == "PST" = zt { zonedTimeZone = pst }
  | timeZoneName tz == "PDT" = zt { zonedTimeZone = pdt }
  | otherwise                = zt
  where
    pst = TimeZone (-8 * 60) False "PST"
    pdt = TimeZone (-7 * 60) True  "PDT"
