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
  ) where

import Distribution.Package
         ( PackageIdentifier )
import Distribution.Text
         ( simpleParse )
import Distribution.Simple.Utils
         ( comparing, equating )

import Data.Time.LocalTime
         ( ZonedTime(..), TimeZone(..), zonedTimeToUTC )
import Data.Time.Format
         ( parseTime )
import System.Locale
         ( defaultTimeLocale )
import Data.List
         ( sortBy, groupBy )

import Prelude hiding (read)

data Entry = Entry PackageIdentifier ZonedTime UserName [(ZonedTime, UserName)]
type UserName = String


read :: String -> Either String [Entry]
read = either Left (Right . groupEntries) . check [] . map parseLine . lines
  where
    check es' []           = Right es'
    check es' (Just  e:es) = check (e:es') es
    check _   (Nothing:_)  = Left "FIXME"

groupEntries :: [Entry] -> [Entry]
groupEntries =
    map ((\(Entry p zt u _:ps) -> Entry p zt u [ (zt', u')
                                               | Entry _ zt' u' _ <- ps ])
       . sortBy (comparing packageTime))
  . groupBy (equating packageId)
  . sortBy (comparing packageId)
  where
    packageId   (Entry pkgid _  _ _) = pkgid
    packageTime (Entry _     zt _ _) = zonedTimeToUTC zt
    
parseLine :: String -> Maybe Entry
parseLine line = do
  [day, mon, dayno, time, tz, year, user, pkg, vers] <- return (words line)
  zt <- parseTime defaultTimeLocale "%c" $
          unwords [day, mon, dayno, time, tz, year]
  pkgId <- simpleParse (pkg ++ "-" ++ vers)
  return (Entry pkgId (fixupTimeZone zt) user [])

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
