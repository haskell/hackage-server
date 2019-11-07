-----------------------------------------------------------------------------
-- |
-- Module      :  ImportClient.UploadLog
-- Copyright   :  (c) Ross Paterson 2007
--                    Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Support for reading the upload log of the old hackage server.
-----------------------------------------------------------------------------
module Distribution.Client.UploadLog (
    Entry(..),
    read,
    collectUploadInfo,
    collectMaintainerInfo,
  ) where

import Prelude ()
import Distribution.Server.Prelude hiding (read)

import Distribution.Server.Users.Types
         ( UserName )

import Distribution.Package
         ( PackageId, PackageName, packageName, PackageIdentifier(..))
import Distribution.Text
         ( Text(..), simpleParse )
import Distribution.Pretty (Pretty(..))
import Distribution.Parsec (Parsec(..))
import qualified Distribution.Parsec.Class as P
import qualified Distribution.Compat.CharParsing as P
import Distribution.ParseUtils ( parseMaybeQuoted )
import qualified Text.ParserCombinators.ReadP as Parse
import qualified Text.PrettyPrint             as Disp
import Text.PrettyPrint
         ( (<+>) )
import Distribution.Simple.Utils
         ( equating )

import Data.Time.Clock
         ( UTCTime )
import Data.Time.LocalTime
         ( ZonedTime, zonedTimeToUTC )
import Data.Time.Format
         ( formatTime )
import Data.Time.Locale.Compat
         ( defaultTimeLocale )
import Data.List
         ( groupBy, nub )
import qualified Data.Char as Char

data Entry = Entry UTCTime UserName PackageIdentifier
  deriving (Eq, Ord, Show)

instance Pretty Entry where
  pretty (Entry time user pkgid) =
        Disp.text (formatTime defaultTimeLocale "%c" time)
    <+> pretty user <+> pretty pkgid

instance Parsec Entry where
  parsec = do
    time <- parseDateTimeFmt P.<?> "ZonedTime"
    P.skipSpaces1
    user <- parsec
    P.skipSpaces1
    pkg  <- P.parsecMaybeQuoted parsec
    P.skipSpaces1 -- NB: we expect spaces here instead of '-' because the pre-existing 'instance Pretty Entry' also had this asymmetry; TODO: investigate whether this is intentional
    ver  <- parsec
    let pkgid = PackageIdentifier pkg ver
    return (Entry (zonedTimeToUTC time) user pkgid)

parseDateTimeFmt :: P.CabalParsing m => m ZonedTime
parseDateTimeFmt = do
    -- parseDateTimeFmt parses "%a %b %e %H:%M:%S %Z %Y",
    p'a <- P.munch1 Char.isAlpha -- e.g. [Sun,Mon,..,Sat]
    P.skipSpaces1
    p'b <- P.munch1 Char.isAlpha -- e.g. [Jan .. Dec]
    P.skipSpaces1
    p'e <- P.munch1 Char.isDigit -- day of month
    P.skipSpaces1
    p'H <- digit2
    P.char ':'
    p'M <- digit2
    P.char ':'
    p'S <- digit2
    P.skipSpaces1
    -- timezone name (or else offset in the format ±HHMM)
    p'Z <- liftM2 (:) (P.satisfy (\c -> Char.isAsciiLower c || Char.isAsciiUpper c || c == '+' || c == '-'))
                      (P.munch (\c -> Char.isAsciiLower c || Char.isAsciiUpper c || Char.isDigit c))
    P.skipSpaces1
    p'Y <- P.munch1 Char.isDigit

    let tstr = concat [ p'a, " ", p'b, " ", p'e, " ", p'H, ":", p'M, ":", p'S, " ", p'Z, " ", p'Y ]
    case parseTimeMaybe "%a %b %e %H:%M:%S %Z %Y" tstr of
      Nothing -> fail ("invalid ZonedTime '" ++ tstr ++ "'")
      Just t  -> return t
  where
    digit2 = replicateM 2 P.digit

-- | Returns a list of log entries, however some packages have been uploaded
-- more than once, so each entry is paired with any older entries for the same
-- package.
--
read :: String -> Either String [Entry]
read = check [] . map parseLine . lines
  where
    check es' []           = Right (reverse es')
    check es' (Right e:es) = check (e:es') es
    check _   (Left err:_) = Left err
    parseLine line = maybe (Left err) Right (simpleParse line)
      where err = "Failed to parse log line:\n" ++ show line

collectUploadInfo :: [Entry] -> [(PackageId, UTCTime, UserName)]
collectUploadInfo =
    map (uploadInfo . sortBy (comparing entryTime))
  . groupBy (equating entryPackageId)
  . sortBy (comparing entryPackageId)
  where
    entryPackageId (Entry _  _ pkgid) = pkgid
    entryTime      (Entry t  _ _)     = t

    uploadInfo :: [Entry] -> (PackageId, UTCTime, UserName)
    uploadInfo entries =
      case last entries of
        Entry time uname pkgid -> (pkgid, time, uname)

collectMaintainerInfo :: [Entry] -> [(PackageName, [UserName])]
collectMaintainerInfo =
    map maintainersInfo
  . groupBy (equating entryPackageName)
  . sortBy (comparing entryPackageName)
  where
    entryPackageName (Entry _  _ pkgid) = packageName pkgid

    maintainersInfo :: [Entry] -> (PackageName, [UserName])
    maintainersInfo entries =
        (packageName pkgid, maintainers)
      where
        Entry _ _ pkgid = head entries
        maintainers     = nub [ uname | Entry _ uname _ <- entries ]

