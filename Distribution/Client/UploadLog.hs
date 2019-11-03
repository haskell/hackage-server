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
-- import Distribution.Parsec.Class (Parsec(..))
-- import qualified Distribution.Parsec.Class as P
-- import qualified Distribution.Compat.CharParsing as P
import Distribution.ParseUtils ( parseMaybeQuoted )
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint          as Disp
import Text.PrettyPrint
         ( (<+>) )
import Distribution.Simple.Utils
         ( equating )

import Data.Time.Clock
         ( UTCTime )
import Data.Time.LocalTime
         ( zonedTimeToUTC )
import Data.Time.Format
         ( formatTime )
import Data.Time.Locale.Compat
         ( defaultTimeLocale )
import Data.List
         ( groupBy, nub )

data Entry = Entry UTCTime UserName PackageIdentifier
  deriving (Eq, Ord, Show)

-- TODO: remove this instance for Cabal 3.0
instance Text Entry where
  disp (Entry time user pkgid) =
        Disp.text (formatTime defaultTimeLocale "%c" time)
    <+> disp user <+> disp pkgid
  parse = do
    time <- readPTime' "%c"
    Parse.skipSpaces
    user <- parse
    Parse.skipSpaces
    pkg  <- parseMaybeQuoted parse
    Parse.skipSpaces
    ver  <- parse
    let pkgid = PackageIdentifier pkg ver
    return (Entry (zonedTimeToUTC time) user pkgid)

instance Pretty Entry where
  pretty (Entry time user pkgid) =
        Disp.text (formatTime defaultTimeLocale "%c" time)
    <+> pretty user <+> pretty pkgid

{- TODO

instance Parsec Entry where
  parse = do
    -- parseDateTimeFmt parses "%a %b %e %H:%M:%S %Z %Y",
    time <- parseDateTimeFmt
    P.skipSpaces1
    user <- parsec
    P.skipSpaces1
    pkg  <- parseMaybeQuoted parse
    P.skipSpaces
    ver  <- parsec
    let pkgid = PackageIdentifier pkg ver
    return (Entry (zonedTimeToUTC time) user pkgid)

-}

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

