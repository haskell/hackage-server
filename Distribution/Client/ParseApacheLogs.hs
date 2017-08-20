{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- Extract download counts from Apache log files
module Distribution.Client.ParseApacheLogs
  ( logToDownloadCounts
  ) where

-- TODO: We assume the Apache log files are ASCII, not Unicode.
import Distribution.Server.Prelude

import Distribution.Package (PackageName)
import Distribution.Version (Version)
import Distribution.Text    (display, simpleParse)

import Data.List (intercalate)
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Map (Map)
import Data.Time.Calendar (Day)
import qualified Data.ByteString.Char8      as SBS
import qualified Data.Attoparsec.ByteString.Char8 as Att
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map                   as Map

logToDownloadCounts :: LBS.ByteString -> LBS.ByteString
logToDownloadCounts =
    LBS.unlines
  . map formatOutput
  . Map.toList
  . accumHist
  . catMaybes
  . map ((packageGET >=> parseGET) . parseLine . SBS.concat . LBS.toChunks)
  . LBS.lines

data LogLine = LogLine {
      _getIP     :: !SBS.ByteString
    , _getIdent  :: !SBS.ByteString
    , _getUser   :: !SBS.ByteString
    , getDate    :: !SBS.ByteString
    , getReq     :: !SBS.ByteString
    , _getStatus :: !SBS.ByteString
    , _getBytes  :: !SBS.ByteString
    , _getRef    :: !SBS.ByteString
    , _getUA     :: !SBS.ByteString
} deriving (Ord, Show, Eq)

plainValue :: Parser SBS.ByteString
plainValue = Att.takeWhile1 (\c -> c /= ' ' && c /= '\n') --many1' (noneOf " \n")

bracketedValue :: Parser SBS.ByteString
bracketedValue = do
    Att.char '['
    content <- Att.takeWhile1 (\c -> c /= ']') --many' (noneOf "]")
    Att.char ']'
    return content

quotedValue :: Parser SBS.ByteString
quotedValue = do
    Att.char '"'
    content <- Att.takeWhile1 (\c -> c /= '"') --many' (noneOf "\"")
    Att.char '"'
    return content

logLine :: Parser LogLine
logLine = do
    ip     <- plainValue     ; Att.skipSpace
    ident  <- plainValue     ; Att.skipSpace
    usr    <- plainValue     ; Att.skipSpace
    date   <- bracketedValue ; Att.skipSpace
    req    <- quotedValue    ; Att.skipSpace
    status <- plainValue     ; Att.skipSpace
    bytes  <- plainValue     ; Att.skipSpace
    ref    <- quotedValue    ; Att.skipSpace
    ua     <- quotedValue
    return $! LogLine ip ident usr date req status bytes ref ua

parseLine :: SBS.ByteString -> Either SBS.ByteString LogLine
parseLine line = case Att.parseOnly logLine line of
                   Left _    -> Left line
                   Right res -> Right res

packageGET :: Either a LogLine -> Maybe (SBS.ByteString, SBS.ByteString, SBS.ByteString)
packageGET (Right logline)
  | [method, path, _] <- SBS.words (getReq logline)
  , method == methodGET
  , [root, dir1, dir2, name, ver, tarball] <- SBS.split '/' path
  , SBS.null root, dir1 == packagesDir, dir2 == archiveDir
  , SBS.isSuffixOf targzExt tarball
  = Just (name, ver, getDate logline)
packageGET _ = Nothing

parseGET :: (SBS.ByteString, SBS.ByteString, SBS.ByteString) -> Maybe (PackageName, Version, Day)
parseGET (pkgNameStr, pkgVersionStr, dayStr) = do
  name    <- simpleParse . SBS.unpack $ pkgNameStr
  version <- simpleParse . SBS.unpack $ pkgVersionStr
  day     <- parseTimeMaybe "%d/%b/%Y:%T %z" . SBS.unpack $ dayStr
  return (name, version, day)

methodGET, packagesDir, archiveDir, targzExt :: SBS.ByteString
methodGET   = SBS.pack "GET"
packagesDir = SBS.pack "packages"
archiveDir  = SBS.pack "archive"
targzExt    = SBS.pack ".tar.gz"

accumHist :: Ord k => [k] -> Map k Int
accumHist es = Map.fromListWith (+) [ (pkgId,1) | pkgId <- es ]

formatOutput :: ((PackageName, Version, Day), Int) -> LBS.ByteString
formatOutput ((name, version, day), numDownloads) =
  LBS.pack $ intercalate "," $ map show [ display name
                                        , show day
                                        , display version
                                        , show numDownloads
                                        ]
