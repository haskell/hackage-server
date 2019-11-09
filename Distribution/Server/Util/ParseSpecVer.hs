{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Server.Util.ParseSpecVer
  ( parseSpecVer
  , parseSpecVerLazy
  , scanSpecVersion
  , scanSpecVersionLazy
  , parseGenericPackageDescriptionChecked
  ) where

import           Distribution.Server.Prelude

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.ByteString.Lazy.Char8 as BC8L
import           Distribution.Text
import           Distribution.Pretty ( prettyShow )
import           Distribution.Parsec ( PWarning, PError )
import           Distribution.Version
import qualified Data.HashMap.Strict   as Map
import           Foreign.C
import           Foreign.Ptr
import           System.IO.Unsafe
import Distribution.PackageDescription ( GenericPackageDescription(..), specVersion )
import Distribution.PackageDescription.Parsec ( scanSpecVersion )

#if defined(MIN_VERSION_cabal_parsers)
import           Cabal.Parser (compatParseGenericPackageDescription)
#else
import           Distribution.PackageDescription.Parsec ( runParseResult, parseGenericPackageDescription )
#endif

-- | Heuristic @cabal-version:@-field parser
--
-- This parser is intended to be very fast and assumes a sane & valid .cabal file
-- (i.e. one which can be parsed successfully by 'parseGenericPackageDescription')
--
-- Hackage shall only accept packages and files for which
-- 'parseGenericPackageDescription' and 'parseSpecVer' agree on the
-- inferred 'specVersion'. IOW, Hackage ensures on upload that only
-- @.cabal@ files are accepted which support the heuristic parsing.
--
-- If no valid version field can be found, @mkVersion [0]@ is returned.
parseSpecVer :: ByteString -> Version
parseSpecVer = maybe (mkVersion [0]) id . findCabVer

parseSpecVerLazy :: BSL.ByteString -> Version
parseSpecVerLazy = parseSpecVer . BSL.toStrict

isWS :: Word8 -> Bool
isWS = (`elem` [0x20,0x09])

eatWS :: ByteString -> ByteString
eatWS = BS.dropWhile isWS

-- | Try to heuristically locate & parse a 'cabal-version' field
findCabVer :: ByteString -> Maybe Version
findCabVer raw = msum [ decodeVer y | (_,_,y) <- findCabVers raw ]

-- | Return list of @cabal-version@ candidates as 3-tuples of
-- (prefix,indentation-level,value-words) in reverse order of
-- occurrence.
--
-- A necessary condition for a @cabal-version@ field to be valid is
-- that it matches the following regexp case-insensitively:
--
-- > ^[ \t]*cabal-version[ \t]*:
--
-- IOW, the token @cabal-version@ must appear on a line prefixed only
-- by 0 or more whitespace, and between the @cabal-version@ and the
-- @:@ token may only exist whitespace.
--
-- NB: Later occurrences of @cabal-version@ override earlier ones. In
--     future @cabal-versions@ it will be disallowed.
findCabVers :: ByteString -> [(ByteString,Int,[ByteString])]
findCabVers buf0 = mapMaybe go ixs
  where
    go i
      | BS.null post          = Nothing -- premature EOF
      | BS.head post /= 0x3a  = Nothing -- ':' missing on same line
      | not (BS.null pre'), BS.last pre' /= 0x0a = Nothing -- non-WS on line before 'cabal-version' token
      | otherwise             = Just (pre',indLvl,concatMap BC8.words postLs)
      where
        (pre,post) = fmap (eatWS . BS.drop 13) $ BS.splitAt i buf0
        (pre',indStr) = BS.spanEnd isWS pre

        indLvl = BS.length indStr

        postLs = case BC8.lines $ eatWS $ BS.tail post of
          [] -> []
          -- removes indentation from hanging lines and also comment-lines
          (l0:l') -> (l0 :) $
                     map snd $
                     takeWhile (\(j,_) -> j > indLvl) $
                     filter (\(_,x) -> isNonComment x) $
                     map getInd l'

    -- split off indentation for single line
    getInd :: ByteString -> (Int,ByteString)
    getInd x = case BS.span isWS x of (i,r) -> (BS.length i,r)

    isNonComment = not . BS.isPrefixOf "--"

    ixs :: [Int]
    ixs = strCaseStrAll buf0 "cabal-version"

-- | Lookup-table mapping "x.y.z" strings to 'Version'
verDictV :: Map.HashMap ByteString Version
verDictV = Map.fromList [ (BC8.pack (prettyShow v), v) | v <- knownVers ]

-- | Lookup-table mapping ">=x.y.z" strings to 'Version'
verDictRg :: Map.HashMap ByteString Version
verDictRg = Map.fromList [ (">=" <> BC8.pack (prettyShow v), v) | v <- knownVers ]

-- | List of cabal-version values contained in Hackage's package index as of 2017-07
knownVers :: [Version]
knownVers = map mkVersion
    [ [0,2]
    , [1,0]
    , [1,1]
    , [1,1,3]
    , [1,1,4]
    , [1,1,6]
    , [1,2,0,0]
    , [1,2,0]
    , [1,2,1]
    , [1,2,2]
    , [1,2,3,0]
    , [1,2,3,1]
    , [1,2,3]
    , [1,2,3]
    , [1,2,4]

    , [1,2]
    , [1,2]

    , [1,3]

    , [1,4,0,0]
    , [1,4,0]
    , [1,4]
    , [1,4]

    , [1,5,5]
    , [1,5]

    , [1,6,0,1]
    , [1,6,0,2]
    , [1,6,0,3]
    , [1,6,0]
    , [1,6]
    , [1,6]

    , [1,7]

    , [1,8,0,2]
    , [1,8,0,4]
    , [1,8,0,6]
    , [1,8,0]
    , [1,8,1]
    , [1,8]
    , [1,8]
    , [1,8]

    , [1,9,1]
    , [1,9,2]
    , [1,9]

    , [1,10,0]
    , [1,10,1,0]
    , [1,10,1]
    , [1,10,2,0]
    , [1,10,2]
    , [1,10]

    , [1,12]
    , [1,12]

    , [1,14,0]
    , [1,14]

    , [1,16,0,3]
    , [1,16,0]
    , [1,16,0]
    , [1,16]

    , [1,17]

    , [1,18,0,2]
    , [1,18,0,4]
    , [1,18,0]
    , [1,18,1]
    , [1,18]

    , [1,19]

    , [1,20,0,2]
    , [1,20,0]
    , [1,20]

    , [1,21]

    , [1,22,0]
    , [1,22,1,1]
    , [1,22,5]
    , [1,22]

    , [1,23]

    , [1,24,0,0]
    , [1,24]
    , [1,24]

    , [2,0]
    ]

-- | Fast decoder
decodeVer :: [ByteString] -> Maybe Version
decodeVer ws = case ws of
  [">=",v] -> Map.lookup v verDictV      -- most common case
  [v]      -> Map.lookup v verDictRg <|> -- most common case
              Map.lookup v verDictV <|>
              decodeVerFallback v
  _        -> decodeVerFallback (mconcat ws)

-- | Fallback parser for when lookup-table based parsing fails
decodeVerFallback :: ByteString -> Maybe Version
decodeVerFallback v0 = simpleParse v <|> parseSpecVR
  where
    parseSpecVR = do
        vr <- simpleParse v
        case asVersionIntervals vr of
          []                            -> Just $ mkVersion [0]
          ((LowerBound version _, _):_) -> Just $ version

    v = BC8.unpack v0

-- Warning: this function is locale sensitive!
foreign import ccall unsafe "string.h strcasestr" c_strcasestr :: Ptr CChar -> Ptr CChar -> IO (Ptr CChar)

-- | Find indices (in reverse order) of all non-overlapping
-- case-insensitive occurrences of s2 in s1
{-# NOINLINE strCaseStrAll  #-}
strCaseStrAll :: ByteString -> ByteString -> [Int]
strCaseStrAll s1 s2
    | BS.null s1 || BS.null s2 = []
    | BS.elem 0 s1 || BS.elem 0 s2 = undefined
    | otherwise = unsafePerformIO $ do
          BS.useAsCString s2 $ \s2p ->
            BS.useAsCString s1 $ \s1p ->
              go0 s2p s1p
  where
    needleSz = BS.length s2

    go0 needle hay0 = go hay0 []
      where
        go hay' acc = do
          m <- c_strcasestr hay' needle
          if m == nullPtr then
            return acc
          else do
            let !ofs = m `minusPtr` hay0
            go (m `plusPtr` needleSz) (ofs:acc)


-- | Lazy 'BSL.ByteString' version of 'scanSpecVersion'
scanSpecVersionLazy :: BSL.ByteString -> Maybe Version
scanSpecVersionLazy bs = do
    fstline':_ <- pure (BC8L.lines bs)
    scanSpecVersion (BSL.toStrict fstline')


-- | Version of 'parseGenericPackageDescription' which also validates spec-version heuristics
--
-- * Result of 'parseSpecVerLazy' must agree with 'parseGenericPackageDescription'
-- * If 'scanSpecVersionLazy' detects a version, then it must agree with 'parseGenericPackageDescription' as well
-- * Starting with cabal-version:2.2 'scanSpecVersionLazy' must succeed
--
-- 'True' is returned in the first element if sanity checks passes.
parseGenericPackageDescriptionChecked :: BSL.ByteString -> (Bool,[PWarning], Either (Maybe Version, [PError]) GenericPackageDescription)
parseGenericPackageDescriptionChecked bs = case parseGenericPackageDescription' bs of
   (warns, pe@(Left _)) -> (False, warns, pe)
   (warns, Right gpd)   -> (isOk (specVersion (packageDescription gpd)),warns, Right gpd)
 where
   isOk :: Version -> Bool
   isOk v
     | v /= parseSpecVerLazy bs           = False
     | Just v' <- scanSpecVersionLazy bs  = v == v'
     | otherwise                          = v < mkVersion [2,3]

#if defined(MIN_VERSION_cabal_parsers)
   parseGenericPackageDescription' bs' = compatParseGenericPackageDescription (BSL.toStrict bs')
#else
   parseGenericPackageDescription' bs' = runParseResult (parseGenericPackageDescription (BSL.toStrict bs'))
#endif
