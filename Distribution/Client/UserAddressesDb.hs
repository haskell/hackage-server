{-# LANGUAGE BangPatterns, PatternGuards #-}
-- | Parsing @hackage.addresses@ files
--
module Distribution.Client.UserAddressesDb (
    UserAddressesDb,
    UserEntry,
    parseFile
  ) where

import Distribution.Server.Users.Types (UserName(..))
import Data.List
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.Read           as T
import Data.Functor
import Data.Char (chr)

type UserAddressesDb = [UserEntry]
type UserEntry    = (UserName, UserRealName, UserAddress)
type UserRealName = Text
type UserAddress  = Text

parseFile :: FilePath -> IO (Either String UserAddressesDb)
parseFile fn = parse <$> BS.readFile fn

parse :: ByteString -> Either String UserAddressesDb
parse = accum 0 [] . map parseLine . BS.lines
  where
    accum  _ entries []                = Right (reverse entries)
    accum !n entries (Just entry:rest) = accum (n+1) (entry:entries) rest
    accum  n _       (Nothing   :_   ) = Left errmsg
      where
        errmsg = "parse error in addresses file on line " ++ show (n :: Int)

parseLine :: ByteString -> Maybe UserEntry
parseLine line = case splitOn ':' line of
  -- entries like:
  -- DuncanCoutts:Duncan Coutts:duncan.coutts@worc.ox.ac.uk:RossPaterson:Wed Jan 10 16:00:00 PDT 2007
  (username:realname:email:_) -> Just ( UserName (BS.unpack username)
                                      , decodeMixed realname
                                      , decodeMixed email )
  _                           -> Nothing

{-
test = T.unlines . map transLine . BS.lines

transLine :: ByteString -> Text
transLine line = case splitOn ':' line of
  (username:realname:email:rest) ->
    T.intercalate ":" (T.decodeUtf8 username:decodeMixed realname:decodeMixed email: map T.decodeUtf8 rest)
-}

-- Unfortunately the file uses mixed encoding, mostly UTF8
-- but some Latin1 and some Html escape sequences
decodeMixed :: ByteString -> Text
decodeMixed bs
  | T.any ('\xFFFD' ==) astext = unescape (T.pack (BS.unpack bs))
  | otherwise                  = unescape astext
  where
    astext = T.decodeUtf8With T.lenientDecode bs

-- unescape things like "&#287;"
unescape :: Text -> Text
unescape s
  | let (s0,s1) = T.breakOn (T.pack "&#") s
  , not (T.null s1)
  , Right (n,s2) <- T.decimal (T.drop 2 s1)
  = T.append s0 (T.cons (chr n) (T.drop 1 (unescape s2)))

  | otherwise = s

splitOn :: Char -> ByteString -> [ByteString]
splitOn c = unfoldr $ \s -> if BS.null s then Nothing
                                         else case BS.break (==c) s of
                                                (x,s') -> Just (x, BS.drop 1 s')


