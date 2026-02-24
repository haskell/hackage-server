-- | Parsing and UTF8 utilities
module Distribution.Server.Util.Parse (
    int, unpackUTF8, unpackUTF8Strict, packUTF8
  ) where

import qualified Text.ParserCombinators.ReadP as Parse

import qualified Data.Char as Char
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.Text                as TextStrict
import qualified Data.Text.Encoding       as TextStrict
import qualified Data.Text.Lazy           as TextLazy
import qualified Data.Text.Lazy.Encoding  as TextLazy
import qualified Data.Text.Encoding.Error as Text

-- | Parse a positive integer. No leading @0@'s allowed.
--
int :: Parse.ReadP Int
int = do
  first <- Parse.satisfy Char.isDigit
  if first == '0'
    then return 0
    else do rest <- Parse.munch Char.isDigit
            return (read (first : rest))

-- | Ignore a Unicode byte order mark (BOM) at the beginning of the input
--
-- (Also in Distribution.Simple.Utils, but not exported)
ignoreBOM :: String -> String
ignoreBOM ('\xFEFF':string) = string
ignoreBOM string            = string

unpackUTF8 :: LazyByteString -> String
unpackUTF8 = ignoreBOM . TextLazy.unpack . TextLazy.decodeUtf8With Text.lenientDecode

unpackUTF8Strict :: StrictByteString -> String
unpackUTF8Strict = ignoreBOM . TextStrict.unpack . TextStrict.decodeUtf8With Text.lenientDecode

packUTF8 :: String -> LazyByteString
packUTF8 = TextLazy.encodeUtf8 . TextLazy.pack
