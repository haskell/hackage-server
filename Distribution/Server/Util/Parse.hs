module Distribution.Server.Util.Parse (
    int, unpackUTF8
  ) where

import qualified Distribution.Compat.ReadP as Parse

import qualified Data.Char as Char
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy          as Text
import qualified Data.Text.Lazy.Encoding as Text

-- | Parse a positive integer. No leading @0@'s allowed.
--
int :: Parse.ReadP r Int
int = do
  first <- Parse.satisfy Char.isDigit
  if first == '0'
    then return 0
    else do rest <- Parse.munch Char.isDigit
            return (read (first : rest))

unpackUTF8 :: ByteString -> String
unpackUTF8 = Text.unpack . Text.decodeUtf8
