module Distribution.Server.Util.Parse (
    int, unpackUTF8
  ) where

import qualified Distribution.Compat.ReadP as Parse
import Distribution.Simple.Utils ( fromUTF8 )

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Char as Char

-- | Parse a positive integer. No leading @0@'s allowed.
--
int :: Parse.ReadP r Int
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

unpackUTF8 :: BS.ByteString -> String
unpackUTF8 = ignoreBOM . fromUTF8 . BS.unpack
