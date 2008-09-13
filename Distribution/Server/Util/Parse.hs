module Distribution.Server.Util.Parse (
    int,
  ) where

import qualified Distribution.Compat.ReadP as Parse
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
