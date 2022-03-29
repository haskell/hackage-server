-- | Miscellaneous utility functions
{-# LANGUAGE ViewPatterns #-}
module Util (
    explode
  , trim
  , info
  , decodeJSON
  ) where

import Data.Char
import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as LBS

-- | > explode ',' "abc,def,ghi" == ["abc", "def", "ghi"]
explode :: Eq a => a -> [a] -> [[a]]
explode x (break (== x) -> (xs1, []))        = [xs1]
explode x (break (== x) -> (xs1, (_ : xs'))) = xs1 : explode x xs'
explode _ _ = fail "the impossible happened"

-- | Remove leading and trailing whitespace
trim :: String -> String
trim = reverse . dropWhile isSpace
     . reverse . dropWhile isSpace

info :: String -> IO ()
info str = putStrLn ("= " ++ str)

decodeJSON :: FromJSON a => String -> IO a
decodeJSON str =
  case decode (LBS.pack str) of
    Nothing     -> fail "Could not decode JSON"
    Just result -> return result
