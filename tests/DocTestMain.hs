-- | `doctest-parallel` runner for `hackage-server` library.
module Main where

import System.Environment
         ( getArgs )
import Test.DocTest
         ( mainFromLibrary )
import Test.DocTest.Helpers
         ( extractSpecificCabalLibrary, findCabalPackage )

-- | Doctest @hackage-server:lib-server@.
main :: IO ()
main = do
  args <- getArgs
  pkg  <- findCabalPackage "hackage-server"
  -- Need to give the library name, otherwise the parser does not find it.
  lib  <- extractSpecificCabalLibrary Nothing pkg
  mainFromLibrary lib args
