module Distribution.Server.Packages.Readme (
    isReadmeFile
  ) where

import System.FilePath (splitExtension)
import Data.Char as Char

isReadmeFile :: FilePath -> Bool
isReadmeFile fname = map Char.toLower base `elem` basenames
                        && ext `elem` extensions
  where
    (base, ext) = splitExtension fname
    basenames  = ["readme"]
    extensions = ["", ".txt", ".html", ".md", ".markdown"]
