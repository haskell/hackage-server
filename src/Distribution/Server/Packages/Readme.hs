module Distribution.Server.Packages.Readme (
    isReadmeFile
  ) where

import System.FilePath (splitExtension)
import Data.Char as Char

isReadmeFile :: FilePath -> Bool
isReadmeFile fname = base `elem` basenames
                     && ext `elem` extensions
  where
    (base, ext) = splitExtension (map Char.toLower fname)
    basenames  = ["readme"]
    extensions = ["", ".txt", ".html", ".md", ".markdown"]
