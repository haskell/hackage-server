module Distribution.Server.Packages.ChangeLog (
    isChangeLogFile
  ) where


import System.FilePath (splitExtension)
import Data.Char as Char


isChangeLogFile :: FilePath -> Bool
isChangeLogFile fname = base `elem` basenames
                        && ext `elem` extensions
  where
    (base, ext) = splitExtension (map Char.toLower fname)
    basenames  = ["news", "changelog", "change_log", "changes"]
    extensions = ["", ".txt", ".md", ".markdown"]
