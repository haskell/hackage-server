module Distribution.Server.Packages.ChangeLog (
    isChangeLogFile
  ) where


import System.FilePath (splitExtension)
import Data.Char as Char


isChangeLogFile :: FilePath -> Bool
isChangeLogFile fname = map Char.toLower base `elem` basenames
                        && ext `elem` extensions
  where
    (base, ext) = splitExtension fname
    basenames  = ["news", "changelog", "change_log", "changes"]
    extensions = ["", ".txt", ".md", ".markdown"]
