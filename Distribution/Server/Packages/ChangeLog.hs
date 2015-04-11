{-# LANGUAGE NamedFieldPuns #-}
module Distribution.Server.Packages.ChangeLog (
    findChangeLog
  ) where

import Data.TarIndex (TarIndex, TarEntryOffset)
import qualified Data.TarIndex as TarIndex
import Distribution.Server.Packages.Types (PkgInfo)
import Distribution.Package (packageId)
import Distribution.Text (display)

import System.FilePath ((</>), splitExtension)
import Data.Char as Char
import Data.Maybe


findChangeLog :: PkgInfo -> TarIndex -> Maybe (TarEntryOffset, String)
findChangeLog pkg index = do
    let topdir = display (packageId pkg)
    TarIndex.TarDir fnames <- TarIndex.lookup index topdir
    listToMaybe
      [ (offset, fname')
      | (fname, _) <- fnames
      , isChangelogFile fname
      , let fname' = topdir </> fname
      , Just (TarIndex.TarFileEntry offset) <- [TarIndex.lookup index fname'] ]
  where
    isChangelogFile fname = 
      let (base, ext) = splitExtension fname
       in map Char.toLower base `elem` basenames
       && ext `elem` extensions

    basenames  = ["changelog", "change_log", "changes"]
    extensions = ["", ".txt", ".md", ".markdown"]

