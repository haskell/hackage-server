{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.TagsFile
-- Copyright   :  (c) Duncan Coutts 2012
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
--
-- Support for reading the tags files of the old hackage server.
-----------------------------------------------------------------------------
module Distribution.Client.TagsFile (
    Entry,
    read,
    collectDeprecated,
  ) where

import Distribution.Package
         ( PackageName, PackageId, packageName )
import Distribution.Text
         ( simpleParse )
import Distribution.Simple.Utils
         ( comparing, equating )

import Data.List
         ( foldl', sortBy, groupBy, stripPrefix )
import System.FilePath
         ( splitDirectories )

import Prelude hiding (read)

data Entry = Entry PackageId Bool (Maybe PackageName)
  deriving (Eq, Ord, Show)

-- | Returns a list of log entries, however some packages have been uploaded
-- more than once, so each entry is paired with any older entries for the same
-- package.
--
read :: FilePath -> String -> Either String Entry
read filepath content
  | ("tags":verstr:namestr:_) <- reverse (splitDirectories filepath)
  , Just pkgid <- simpleParse (namestr ++ "-" ++ verstr)
  = Right $! foldl' accum (Entry pkgid False Nothing) (lines content)

  | otherwise
  = Left $ "cannot get a package id from the file name " ++ filepath
  where
    accum e@(Entry pkgid deprecated replacement) s
      | Just "true" <- stripPrefix "deprecated: " s
      = Entry pkgid True replacement

      | Just newnamestr <- stripPrefix "superseded by: " s
      , Just newpkg     <- simpleParse newnamestr
      = Entry pkgid deprecated (Just newpkg)

      | otherwise
      = e

collectDeprecated :: [Entry] -> [(PackageName, Maybe PackageName)]
collectDeprecated =
    map    (\(Entry pkgid _ replacement) -> (packageName pkgid, replacement))
  . filter (\(Entry _ deprecated _) -> deprecated)
  . map last
  . groupBy (equating (\(Entry pkgid _ _) -> packageName pkgid))
  . sortBy (comparing (\(Entry pkgid _ _) -> pkgid))

