{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.DistroMap
-- Copyright   :  (c) Duncan Coutts 2012
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
--
-- Support for reading the distromap files of the old hackage server.
-----------------------------------------------------------------------------
module Distribution.Client.DistroMap (
    Entry(..),
    read,
    toCSV,
  ) where

import Distribution.Package
         ( PackageName )
import Distribution.Version
         ( Version )
import Distribution.Text
         ( display, simpleParse )

import Text.CSV
         ( CSV )
import Network.URI
         ( URI, parseURI )
import Data.Either
         ( partitionEithers )

import Prelude hiding (read)

data Entry = Entry PackageName Version (Maybe URI)
  deriving (Eq, Show)

-- | Returns a list of log entries.
--
read :: String -> ([String], [Entry])
read = partitionEithers . map parseLine . lines
  where
    parseLine line
      | [((pkgnamestr, pkgverstr, murlstr),_)] <- reads line
      , Just pkgname <- simpleParse pkgnamestr
      , Just pkgver  <- simpleParse pkgverstr
      , Just murl    <- maybe (Just Nothing) (fmap Just . parseURI) murlstr
      = Right (Entry pkgname pkgver murl)

      | otherwise
      = Left err
      where
        err = "Failed to parse distro map line:\n" ++ show line

toCSV :: [Entry] -> CSV
toCSV = map $ \(Entry pkgname pkgver murl) ->
                [display pkgname, display pkgver, maybe "" show murl]

