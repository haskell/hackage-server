{-| Many of the files written out by the export
  functionality are either already a binary file
  (such as the uploaded tar.gz source packages)
  or the information already has a sepcified format
  (such as the .cabal files).

  For the rest of the data we format it to CSV
  files.
 -}
module Distribution.Server.Backup.FlatFiles
    ( distroToCSV
    ) where


import Distribution.Server.Backup.Utils

import Distribution.Server.Users.Types as Users
import Distribution.Server.Users.Users as Users

import qualified Distribution.Server.Distributions.Distributions as Distros
import Distribution.Server.Distributions.Distributions
    ( DistroName
    , DistroVersions
    , DistroPackageInfo(..)
    )

import Distribution.Server.Packages.Types
import Distribution.Server.Auth.Types

import Distribution.Text

import Text.CSV
import Data.Version
import Data.Time
import System.Locale

{-

This module is responsible for creating the contents
of the flat-files we need to manufacture as part of the
server export.

A few guiding philosophies:

  - Human readbale/editable is good
  - Machine parasable is also good

 -} 

