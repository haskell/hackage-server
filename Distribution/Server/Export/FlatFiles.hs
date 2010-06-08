{-| Many of the files written out by the export
  functionality are either already a binary file
  (such as the uploaded tar.gz source packages)
  or the information already has a sepcified format
  (such as the .cabal files).

  For the rest of the data we format it to CSV
  files.
 -}
module Distribution.Server.Export.FlatFiles
    ( usersToCSV
--    , permsToCSV
    , uploadsToCSV
    , distroToCSV
    ) where


import Distribution.Server.Export.Utils

import Distribution.Server.Users.Types as Users
import Distribution.Server.Users.Users as Users
import Distribution.Server.Users.UserBackup

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

-- uploads.csv
{-| For a particular package, when and by whom was it
  uploaded.
 -}
uploadsToCSV :: PkgInfo -> CSV
uploadsToCSV pkgInfo
    = ([showVersion uploadsCSVVer]:) $
      (uploadsCSVKey:) $

      flip map (uploadTimes pkgInfo) . uncurry $ \time user ->
          [ display user
          , formatTime defaultTimeLocale timeFormatSpec time
          ]

 where      
      uploadsCSVKey
          = [ "user"
            , "time"
            ]

uploadsCSVVer :: Version
uploadsCSVVer = Version [0,1] ["unstable"]

uploadTimes :: PkgInfo -> [(UTCTime, UserId)]
uploadTimes pkgInfo
    = [front]

 where front = pkgUploadData pkgInfo
--       back  = pkgUploadOld pkgInfo

distroToCSV :: DistroName -> DistroVersions -> CSV
distroToCSV distro distInfo
    = let stats = Distros.distroStatus distro distInfo
      in ([showVersion distrosCSVVer]:) $
         ([display distro]:) $
         (distrosCSVKey:) $

         flip map stats . uncurry $
           \packageName (DistroPackageInfo version url) ->
               [display packageName, showVersion version, url]
 where
   distrosCSVKey
       = [ "package"
         , "version"
         , "url"
         ]

distrosCSVVer :: Version
distrosCSVVer = Version [0,1] ["unstable"]
