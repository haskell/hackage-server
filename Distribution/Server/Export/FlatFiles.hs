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

-- auth.csv
{- | Produces a CSV file for the users DB.
   .
   Format:
   .
   User name,User Id,(enabled|disabled|deleted),(basic-auth,no-auth),pwd-hash   
 -}
usersToCSV :: Users.Users -> CSV
usersToCSV users
    = ([showVersion userCSVVer]:) $
      (usersCSVKey:) $

      flip map (Users.enumerateAll users) $ \(user, userInfo) ->

      [ display . Users.userName $ userInfo
      , display user
      , infoToStatus userInfo
      , infoToAuthType userInfo
      , infoToAuth userInfo
      ]

 where
   usersCSVKey =
       [ "name"
       , "id"
       , "status"
       , "auth-type"
       , "auth-info"
       ]

   -- one of "enabled" "disabled" or "deleted"
   infoToStatus :: Users.UserInfo -> String
   infoToStatus userInfo
       = case Users.userStatus userInfo of
           Users.Deleted  -> "deleted"
           Users.Active Users.Disabled _ -> "disabled"
           Users.Active Users.Enabled  _ -> "enabled"

   -- one of "none", "basic", or "digest"
   infoToAuthType :: Users.UserInfo -> String
   infoToAuthType userInfo
       = case Users.userStatus userInfo of
           Users.Deleted -> "none"
           Users.Active _ (Users.UserAuth _ atype)-> case atype of
               BasicAuth -> "basic"
               DigestAuth -> "digest"

   -- may be null
   infoToAuth :: Users.UserInfo -> String
   infoToAuth userInfo
       = case Users.userStatus userInfo of
           Users.Deleted{} -> ""
           Users.Active _ (UserAuth (PasswdHash hash) _) -> hash



userCSVVer :: Version
userCSVVer = Version [0,1] ["unstable"]                   

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
uploadsCSVVer = userCSVVer

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
distrosCSVVer = userCSVVer
