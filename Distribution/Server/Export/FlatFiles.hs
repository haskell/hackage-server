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
    , permsToCSV
    , uploadsToCSV
    ) where


import Data.Maybe

import Distribution.Server.Export.Utils

import Distribution.Server.Users.Types as Users
import Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Permissions as Permissions

import Distribution.Server.Packages.Types

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
  - secret id numbers are not preserved (such as user ids)

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

      flip map (Users.enumerateAllWithIds users) $ \(user, userInfo) ->

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
           Users.Deleted{}  -> "deleted"
           Users.Disabled{} -> "disabled"
           Users.Enabled{}  -> "enabled"

   -- one of "none" or "basic"
   infoToAuthType :: Users.UserInfo -> String
   infoToAuthType userInfo
       = case Users.userStatus userInfo of
           Users.Deleted{} -> "none"
           _ -> "basic"

   -- may be null
   infoToAuth :: Users.UserInfo -> String
   infoToAuth userInfo
       = case Users.userStatus userInfo of
           Users.Deleted{} -> ""
           Users.Disabled (PasswdHash hash) -> hash
           Users.Enabled  (PasswdHash hash) -> hash



userCSVVer :: Version
userCSVVer = Version [0,1] ["unstable"]

-- permissions.csv
{-| User groups membership.
 -}
permsToCSV :: Permissions.Permissions -> CSV
permsToCSV perms
    = ([showVersion permsCSVVer]:) $
      -- (permsCSVKey:) $

      flip map (Permissions.enumerate perms) . uncurry
        $ \groupName group ->
            (display groupName:) $
              (
               map display . Group.enumerate $ group
              )    
                   

permsCSVVer = userCSVVer


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

uploadsCSVVer = userCSVVer

uploadTimes :: PkgInfo -> [(UTCTime, UserId)]
uploadTimes pkgInfo
    = front : back

 where front = (pkgUploadTime pkgInfo, pkgUploadUser pkgInfo)
       back  = pkgUploadOld pkgInfo
