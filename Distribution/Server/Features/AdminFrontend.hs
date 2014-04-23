{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards, BangPatterns,
             StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Distribution.Server.Features.AdminFrontend (
    initAdminFrontendFeature
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.Templating

import Distribution.Server.Features.Users
import Distribution.Server.Features.UserDetails
import Distribution.Server.Features.UserSignup

import Distribution.Server.Users.Types
import qualified Distribution.Server.Users.Users as Users
import Distribution.Text
import Distribution.Simple.Utils (lowercase)

import Data.List
import Control.Applicative (optional)

-- | An HTML UI for various server admin tasks, mostly user accounts
--
initAdminFrontendFeature :: ServerEnv
                         -> UserFeature -> UserDetailsFeature
                         -> IO HackageFeature
initAdminFrontendFeature env@ServerEnv{serverTemplatesDir, serverTemplatesMode}
                         user userdetails = do

  -- Page templates
  templates <- loadTemplates serverTemplatesMode
                 [serverTemplatesDir, serverTemplatesDir </> "AdminFrontend"]
                 ["admin.html", "accounts.html", "account.html"]

  let feature = adminFrontendFeature env templates user userdetails

  return feature


adminFrontendFeature :: ServerEnv -> Templates
                     -> UserFeature
                     -> UserDetailsFeature
                     -> HackageFeature
adminFrontendFeature _env templates
                     UserFeature{..} UserDetailsFeature{..} =
  (emptyHackageFeature "admin-frontend") {
    featureResources =
      [ adminPortalResource
      , adminAccountsResource
      , adminAccountResource
      ]
  , featureState = []
  , featureReloadFiles = reloadTemplates templates
  }

  where
    adminPortalResource =
      (resourceAt "/admin")  {
        resourceDesc = [(GET,  "Server admin portal")],
        resourceGet  = [("html", serveAdminPortalGet)]
      }
    adminAccountsResource =
      (resourceAt "/admin/accounts")  {
        resourceDesc = [(GET,  "All user accounts")],
        resourceGet  = [("html", serveAdminAccountsGet)]
      }
    adminAccountResource =
      (resourceAt "/admin/account/:uid")  {
        resourceDesc = [(GET,  "User account info")],
        resourceGet  = [("html", serveAdminAccountGet)]
      }
    

    serveAdminPortalGet :: DynamicPath -> ServerPartE Response
    serveAdminPortalGet _ = do
        guardAuthorised_ [InGroup adminGroup]
        template    <- getTemplate templates "admin.html"

        findAccount <- optional (look "find-account")
        accounts    <- case findAccount of
                         Nothing          -> return []
                         Just searchTerms -> do users <- queryGetUserDb
                                                return (accountSearch users searchTerms)

        ok $ toResponse $ template
          [ "findAccount" $= findAccount
          , "accounts"    $= [ accountBasicInfoToTemplate uid uinfo
                             | (uid, uinfo) <- accounts ]
          ]
      where
        accountSearch users searchTerms =
          [ (uid, uinfo)
          | let terms = map lowercase (words searchTerms)
          , (uid, uinfo@UserInfo{ userName = UserName uname })
              <- Users.enumerateAllUsers users
          , any (`isInfixOf` lowercase uname) terms ]

    serveAdminAccountsGet :: DynamicPath -> ServerPartE Response
    serveAdminAccountsGet _ = do
        guardAuthorised_ [InGroup adminGroup]
        template <- getTemplate templates "accounts.html"
        accounts <- Users.enumerateAllUsers <$> queryGetUserDb

        ok $ toResponse $ template
          [ "accounts" $= [ accountBasicInfoToTemplate uid uinfo
                          | (uid, uinfo) <- accounts ]
          ]

    serveAdminAccountGet :: DynamicPath -> ServerPartE Response
    serveAdminAccountGet dpath = do
        guardAuthorised_ [InGroup adminGroup]
        template  <- getTemplate templates "account.html"
        uid       <- maybe mzero return (simpleParse =<< lookup "uid" dpath)
        uinfo     <- lookupUserInfo uid
        mudetails <- queryUserDetails uid
        ok $ toResponse $ template
          [ "account" $= accountBasicInfoToTemplate uid uinfo
          , "details" $= accountDetailsToTemplate uinfo mudetails
          ]

    accountBasicInfoToTemplate uid uinfo =
      templateDict
        [ templateVal "id"      (display uid)
        , templateVal "name"    (display (userName uinfo))
        , templateVal "active"  (isActiveAccount (userStatus uinfo))
        , templateVal "enabled" (case userStatus uinfo of
                                   AccountEnabled  _        -> True
                                   _                        -> False)
        , templateVal "hasAuth" (case userStatus uinfo of
                                   AccountEnabled  _        -> True
                                   AccountDisabled (Just _) -> True
                                   _                        -> False)
        , templateVal "status"  (case userStatus uinfo of
                                   AccountEnabled  _        -> "enabled"
                                   AccountDisabled Nothing  -> "disabled, no password set"
                                   AccountDisabled (Just _) -> "disabled, with password set"
                                   AccountDeleted           -> "deleted")
        ]

    accountDetailsToTemplate _ Nothing         = templateDict []
    accountDetailsToTemplate uinfo (Just udetails) =
      templateDict
        [ templateVal "realname" (accountName udetails)
        , templateVal "email"    (accountContactEmail udetails)
        , templateVal "kind"     (fmap showAccountKind (accountKind udetails))
        , templateVal "kindenum" (templateEnumDesriptor
                                    (maybe "notset" showAccountKind)
                                    (Nothing : map Just [minBound..maxBound])
                                    (accountKind udetails))
        , templateVal "notes"    (accountAdminNotes udetails)
        , templateVal "canreset" (accountSuitableForPasswordReset uinfo udetails)
        ]
 
    showAccountKind AccountKindRealUser = "RealUser"
    showAccountKind AccountKindSpecial  = "Special"

