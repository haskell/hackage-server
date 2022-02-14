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
import Distribution.Server.Features.LegacyPasswds

import Distribution.Server.Users.Types
import qualified Distribution.Server.Users.Users as Users
import Distribution.Text
import Distribution.Simple.Utils (lowercase)

import Data.List
import qualified Data.Text as T
import Control.Applicative (optional)
import Data.Maybe (isJust)


-- | An HTML UI for various server admin tasks, mostly user accounts
--
initAdminFrontendFeature :: ServerEnv
                         -> IO (UserFeature
                             -> UserDetailsFeature
                             -> UserSignupFeature
                             -> LegacyPasswdsFeature
                             -> IO HackageFeature)
initAdminFrontendFeature env@ServerEnv{ serverTemplatesDir,
                                        serverTemplatesMode } = do
    -- Page templates
    templates <- loadTemplates serverTemplatesMode
                   [serverTemplatesDir, serverTemplatesDir </> "AdminFrontend"]
                   [ "admin.html", "accounts.html", "account.html"
                   , "signups.html", "resets.html", "legacy.html" ]

    return $ \user userdetails usersignup legacypasswds -> do
      let feature = adminFrontendFeature env templates
                                         user userdetails
                                         usersignup legacypasswds

      return feature


adminFrontendFeature :: ServerEnv -> Templates
                     -> UserFeature
                     -> UserDetailsFeature
                     -> UserSignupFeature
                     -> LegacyPasswdsFeature
                     -> HackageFeature
adminFrontendFeature _env templates
                     UserFeature{..} UserDetailsFeature{..}
                     UserSignupFeature{..} LegacyPasswdsFeature{..} =
  (emptyHackageFeature "admin-frontend") {
    featureResources =
      [ adminPortalResource
      , adminAccountsResource
      , adminAccountResource
      , adminSignupsResource
      , adminResetsResource
      , adminLegacyResource
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
    adminSignupsResource =
      (resourceAt "/admin/signups")  {
        resourceDesc = [(GET,  "All account signup requests")],
        resourceGet  = [("html", serveAdminSignupsGet)]
      }
    adminResetsResource =
      (resourceAt "/admin/resets")  {
        resourceDesc = [(GET,  "All password reset requests")],
        resourceGet  = [("html", serveAdminResetsGet)]
      }
    adminLegacyResource =
      (resourceAt "/admin/legacy")  {
        resourceDesc = [(GET,  "All accounts with legacy passwords")],
        resourceGet  = [("html", serveAdminLegacyGet)]
      }

    serveAdminPortalGet :: DynamicPath -> ServerPartE Response
    serveAdminPortalGet _ = do
        guardAuthorised_ [InGroup adminGroup]
        cacheControlWithoutETag [Private]
        template    <- getTemplate templates "admin.html"

        findAccount <- optional (look "find-account")
        accounts    <- case findAccount of
                         Nothing          -> return []
                         Just searchTerms -> do
                           users <- queryGetUserDb
                           return (accountSearch users searchTerms)

        findSignup <- optional (lookText' "find-signup")
        signups    <- case findSignup of
                         Nothing          -> return []
                         Just searchTerms -> do
                           allSignupInfo <- queryAllSignupResetInfo
                           return (signupSearch allSignupInfo searchTerms)

        ok $ toResponse $ template
          [ "findAccount" $= findAccount
          , "accounts"    $= [ accountBasicInfoToTemplate uid uinfo
                             | (uid, uinfo) <- accounts ]
          , "findSignup"  $= findSignup
          , "signups"     $= map signupRequestToTemplate signups
          ]
      where
        accountSearch users searchTerms =
          [ (uid, uinfo)
          | let terms = map lowercase (words searchTerms)
          , (uid, uinfo@UserInfo{ userName = UserName uname })
              <- Users.enumerateAllUsers users
          , any (`isInfixOf` lowercase uname) terms ]

        signupSearch allSignupInfo searchTerms =
          [ signupInfo
          | let terms = map T.toCaseFold (T.words searchTerms)
          , signupInfo@SignupInfo {
                         signupUserName, signupRealName, signupContactEmail
                       } <- allSignupInfo
          , any (\term -> term `T.isInfixOf` T.toCaseFold signupUserName
                       || term `T.isInfixOf` T.toCaseFold signupRealName
                       || term `T.isInfixOf` T.toCaseFold signupContactEmail)
                terms
          ]

    signupRequestToTemplate SignupInfo { signupUserName, signupRealName,
                                         signupContactEmail, nonceTimestamp } =
      templateDict
        [ templateVal "username"  signupUserName
        , templateVal "realname"  signupRealName
        , templateVal "email"     signupContactEmail
        , templateVal "timestamp" nonceTimestamp
        ]
    signupRequestToTemplate _ = error "signupRequestToTemplate"

    serveAdminAccountsGet :: DynamicPath -> ServerPartE Response
    serveAdminAccountsGet _ = do
        guardAuthorised_ [InGroup adminGroup]
        cacheControlWithoutETag [Private]
        template <- getTemplate templates "accounts.html"
        accounts <- Users.enumerateAllUsers <$> queryGetUserDb
        ok $ toResponse $ template
          [ "accounts" $= [ accountBasicInfoToTemplate uid uinfo
                          | (uid, uinfo) <- accounts ]
          ]

    serveAdminSignupsGet :: DynamicPath -> ServerPartE Response
    serveAdminSignupsGet _ = do
        guardAuthorised_ [InGroup adminGroup]
        cacheControlWithoutETag [Private]
        template      <- getTemplate templates "signups.html"
        allSignupInfo <- queryAllSignupResetInfo
        ok $ toResponse $ template
          [ "signups" $= [ signupRequestToTemplate signupInfo
                         | signupInfo@SignupInfo {} <- allSignupInfo ]
          ]

    serveAdminResetsGet :: DynamicPath -> ServerPartE Response
    serveAdminResetsGet _ = do
        guardAuthorised_ [InGroup adminGroup]
        cacheControlWithoutETag [Private]
        template     <- getTemplate templates "resets.html"
        usersdb      <- queryGetUserDb
        allResetInfo <- queryAllSignupResetInfo
        ok $ toResponse $ template
          [ "resets" $= [ resetRequestToTemplate resetInfo uinfo
                        | resetInfo@ResetInfo {resetUserId} <- allResetInfo
                        , let Just uinfo = Users.lookupUserId resetUserId usersdb ]
          ]

    serveAdminLegacyGet :: DynamicPath -> ServerPartE Response
    serveAdminLegacyGet _ = do
        guardAuthorised_ [InGroup adminGroup]
        cacheControlWithoutETag [Private]
        template     <- getTemplate templates "legacy.html"
        usersdb      <- queryGetUserDb
        legacyUsers  <- enumerateAllUserLegacyPasswd <$> queryLegacyPasswds
        ok $ toResponse $ template
          [ "accounts" $= [ accountBasicInfoToTemplate uid uinfo
                          | uid <- legacyUsers
                          , let Just uinfo = Users.lookupUserId uid usersdb ]
          ]

    resetRequestToTemplate :: SignupResetInfo -> UserInfo -> TemplateVal
    resetRequestToTemplate ResetInfo {nonceTimestamp, resetUserId} uinfo =
      templateDict
        [ templateVal "timestamp" nonceTimestamp
        , templateVal "account" (accountBasicInfoToTemplate resetUserId uinfo)
        ]
    resetRequestToTemplate _ _ = templateDict []

    serveAdminAccountGet :: DynamicPath -> ServerPartE Response
    serveAdminAccountGet dpath = do
        guardAuthorised_ [InGroup adminGroup]
        cacheControlWithoutETag [Private]
        template  <- getTemplate templates "account.html"
        uid       <- maybe mzero return (simpleParse =<< lookup "uid" dpath)
        uinfo     <- lookupUserInfo uid
        mudetails <- queryUserDetails uid
        resetInfo <- lookupPasswordReset uid <$> queryAllSignupResetInfo
        mlegacy   <- lookupUserLegacyPasswd uid <$> queryLegacyPasswds

        ok $ toResponse $ template
          [ "account" $= accountBasicInfoToTemplate uid uinfo
          , "details" $= accountDetailsToTemplate uinfo mudetails
          , "resetRequests"     $= resetInfo
          , "hasLegacyPassword" $= isJust mlegacy
          ]
      where
        lookupPasswordReset uid allResetInfo =
          [ templateDict [ templateVal "timestamp" nonceTimestamp ]
          | ResetInfo { resetUserId, nonceTimestamp }  <- allResetInfo
          , resetUserId == uid ]

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
    accountDetailsToTemplate _ Nothing =
      templateDict
        [ templateVal "kindenum" (templateEnumDesriptor
                                    (maybe "notset" showAccountKind)
                                    (Nothing : map Just [minBound..maxBound])
                                     Nothing)
        ]

    showAccountKind AccountKindRealUser = "RealUser"
    showAccountKind AccountKindSpecial  = "Special"

