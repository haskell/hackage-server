{-# LANGUAGE FlexibleContexts #-}

module Distribution.Server.Distributions.ServerParts
    ( adminDist
    , distros
    ) where

import Distribution.Server.Pages.Util
import qualified Distribution.Server.Pages.Distributions as Pages

import Distribution.Server.ServerParts

import Distribution.Server.Distributions.State as Dist
import Distribution.Server.Packages.State
import Distribution.Server.Users.State

import Distribution.Server.Distributions.Distributions
import Distribution.Server.Users.Types
import Distribution.Server.Users.Permissions (GroupName(..))

import qualified Distribution.Server.ResourceTypes as Resources

import Distribution.Text
import Distribution.Package (PackageName)

import Happstack.State
import Happstack.Server
import Happstack.Server.SURI hiding (query, path)

import Control.Monad (msum, mzero)
import Control.Monad.Trans (MonadIO)
import qualified Data.ByteString.Char8 as BS

import System.FilePath.Posix ((</>))

import Text.URI (escapeString, okInPath)


{-| Administrator entry points. Assumes that permissions
  have already been verified. These parts are for driving
  web-forms and the like.

  \/distro
     -- page listing known distros allowing:
      * creating a new distro
      * opening a specific distros admin page

  \/distro\/\<distroname\>
     -- page listing all members of a distro allowing:
      * add a member
      * remove any member
      * delete the distro
      * edit the distro description
      * create new distro with same mebers?

 \/distro\/\<distroname\>\/delete
    -- POST delete a created distribution

 \/createDistro
    -- POST create a new distribution
     name = \<distroname\>
 -}
adminDist :: ServerPart Response
adminDist
    = msum
      [ dir "distro" $ msum
         [ methodSP GET $ adminHomePage
         , withDistro $ \dist -> msum
            [ methodSP GET $
              adminDistroPage dist
            , dir "delete" $ methodSP POST $ do
                delete dist
                seeOther "/admin/distro" $ toResponse "Ok!"
            ]
         ]
      , dir "createDistro" $ methodSP POST $
        lookDistroName create
      ]



{-| End user entry points. This function performs all
  need permission verification.

The first are a set of REST-style requests, intended
to be usable from command-line tools such as curl and
wget.

\/\<distro\>
      -- PUT request to create a distribution
         This only succeeds if not present
         This requires admin security

      -- DELETE request to delete the specified
         distirbution. Requires admin security


\/\<distro\>\/\<packageName\>
      -- POST request to add the package as being
         in this distribution.

         Parameters delivered as form data:
         version: required
         uri: optional

         This reqest requires maintainer security

     -- DELETE request to declare that this package
        is not being distributed by this distro

        Requires distro maintainer security

The following are intended for use from
web-forms or for generating user-facing
HTML.

 \/    -- listing of all distros, with a link
        to the per-distro page

 \/\<distro\>
      -- GET lists packages in distro
       * Also a link to a distro admin page

 \/\<distro\>\/admin
      -- GET admin page for end users

 \/\<distro\>\/admin/addMember
      -- POST request with key:
        userName = \<user name\>
       * adds the user as a maintainer

 \/\<distro\>\/admin\/removeMember
      -- POST request with key:
        userName = \<user name\>
       * removes the user as a maintainer

 \/\<distro\>\/admin\/addPackage
      -- POST request with key:
        package = \<package name\>
        version = \<version\>
        uri = URL for package in distribution
       * adds the package as tracked at the
         specified version
 -}
distros :: ServerPart Response
distros
    = msum
      [ methodSP GET homePage


      , withText $ \distroName ->
          msum
          [ methodSP PUT $ do
              guardAuth [Administrator]
              create distroName
          ]


      , withDistro $ \dist ->
        msum
        [ methodSP GET $ distroPage dist
  
        -- REST-style API
        , methodSP DELETE $ do
            guardAuth [Administrator]
            delete dist
            ok $ toResponse "Ok!"

        , dir "package" $ do
            guardAuth [Administrator, DistroMaintainer dist]
            withText $ \pName ->
             msum
              [ methodSP DELETE $ deletePackage dist pName
              , methodSP POST   $ addPackagePart dist pName
              ]

        -- web forms
        , dir "admin" $ do
            guardAuth [Administrator, DistroMaintainer dist]

            msum
             [ methodSP GET $ distroAdminPage dist
             , dir "addMember" $ methodSP POST $ addMember dist
             , dir "removeMember" $ methodSP POST $ removeMember dist
             , dir "addPackage" $ methodSP POST $ addPackageForm dist
             ]
        ]
      ]

-- Pages

-- | Front end for administrating distributions.
-- Targeted towards hackage admins.
adminHomePage :: ServerPart Response
adminHomePage = do
  distNames <- map snd `fmap` query Dist.Enumerate
  ok $ toResponse $ Resources.XHtml $ Pages.adminHomePage distNames

-- | Admin page for a particular distribution.
-- Targeted towards hackage admins.
adminDistroPage :: DistroId -> ServerPart Response
adminDistroPage dist = do
  Just distName <- query $ LookupDistroName dist
  distGroup <- query $ LookupUserGroup $ DistroMaintainer dist
  distUsers <- query $ ListGroupMembers distGroup
  ok $ toResponse $ Resources.XHtml $ Pages.adminDistroPage distName distUsers

-- | Listing of a all known distros.
-- Public.
homePage :: ServerPart Response
homePage = do
  distNames <- map snd `fmap` query Dist.Enumerate
  ok $ toResponse $ Resources.XHtml $ Pages.homePage distNames

-- | Public page for a particular distro
distroPage :: DistroId -> ServerPart Response
distroPage dist
    = do
  Just distName <- query $ LookupDistroName dist
  distPkgs <- query $ DistroStatus dist
  ok $ toResponse $ Resources.XHtml $
    Pages.distroListing distName distPkgs $
    "/distro" </> displayDir distName </> "admin"

-- | Admin page for a particular distro.
-- For distro maintainer.
distroAdminPage :: DistroId -> ServerPart Response
distroAdminPage dist = do
  Just distName <- query $ LookupDistroName dist
  distGroup <- query $ LookupUserGroup $ DistroMaintainer dist
  distUsers <- query $ ListGroupMembers distGroup
  ok $ toResponse $ Resources.XHtml $ Pages.distroPage distName distUsers


-- Actions

-- | POST request to create a new distribution.
-- The input param 'distroName' must be
-- supplied. The caller is responsible for verifying
-- permissions
create :: DistroName -> ServerPart Response
create distroName
    = do
  mdistro <- update $ AddDistro distroName
  case mdistro of
    Nothing
        -> ok $ toResponse $
           hackageError "Selected distribution name is already in use"
    Just{}
        -> seeOther ("/admin/distro/" ++ displayDir distroName) $
           toResponse "Ok!"

displayDir :: Text a => a -> String
displayDir = escapeString pred . display
 where pred c = okInPath c && c /= '/'


-- | POST request to remove a distribution.
-- The caller is responsible for verifying
-- permissions.
delete :: MonadIO m => DistroId -> m ()
delete distro
    = update $ RemoveDistro distro
      
deletePackage :: DistroId -> PackageName -> ServerPart Response
deletePackage distro packageName
    = do
  update $ DropPackage distro packageName
  ok $ toResponse "Ok!"
      

addMember :: DistroId -> ServerPart Response
addMember distro
    = lookUserId $ \user -> do
      update $ AddToGroup (DistroMaintainer distro) user
      nextUri <- bounceUri "/distro"
      seeOther nextUri $ toResponse "Ok!"

removeMember :: DistroId -> ServerPart Response
removeMember distro
    = lookUserId $ \user -> do
      update $ RemoveFromGroup (DistroMaintainer distro) user
      nextUri <- bounceUri "/distro"
      seeOther nextUri $ toResponse "Ok!"

addPackageForm :: DistroId -> ServerPart Response
addPackageForm dist
    = lookPackageName $ addPackagePart dist

addPackagePart :: DistroId -> PackageName -> ServerPart Response
addPackagePart dist packageName =
      lookPackageInfo $ \distInfo -> do
       update $ AddPackage dist packageName distInfo
       nextUri <- bounceUri "/distro"
       seeOther nextUri $ toResponse "Ok!"

-- Combinators for server-parts, utility functions

-- |Pop a distro name off of the path, lookup the corresponding
-- id. Returns 404 on failure to find distro.
withDistro :: (DistroId -> ServerPart a) -> ServerPart a
withDistro k
    = withText $ \distName -> do
        mdist <- query $ LookupDistroId distName
        case mdist of
          Nothing   -> finishNotFound $ toResponse $
                       hackageNotFound "No such distribution"
          Just dist -> k dist

-- |Pop a parsable thing off of the path.
withText :: Text t => (t -> ServerPart a) -> ServerPart a
withText k
    = path $ \str -> do
      case simpleParse str of
        Nothing -> mzero
        Just text -> k text

-- | Form validation for entering a distribution name
-- This does not validate that the distribution exists
lookDistroName :: (DistroName -> ServerPart a) -> ServerPart a
lookDistroName k
    = do
  mdistroName <- getDataFn $ do
                   distStr <- look "distroName"
                   case simpleParse distStr of
                     Nothing -> mzero
                     Just distroName -> return distroName
  case mdistroName of
   Nothing -> finishOk $ toResponse $
              hackageError "That wasn't a valid distribution name!"
   Just distroName -> k distroName


lookPackageName :: (PackageName -> ServerPart a) -> ServerPart a
lookPackageName k
    = do
  mName <- getDataFn $ do
             pNameStr <- look "packageName"

             case simpleParse pNameStr of
               Nothing -> mzero
               Just pName -> return pName
  case mName of
    Nothing -> finishOk $ toResponse $
               hackageError
                "Sorry, that doesn't seem to be a valid package name."
    Just pName -> k pName

lookPackageInfo :: (DistroPackageInfo -> ServerPart a)
                -> ServerPart a
lookPackageInfo k
    = do
  mInfo <- getDataFn $ do
             pVerStr <- look "version"
             pUriStr  <- look "uri"

             case simpleParse pVerStr of
               Nothing -> mzero
               Just pVer
                   -> return $ DistroPackageInfo pVer pUriStr

  case mInfo of
    Nothing -> finishOk $ toResponse $
               hackageError "Sorry, something went wrong there"
    Just pInfo -> k pInfo


-- | Form validation for entering a distribution. Peforms
-- a lookup from name to id, which also validates the existence
-- of the named distribution
lookDistroId :: (DistroId -> ServerPart a) -> ServerPart a
lookDistroId k
    = lookDistroName $ \distroName -> do
      mDistro <- query $ LookupDistroId distroName
      case mDistro of
        Nothing -> finishOk $ toResponse $
                   hackageError "Distribution does not exist"
        Just distro -> k distro


-- | Retrieves the form element "userName" and converts
-- it to a user id, which validates that the user exists
lookUserId :: (UserId -> ServerPart a) -> ServerPart a
lookUserId k
    = do
  mUserStr <- getDataFn $ look "userName"
  case mUserStr of
    Nothing -> finishOk $ toResponse $
               hackageError "User not specified"
    Just userStr
        -> case simpleParse userStr of
             Nothing -> finishOk $ toResponse
                        $ hackageError "Malformed user name"
             Just userName
                 -> do
                  mUser <- query $ LookupUserName userName
                  case mUser of
                    Nothing
                        -> finishOk $ toResponse $
                           hackageError "Unknown user!"
                    Just user -> k user

-- | Returns the referring URI or the passed in URI
bounceUri :: ToSURI uri => uri -> ServerPart SURI
bounceUri defaultUri
    = do
  mRef <- getHeaderM "Referer"
  return $ case mRef of
    Nothing  -> toSURI defaultUri
    Just ref -> toSURI $ BS.unpack ref


finishNotFound, finishOk :: (FilterMonad Response m, WebMonad a m) => a -> m b
finishNotFound r = notFound () >> finishWith r
finishOk       r = ok       () >> finishWith r
