{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.Vouch (VouchFeature(..), initVouchFeature, judgeVouch) where

import Distribution.Server.Features.Vouch.Acid (acidStore)
import qualified Distribution.Server.Features.Vouch.Store as Store
import Distribution.Server.Features.Vouch.Types
import Control.Monad (when, join)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Time (UTCTime(..), addUTCTime, getCurrentTime, nominalDay, secondsToDiffTime)
import Data.Time.Format.ISO8601 (formatShow, iso8601Format)
import Text.XHtml.Strict (prettyHtmlFragment, stringToHtml, li)

import Distribution.Server.Framework ((</>), DynamicPath, HackageFeature, IsHackageFeature, IsHackageFeature(..))
import Distribution.Server.Framework (MessageSpan(MText), Method(..), Response, ServerEnv(..), ServerPartE)
import Distribution.Server.Framework (emptyHackageFeature, errBadRequest)
import Distribution.Server.Framework (featureDesc, featureReloadFiles, featureResources, featureState)
import Distribution.Server.Framework (liftIO, resourceAt, resourceDesc, resourceGet)
import Distribution.Server.Framework (resourcePost, toResponse)
import Distribution.Server.Framework.Templating (($=), TemplateAttr, getTemplate, loadTemplates, reloadTemplates, templateUnescaped)
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Types (UserId(..), UserInfo, UserName(..), userName)
import Distribution.Server.Features.Upload(UploadFeature(..))
import Distribution.Server.Features.Users (UserFeature(..))
import Distribution.Simple.Utils (toUTF8LBS)

data VouchFeature =
  VouchFeature
    { vouchFeatureInterface :: HackageFeature
    , drainQueuedNotifications :: forall m. MonadIO m => m [UserId]
    }

instance IsHackageFeature VouchFeature where
  getFeatureInterface = vouchFeatureInterface

requiredCountOfVouches :: Int
requiredCountOfVouches = 2

isWithinLastMonth :: UTCTime -> (UserId, UTCTime) -> Bool
isWithinLastMonth now (_, vouchTime) =
  addUTCTime (30 * nominalDay) vouchTime >= now

judgeVouch
  :: Group.UserIdSet
  -> UTCTime
  -> UserId
  -> [(UserId, UTCTime)]
  -> [(UserId, UTCTime)]
  -> UserId
  -> Either VouchError VouchSuccess
judgeVouch ugroup now vouchee vouchersForVoucher existingVouchers voucher = join . runExceptT $ do
  when (not (voucher `Group.member` ugroup)) $
    throwError NotAnUploader
  -- You can only vouch for non-uploaders, so if this list has items, the user is uploader because of these vouches.
  -- Make sure none of them are too recent.
  when (length vouchersForVoucher >= requiredCountOfVouches && any (isWithinLastMonth now) vouchersForVoucher) $
    throwError You'reTooNew
  when (vouchee `Group.member` ugroup) $
    throwError VoucheeAlreadyUploader
  when (length existingVouchers >= requiredCountOfVouches) $
    throwError AlreadySufficientlyVouched
  when (voucher `elem` map fst existingVouchers) $
    throwError YouAlreadyVouched
  pure $
    if length existingVouchers == requiredCountOfVouches - 1
       then AddVouchComplete
       else
         let stillRequired = requiredCountOfVouches - length existingVouchers - 1
          in AddVouchIncomplete stillRequired

renderToLBS :: (UserId -> ServerPartE UserInfo) -> [(UserId, UTCTime)] -> ServerPartE TemplateAttr
renderToLBS lookupUserInfo vouches = do
  rendered <- traverse (renderVouchers lookupUserInfo) vouches
  pure $
    templateUnescaped "vouches" $
      if null rendered
         then LBS.pack "Nobody has endorsed yet."
         else LBS.intercalate mempty rendered

renderVouchers :: (UserId -> ServerPartE UserInfo) -> (UserId, UTCTime) -> ServerPartE LBS.ByteString
renderVouchers lookupUserInfo (uid, timestamp) = do
  info <- lookupUserInfo uid
  let UserName name = userName info
      -- We don't need to show millisecond precision
      -- So we truncate it off here
      truncated = truncate $ utctDayTime timestamp
      newUTCTime = timestamp {utctDayTime = secondsToDiffTime truncated}
  pure . toUTF8LBS . prettyHtmlFragment . li . stringToHtml $ name <> " vouched on " <> formatShow iso8601Format newUTCTime

initVouchFeature :: ServerEnv -> IO (UserFeature -> UploadFeature -> IO VouchFeature)
initVouchFeature ServerEnv{serverStateDir, serverTemplatesDir, serverTemplatesMode} = do
  vouchBackend <- acidStore serverStateDir
  templates <- loadTemplates serverTemplatesMode [ serverTemplatesDir, serverTemplatesDir </> "Html"]
                                                 ["vouch.html"]
  vouchTemplate <- getTemplate templates "vouch.html"
  return $ \UserFeature{userNameInPath, lookupUserName, lookupUserInfo, guardAuthenticated}
            UploadFeature{uploadersGroup} -> do
    let
      vouchStore = Store.backendStore vouchBackend
      handleGetVouches :: DynamicPath -> ServerPartE Response
      handleGetVouches dpath = do
        uid <- lookupUserName =<< userNameInPath dpath
        vouches <- Store.getVouchesFor vouchStore uid
        param <- renderToLBS lookupUserInfo vouches
        pure . toResponse $ vouchTemplate
          [ "msg" $= ""
          , "requiredNumber" $= show requiredCountOfVouches
          , param
          ]
      handlePostVouch :: DynamicPath -> ServerPartE Response
      handlePostVouch dpath = do
        voucher <- guardAuthenticated
        ugroup <- liftIO $ Group.queryUserGroup uploadersGroup
        now <- liftIO getCurrentTime
        vouchee <- lookupUserName =<< userNameInPath dpath
        vouchersForVoucher <- Store.getVouchesFor vouchStore voucher
        existingVouchers <- Store.getVouchesFor vouchStore vouchee
        case judgeVouch ugroup now vouchee vouchersForVoucher existingVouchers voucher of
          Left NotAnUploader ->
            errBadRequest "Not an uploader" [MText "You must be an uploader yourself to endorse other users."]
          Left You'reTooNew ->
            errBadRequest "You're too new" [MText "The latest of the endorsements for your user must be at least 30 days old."]
          Left VoucheeAlreadyUploader ->
            errBadRequest "Endorsee already uploader" [MText "You can't endorse this user, since they are already an uploader."]
          Left AlreadySufficientlyVouched ->
            errBadRequest "Already sufficiently endorsed" [MText "There are already a sufficient number of endorsements for this user."]
          Left YouAlreadyVouched ->
            errBadRequest "Already endorsed" [MText "You have already endorsed this user."]
          Right result -> do
            Store.addVouch vouchStore vouchee (voucher, now)
            param <- renderToLBS lookupUserInfo $ existingVouchers ++ [(voucher, now)]
            case result of
              AddVouchComplete -> do
                -- enqueue vouching completed notification
                -- which will be read using drainQueuedNotifications
                Store.queueVouchCompleteNotification vouchStore vouchee

                liftIO $ Group.addUserToGroup uploadersGroup vouchee
                pure . toResponse $ vouchTemplate
                  [ "msg" $= "Added endorsement. User is now an uploader!"
                  , "requiredNumber" $= show requiredCountOfVouches
                  , param
                  ]
              AddVouchIncomplete stillRequired ->
                pure . toResponse $ vouchTemplate
                  [ "msg" $=
                         "Added endorsement. User still needs "
                      <> show stillRequired
                      <> if stillRequired == 1 then " endorsement" else " endorsements"
                      <> " to become uploader."
                  , param
                  ]
    return $ VouchFeature {
      vouchFeatureInterface =
        (emptyHackageFeature "endorse")
          { featureDesc = "Endorsing users such that they get upload permission."
          , featureResources =
            [(resourceAt "/user/:username/endorse")
              { resourceDesc = [(GET, "list people endorsing")
                               ,(POST, "endorse for user")
                               ]
              , resourceGet = [("html", handleGetVouches)]
              , resourcePost = [("html", handlePostVouch)]
              }
            ]
          , featureState = Store.backendState vouchBackend
          , featureReloadFiles = reloadTemplates templates
          },
      drainQueuedNotifications = Store.drainQueuedNotifications vouchStore
    }
