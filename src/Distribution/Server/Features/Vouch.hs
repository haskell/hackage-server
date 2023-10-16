{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
module Distribution.Server.Features.Vouch (VouchError(..), VouchSuccess(..), initVouchFeature, judgeVouch) where

import Control.Monad (when, join)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime(..), addUTCTime, getCurrentTime, nominalDay, secondsToDiffTime)
import Data.Time.Format.ISO8601 (formatShow, iso8601Format)
import Text.XHtml.Strict (prettyHtmlFragment, stringToHtml, li)

import Data.SafeCopy (base, deriveSafeCopy)
import Distribution.Server.Framework ((</>), AcidState, DynamicPath, HackageFeature, IsHackageFeature, IsHackageFeature(..), MemSize)
import Distribution.Server.Framework (MessageSpan(MText), Method(..), Query, Response, ServerEnv(..), ServerPartE, StateComponent(..), Update)
import Distribution.Server.Framework (abstractAcidStateComponent, emptyHackageFeature, errBadRequest)
import Distribution.Server.Framework (featureDesc, featureReloadFiles, featureResources, featureState)
import Distribution.Server.Framework (liftIO, makeAcidic, openLocalStateFrom, query, queryState, resourceAt, resourceDesc, resourceGet)
import Distribution.Server.Framework (resourcePost, toResponse, update, updateState)
import Distribution.Server.Framework.BackupRestore (RestoreBackup(..))
import Distribution.Server.Framework.Templating (($=), TemplateAttr, getTemplate, loadTemplates, reloadTemplates, templateUnescaped)
import qualified Distribution.Server.Users.Group as Group
import Distribution.Server.Users.Types (UserId(..), UserInfo, UserName(..), userName)
import Distribution.Server.Features.Upload(UploadFeature(..))
import Distribution.Server.Features.Users (UserFeature(..))
import Distribution.Simple.Utils (toUTF8LBS)

newtype VouchData = VouchData (Map.Map UserId [(UserId, UTCTime)])
  deriving (Show, Eq)
  deriving newtype MemSize

putVouch :: UserId -> (UserId, UTCTime) -> Update VouchData ()
putVouch vouchee (voucher, now) = do
  VouchData tbl <- get
  let oldMap = fromMaybe [] (Map.lookup vouchee tbl)
      newMap = (voucher, now) : oldMap
  put $ VouchData (Map.insert vouchee newMap tbl)

getVouchesFor :: UserId -> Query VouchData [(UserId, UTCTime)]
getVouchesFor needle = do
  VouchData tbl <- ask
  pure . fromMaybe [] $ Map.lookup needle tbl

getVouchesData :: Query VouchData VouchData
getVouchesData = ask

replaceVouchesData :: VouchData -> Update VouchData ()
replaceVouchesData = put

$(deriveSafeCopy 0 'base ''VouchData)

makeAcidic ''VouchData
  [ 'putVouch
  , 'getVouchesFor
  -- Stock
  , 'getVouchesData
  , 'replaceVouchesData
  ]

vouchStateComponent :: FilePath -> IO (StateComponent AcidState VouchData)
vouchStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Vouch") (VouchData mempty)
  let initialVouchData = VouchData mempty
      restore =
        RestoreBackup
          { restoreEntry = error "Unexpected backup entry"
          , restoreFinalize = return initialVouchData
          }
  pure StateComponent
    { stateDesc = "Keeps track of vouches"
    , stateHandle = st
    , getState = query st GetVouchesData
    , putState = update st . ReplaceVouchesData
    , backupState = \_ _ -> []
    , restoreState = restore
    , resetState = vouchStateComponent
    }

data VouchFeature =
  VouchFeature
    { vouchFeatureInterface :: HackageFeature
    }

instance IsHackageFeature VouchFeature where
  getFeatureInterface = vouchFeatureInterface

requiredCountOfVouches :: Int
requiredCountOfVouches = 2

isWithinLastMonth :: UTCTime -> (UserId, UTCTime) -> Bool
isWithinLastMonth now (_, vouchTime) =
  addUTCTime (30 * nominalDay) vouchTime >= now

data VouchError
  = NotAnUploader
  | You'reTooNew
  | VoucheeAlreadyUploader
  | AlreadySufficientlyVouched
  | YouAlreadyVouched
  deriving stock (Show, Eq)

data VouchSuccess = AddVouchComplete | AddVouchIncomplete Int
  deriving stock (Show, Eq)

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
         then LBS.pack "Nobody has vouched yet."
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
  vouchState <- vouchStateComponent serverStateDir
  templates <- loadTemplates serverTemplatesMode [ serverTemplatesDir, serverTemplatesDir </> "Html"]
                                                 ["vouch.html"]
  vouchTemplate <- getTemplate templates "vouch.html"
  return $ \UserFeature{userNameInPath, lookupUserName, lookupUserInfo, guardAuthenticated}
            UploadFeature{uploadersGroup} -> do
    let
      handleGetVouches :: DynamicPath -> ServerPartE Response
      handleGetVouches dpath = do
        uid <- lookupUserName =<< userNameInPath dpath
        userIds <- queryState vouchState $ GetVouchesFor uid
        param <- renderToLBS lookupUserInfo userIds
        pure . toResponse $ vouchTemplate
          [ "msg" $= ""
          , param
          ]
      handlePostVouch :: DynamicPath -> ServerPartE Response
      handlePostVouch dpath = do
        voucher <- guardAuthenticated
        ugroup <- liftIO $ Group.queryUserGroup uploadersGroup
        now <- liftIO getCurrentTime
        vouchee <- lookupUserName =<< userNameInPath dpath
        vouchersForVoucher <- queryState vouchState $ GetVouchesFor voucher
        existingVouchers <- queryState vouchState $ GetVouchesFor vouchee
        case judgeVouch ugroup now vouchee vouchersForVoucher existingVouchers voucher of
          Left NotAnUploader ->
            errBadRequest "Not an uploader" [MText "You must be an uploader yourself to vouch for other users."]
          Left You'reTooNew ->
            errBadRequest "You're too new" [MText "The latest of the vouches for your user must be at least 30 days old."]
          Left VoucheeAlreadyUploader ->
            errBadRequest "Vouchee already uploader" [MText "You can't vouch for this user, since they are already an uploader."]
          Left AlreadySufficientlyVouched ->
            errBadRequest "Already sufficiently vouched" [MText "There are already a sufficient number of vouches for this user."]
          Left YouAlreadyVouched ->
            errBadRequest "Already vouched" [MText "You have already vouched for this user."]
          Right result -> do
            updateState vouchState $ PutVouch vouchee (voucher, now)
            param <- renderToLBS lookupUserInfo $ existingVouchers ++ [(voucher, now)]
            case result of
              AddVouchComplete -> do
                liftIO $ Group.addUserToGroup uploadersGroup vouchee
                pure . toResponse $ vouchTemplate
                  [ "msg" $= "Added vouch. User is now an uploader!"
                  , param
                  ]
              AddVouchIncomplete stillRequired ->
                pure . toResponse $ vouchTemplate
                  [ "msg" $=
                         "Added vouch. User still needs "
                      <> show stillRequired
                      <> if stillRequired == 1 then " vouch" else " vouches"
                      <> " to become uploader."
                  , param
                  ]
    return $ VouchFeature $
      (emptyHackageFeature "vouch")
        { featureDesc = "Vouching for users getting upload permission."
        , featureResources =
          [(resourceAt "/user/:username/vouch")
            { resourceDesc = [(GET, "list people vouching")
                             ,(POST, "vouch for user")
                             ]
            , resourceGet = [("html", handleGetVouches)]
            , resourcePost = [("html", handlePostVouch)]
            }
          ]
        , featureState = [ abstractAcidStateComponent vouchState ]
        , featureReloadFiles = reloadTemplates templates
        }
