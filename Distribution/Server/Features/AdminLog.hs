{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, BangPatterns,
             GeneralizedNewtypeDeriving, NamedFieldPuns, RecordWildCards,
             PatternGuards, RankNTypes #-}

module Distribution.Server.Features.AdminLog where

import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Users.Group
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

import Distribution.Server.Pages.AdminLog
import Distribution.Server.Features.Users

import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable
import Data.Maybe(catMaybes)
import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.Read (readMaybe)
import Distribution.Server.Util.Parse

data GroupDesc = MaintainerGroup BS.ByteString | AdminGroup | TrusteeGroup | OtherGroup BS.ByteString deriving (Eq, Ord, Read, Show)

deriveSafeCopy 0 'base ''GroupDesc

instance MemSize GroupDesc where
    memSize (MaintainerGroup x) = memSize x
    memSize _ = 0

data AdminAction = Admin_GroupAddUser UserId GroupDesc | Admin_GroupDelUser UserId GroupDesc deriving (Eq, Ord, Read, Show)

instance MemSize AdminAction where
    memSize (Admin_GroupAddUser x y) = memSize2 x y
    memSize (Admin_GroupDelUser x y) = memSize2 x y

deriveSafeCopy 0 'base ''AdminAction

--TODO Maybe Reason

mkAdminAction :: GroupDescription -> Bool -> UserId -> AdminAction
mkAdminAction gd isAdd uid = (if isAdd then Admin_GroupAddUser else Admin_GroupDelUser) uid groupdesc
    where groupdesc | groupTitle gd == "Hackage admins" = AdminGroup
                    | groupTitle gd == "Package trustees" = TrusteeGroup
                    | Just (pn,_) <- groupEntity gd, groupTitle gd == "Maintainers" = MaintainerGroup (packUTF8 pn)
                    | otherwise = OtherGroup $ packUTF8 (groupTitle gd ++ maybe "" ((' ':) . fst) (groupEntity gd))

newtype AdminLog = AdminLog {
      adminLog :: [(UTCTime,UserId,AdminAction,BS.ByteString)]
} deriving (Typeable, Show, MemSize)

deriveSafeCopy 0 'base ''AdminLog

initialAdminLog :: AdminLog
initialAdminLog = AdminLog []

getAdminLog :: Query AdminLog AdminLog
getAdminLog = ask

addAdminLog :: (UTCTime, UserId, AdminAction, BS.ByteString) -> Update AdminLog ()
addAdminLog x = State.modify (\(AdminLog xs) -> AdminLog (x : xs))

instance Eq AdminLog where
    (AdminLog (x:_)) == (AdminLog (y:_)) = x == y
    (AdminLog []) == (AdminLog []) = True
    _ == _ = False

replaceAdminLog :: AdminLog -> Update AdminLog ()
replaceAdminLog = State.put

makeAcidic ''AdminLog ['getAdminLog
                      ,'replaceAdminLog
                      ,'addAdminLog]

data AdminLogFeature = AdminLogFeature {
      adminLogFeatureInterface :: HackageFeature
    , queryGetAdminLog :: forall m. MonadIO m => m AdminLog
}

instance IsHackageFeature AdminLogFeature where
    getFeatureInterface = adminLogFeatureInterface

initAdminLogFeature :: ServerEnv -> IO (UserFeature -> IO AdminLogFeature)
initAdminLogFeature ServerEnv{serverStateDir} = do
  adminLogState <- adminLogStateComponent serverStateDir
  return $ \users@UserFeature{groupChangedHook} -> do

    let feature = adminLogFeature users adminLogState

    registerHook groupChangedHook $ \(gd,addOrDel,actorUid,targetUid,reason) -> do
        now <- getCurrentTime
        updateState adminLogState $ AddAdminLog
            (now, actorUid, mkAdminAction gd addOrDel targetUid, packUTF8 reason)

    return feature

adminLogFeature :: UserFeature
                -> StateComponent AcidState AdminLog
                -> AdminLogFeature
adminLogFeature UserFeature{..} adminLogState
  = AdminLogFeature {..}

  where
    adminLogFeatureInterface =
      (emptyHackageFeature "admin-actions-log") {
        featureDesc      = "Log of additions and removals of users from groups.",
        featureResources = [adminLogResource],
        featureState     = [abstractAcidStateComponent adminLogState]
      }

    adminLogResource :: Resource
    adminLogResource =
      (resourceAt "/admin/log.:format") {
        resourceDesc = [(GET, "Full list of group additions and removals")],
        resourceGet  = [("html", serveAdminLogGet)]
      }

    queryGetAdminLog :: MonadIO m => m AdminLog
    queryGetAdminLog = queryState adminLogState GetAdminLog

    serveAdminLogGet _ = do
      aLog  <- queryState adminLogState GetAdminLog
      users <- queryGetUserDb
      return . toResponse . adminLogPage users . map mkRow . adminLog $ aLog

    mkRow (time, actorId, Admin_GroupDelUser targetId group, reason) =
          (time, actorId, "Delete", targetId, nameIt group, unpackUTF8 reason)
    mkRow (time, actorId, Admin_GroupAddUser targetId group, reason) =
          (time, actorId, "Add", targetId, nameIt group, unpackUTF8 reason)

    nameIt (MaintainerGroup pn) = "Maintainers for " ++ unpackUTF8 pn
    nameIt AdminGroup           = "Administrators"
    nameIt TrusteeGroup         = "Trustees"
    nameIt (OtherGroup s)       = unpackUTF8 s

adminLogStateComponent :: FilePath -> IO (StateComponent AcidState AdminLog)
adminLogStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "AdminLog") initialAdminLog
  return StateComponent {
      stateDesc    = "AdminLog"
    , stateHandle  = st
    , getState     = query st GetAdminLog
    , putState     = update st . ReplaceAdminLog
    , backupState  = \_ (AdminLog xs) ->
                      [BackupByteString ["adminLog.txt"] . backupLogEntries $ xs]
    , restoreState = restoreAdminLogBackup
    , resetState   = adminLogStateComponent
    }

restoreAdminLogBackup :: RestoreBackup AdminLog
restoreAdminLogBackup =
    go (AdminLog [])
  where
    go logs =
      RestoreBackup {
        restoreEntry = \entry -> case entry of
                        BackupByteString ["adminLog.txt"] bs
                          -> return . go $ importLogs logs bs
                        _ -> return (go logs)
      , restoreFinalize = return logs
      }

importLogs :: AdminLog -> BS.ByteString -> AdminLog
importLogs (AdminLog ls) =
    AdminLog . (++ls) . catMaybes . map fromRecord . lines . unpackUTF8
  where
    fromRecord :: String -> Maybe (UTCTime,UserId,AdminAction,BS.ByteString)
    fromRecord = readMaybe

backupLogEntries :: [(UTCTime,UserId,AdminAction,BS.ByteString)] -> BS.ByteString
backupLogEntries = packUTF8 . unlines . map show
