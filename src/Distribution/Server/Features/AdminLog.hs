{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Server.Features.AdminLog where

import qualified Distribution.Server.Features.AdminLog.Acid as Acid
import Distribution.Server.Features.AdminLog.Backup
import Distribution.Server.Features.AdminLog.Types
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Users.Group
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

import Distribution.Server.Pages.AdminLog
import Distribution.Server.Features.Users

import Data.Time.Clock (getCurrentTime)
import Distribution.Server.Util.Parse

--TODO Maybe Reason

mkAdminAction :: GroupDescription -> Bool -> UserId -> AdminAction
mkAdminAction gd isAdd uid = (if isAdd then Admin_GroupAddUser else Admin_GroupDelUser) uid groupdesc
    where groupdesc | groupTitle gd == "Hackage admins" = AdminGroup
                    | groupTitle gd == "Package trustees" = TrusteeGroup
                    | Just (pn,_) <- groupEntity gd, groupTitle gd == "Maintainers" = MaintainerGroup (packUTF8 pn)
                    | otherwise = OtherGroup $ packUTF8 (groupTitle gd ++ maybe "" ((' ':) . fst) (groupEntity gd))

data AdminLogFeature = AdminLogFeature {
      adminLogFeatureInterface :: HackageFeature
    , queryGetAdminLog :: forall m. MonadIO m => m Acid.AdminLog
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
        updateState adminLogState $ Acid.AddAdminLog
            (now, actorUid, mkAdminAction gd addOrDel targetUid, packUTF8 reason)

    return feature

adminLogFeature :: UserFeature
                -> StateComponent AcidState Acid.AdminLog
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

    queryGetAdminLog :: MonadIO m => m Acid.AdminLog
    queryGetAdminLog = queryState adminLogState Acid.GetAdminLog

    serveAdminLogGet _ = do
      aLog  <- queryState adminLogState Acid.GetAdminLog
      users <- queryGetUserDb
      return . toResponse . adminLogPage users . map mkRow . Acid.adminLog $ aLog

    mkRow (time, actorId, Admin_GroupDelUser targetId group, reason) =
          (time, actorId, "Acid.Delete", targetId, nameIt group, unpackUTF8 reason)
    mkRow (time, actorId, Admin_GroupAddUser targetId group, reason) =
          (time, actorId, "Acid.Add", targetId, nameIt group, unpackUTF8 reason)

    nameIt (MaintainerGroup pn) = "Maintainers for " ++ unpackUTF8 pn
    nameIt AdminGroup           = "Administrators"
    nameIt TrusteeGroup         = "Trustees"
    nameIt (OtherGroup s)       = unpackUTF8 s

adminLogStateComponent :: FilePath -> IO (StateComponent AcidState Acid.AdminLog)
adminLogStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "AdminLog") Acid.initialAdminLog
  return StateComponent {
      stateDesc    = "AdminLog"
    , stateHandle  = st
    , getState     = query st Acid.GetAdminLog
    , putState     = update st . Acid.ReplaceAdminLog
    , backupState  = \_ (Acid.AdminLog xs) ->
                      [BackupByteString ["adminLog.txt"] . backupLogEntries $ xs]
    , restoreState = restoreAdminLogBackup
    , resetState   = adminLogStateComponent
    }

