{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Server.Features.AdminLog where

import Distribution.Server.Features.AdminLog.Acid (acidStore)
import qualified Distribution.Server.Features.AdminLog.Store as Store
import Distribution.Server.Features.AdminLog.Types
import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Users.Group
import Distribution.Server.Framework

import Distribution.Server.Pages.AdminLog
import Distribution.Server.Features.Users

import Data.Time.Clock (getCurrentTime, UTCTime)
import qualified Data.ByteString.Lazy.Char8 as BS
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
    , queryGetAdminLog :: forall m. MonadIO m => m [(UTCTime,UserId,AdminAction,BS.ByteString)]
}

instance IsHackageFeature AdminLogFeature where
    getFeatureInterface = adminLogFeatureInterface

initAdminLogFeature :: ServerEnv -> IO (UserFeature -> IO AdminLogFeature)
initAdminLogFeature ServerEnv{serverStateDir} = do
  adminLogBackend <- acidStore serverStateDir
  return $ \users@UserFeature{groupChangedHook} -> do

    let feature = adminLogFeature users adminLogBackend

    registerHook groupChangedHook $ \(gd,addOrDel,actorUid,targetUid,reason) -> do
        now <- getCurrentTime
        Store.addAdminLog (Store.backendStore adminLogBackend)
            (now, actorUid, mkAdminAction gd addOrDel targetUid, packUTF8 reason)

    return feature

adminLogFeature :: UserFeature
                -> Store.Backend
                -> AdminLogFeature
adminLogFeature UserFeature{..} Store.Backend{backendStore = adminLogStore, backendState}
  = AdminLogFeature {..}

  where
    adminLogFeatureInterface =
      (emptyHackageFeature "admin-actions-log") {
        featureDesc      = "Log of additions and removals of users from groups.",
        featureResources = [adminLogResource],
        featureState     = backendState
      }

    adminLogResource :: Resource
    adminLogResource =
      (resourceAt "/admin/log.:format") {
        resourceDesc = [(GET, "Full list of group additions and removals")],
        resourceGet  = [("html", serveAdminLogGet)]
      }

    queryGetAdminLog :: MonadIO m => m [(UTCTime,UserId,AdminAction,BS.ByteString)]
    queryGetAdminLog = Store.getAdminLog adminLogStore

    serveAdminLogGet _ = do
      aLog  <- queryGetAdminLog
      users <- queryGetUserDb
      return . toResponse . adminLogPage users . map mkRow $ aLog

    mkRow (time, actorId, Admin_GroupDelUser targetId group, reason) =
          (time, actorId, "Acid.Delete", targetId, nameIt group, unpackUTF8 reason)
    mkRow (time, actorId, Admin_GroupAddUser targetId group, reason) =
          (time, actorId, "Acid.Add", targetId, nameIt group, unpackUTF8 reason)

    nameIt (MaintainerGroup pn) = "Maintainers for " ++ unpackUTF8 pn
    nameIt AdminGroup           = "Administrators"
    nameIt TrusteeGroup         = "Trustees"
    nameIt (OtherGroup s)       = unpackUTF8 s
