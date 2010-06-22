module Distribution.Server.Features.StaticFiles (
    staticFilesFeature
  ) where

import Distribution.Server.Feature

import qualified Happstack.Server as Happs

-- | The feature to serve the static html files.
--
-- Don't think this is going to be used that much.
staticFilesFeature :: HackageFeature
staticFilesFeature = HackageFeature {

    featureName = "static files",

    serverPart  = serveStaticFiles,

    -- There is no persistent state for this feature,
    -- so nothing needs to be backed up.
    dumpBackup    = Nothing,
    restoreBackup = Nothing
}

serveStaticFiles :: Config -> Happs.ServerPart Happs.Response
serveStaticFiles conf =
  Happs.fileServe ["hackage.html"] (serverStaticDir conf)
