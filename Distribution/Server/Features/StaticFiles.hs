module Distribution.Server.Features.StaticFiles (
    staticFilesFeature
  ) where

import Distribution.Server.Framework

-- | The feature to serve the static html files.
--
-- Don't think this is going to be used that much, as it's not too modular, and
-- must be last in order. Top-level handling seems more appropriate.
staticFilesFeature :: HackageFeature
staticFilesFeature = HackageFeature {

    featureName = "static files",

    serverPart  = serveStaticFiles,

    -- There is no persistent state for this feature,
    -- so nothing needs to be backed up.
    dumpBackup    = Nothing,
    restoreBackup = Nothing
}

serveStaticFiles :: ServerEnv -> ServerPart Response
serveStaticFiles env =
  fileServe ["hackage.html"] (serverStaticDir env)
