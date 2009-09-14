
module Distribution.Server.Export.ServerParts
    ( export
    ) where

import qualified Distribution.Server.Export as Export

import Happstack.Server
import Happstack.State

import Distribution.Server.Users.State
import Distribution.Server.Packages.State
import qualified Distribution.Server.ResourceTypes as Resources
import Distribution.Server.Util.BlobStorage (BlobStorage)

import Control.Monad.Trans (liftIO)

export :: BlobStorage -> ServerPart Response
export storage
    = methodSP GET $ do
        state <- query GetPackagesState
        perms <- query GetPermissions
        docs  <- query GetDocumentation

        let pkgs = packageList state
            rpts = buildReports state
            users = userDb state

        tarball <- liftIO $ Export.export users perms pkgs docs rpts storage

        return $ toResponse . Resources.ExportTarball $ tarball

        
