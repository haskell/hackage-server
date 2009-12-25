
module Distribution.Server.Export.ServerParts
    ( export
    ) where

import qualified Distribution.Server.Export as Export

import Happstack.Server
import Happstack.State

import Distribution.Server.Distributions.State
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
        dist  <- query GetDistributions

        let pkgs = packageList state
            rpts = buildReports state
            users = userDb state
            dists = dist_distros dist
            distInfo = dist_versions dist

        tarball <- liftIO $
           Export.export users perms pkgs docs rpts storage dists distInfo

        return $ toResponse . Resources.ExportTarball $ tarball

        
