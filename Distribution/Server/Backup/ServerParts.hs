module Distribution.Server.Backup.ServerParts
    ( export
    ) where

import qualified Distribution.Server.Backup.Export as Export

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
        docs  <- query GetDocumentation
        dist  <- query GetDistributions
        users <- query GetUserDb
        rpts  <- query GetBuildReports

        let pkgs = packageList state
            dists = dist_distros dist
            distInfo = dist_versions dist

        -- lazy tarball
        tarball <- liftIO $
           Export.export users pkgs docs rpts storage dists distInfo

        return $ toResponse . Resources.ExportTarball $ tarball

