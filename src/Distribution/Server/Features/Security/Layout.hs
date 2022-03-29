-- | Layout
module Distribution.Server.Features.Security.Layout (
    -- * On-disk layout
    onDiskRoot
  , onDiskMirrors
  , onDiskTimestampKey
  , onDiskSnapshotKey
  , onDiskPrecomputedHashes
    -- * In-index layout
  , inIndexPkgMetadata
    -- * In-repo layout
  , inRepoPkgTarGz
  , secResourceAt
  ) where

-- hackage
import Distribution.Server.Framework.ServerEnv
import Distribution.Server.Framework.Resource

-- Cabal
import Distribution.Package (PackageIdentifier)

-- hackage-security
import qualified Hackage.Security.Server    as Sec
import qualified Hackage.Security.Util.Path as Sec

{-------------------------------------------------------------------------------
  On-disk layout
-------------------------------------------------------------------------------}

-- | The root info
--
-- We just serve this file, we don't actually need the info.
onDiskRoot :: ServerEnv -> Sec.Path Sec.Absolute
onDiskRoot = onDisk "root.json"

-- | The mirrors list
--
-- We just serve this file, we don't actually need this info.
onDiskMirrors :: ServerEnv -> Sec.Path Sec.Absolute
onDiskMirrors = onDisk "mirrors.json"

-- | Timestamp private key
onDiskTimestampKey :: ServerEnv -> Sec.Path Sec.Absolute
onDiskTimestampKey = onDisk "timestamp.private"

-- | Snapshot private key
onDiskSnapshotKey :: ServerEnv -> Sec.Path Sec.Absolute
onDiskSnapshotKey = onDisk "snapshot.private"

-- | File containing pre-computed hashes
onDiskPrecomputedHashes :: ServerEnv -> FilePath
onDiskPrecomputedHashes = Sec.toFilePath . onDisk "md5-to-sha256.map"

{-------------------------------------------------------------------------------
  In-index layout
-------------------------------------------------------------------------------}

inIndexPkgMetadata :: PackageIdentifier -> FilePath
inIndexPkgMetadata = Sec.toUnrootedFilePath . Sec.unrootPath
                   . Sec.indexLayoutPkgMetadata indexLayout

{-------------------------------------------------------------------------------
  In-repo layout
-------------------------------------------------------------------------------}

inRepoPkgTarGz :: PackageIdentifier -> Sec.TargetPath
inRepoPkgTarGz = Sec.TargetPathRepo . Sec.repoLayoutPkgTarGz repoLayout

secResourceAt :: (Sec.RepoLayout -> Sec.RepoPath) -> Resource
secResourceAt = resourceAt
              . addRoot
              . Sec.toUnrootedFilePath . Sec.unrootPath
              . ($ repoLayout)
  where
    -- The path we get from the security library is unrooted (it just says
    -- @root.json@, say, rather than @/root.json@).
    addRoot :: String -> String
    addRoot = ("/" ++)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Layout specification from hackage-security
--
-- The in-index layout and in-repo layout are dictated by the hackage-security
-- library (used both by the Hackage server and clients such as cabal-install)
repoLayout :: Sec.RepoLayout
repoLayout = Sec.hackageRepoLayout

indexLayout :: Sec.IndexLayout
indexLayout = Sec.hackageIndexLayout

onDisk :: String -> ServerEnv -> Sec.Path Sec.Absolute
onDisk file env = serverTUFDir env Sec.</> Sec.fragment file
