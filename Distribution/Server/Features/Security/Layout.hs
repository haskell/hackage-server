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
onDiskRoot :: ServerEnv -> Sec.AbsolutePath
onDiskRoot = onDisk "root.json"

-- | The mirrors list
--
-- We just serve this file, we don't actually need this info.
onDiskMirrors :: ServerEnv -> Sec.AbsolutePath
onDiskMirrors = onDisk "mirrors.json"

-- | Timestamp private key
onDiskTimestampKey :: ServerEnv -> Sec.AbsolutePath
onDiskTimestampKey = onDisk "timestamp.private"

-- | Snapshot private key
onDiskSnapshotKey :: ServerEnv -> Sec.AbsolutePath
onDiskSnapshotKey = onDisk "snapshot.private"

-- | File containing pre-computed hashes
onDiskPrecomputedHashes :: ServerEnv -> FilePath
onDiskPrecomputedHashes = Sec.toFilePath . onDisk "md5-to-sha256.map"

{-------------------------------------------------------------------------------
  In-index layout
-------------------------------------------------------------------------------}

inIndexPkgMetadata :: PackageIdentifier -> FilePath
inIndexPkgMetadata = Sec.toUnrootedFilePath . Sec.unrootPath'
                   . Sec.indexLayoutPkgMetadata (Sec.repoIndexLayout layout)

{-------------------------------------------------------------------------------
  In-repo layout
-------------------------------------------------------------------------------}

inRepoPkgTarGz :: PackageIdentifier -> Sec.TargetPath
inRepoPkgTarGz = Sec.TargetPathRepo . Sec.repoLayoutPkgTarGz layout

secResourceAt :: (Sec.RepoLayout -> Sec.RepoPath) -> Resource
secResourceAt = resourceAt
              . addRoot
              . Sec.toUnrootedFilePath . Sec.unrootPath'
              . ($ layout)
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
layout :: Sec.RepoLayout
layout = Sec.hackageRepoLayout

onDisk :: String -> ServerEnv -> Sec.AbsolutePath
onDisk file env = serverTUFDir env Sec.</> Sec.fragment' file
