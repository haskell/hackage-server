{-# LANGUAGE ExistentialQuantification #-}
module Distribution.Client.Mirror.Repo.Types (
    SourceRepo(..)
  , TargetRepo(..)
  , targetCachedIndexPath
  ) where

-- stdlib
import Network.URI (URI)
import System.FilePath

-- hackage-security
import qualified Hackage.Security.Client                    as Sec
import qualified Hackage.Security.Client.Repository.Cache   as Sec

-- | Source repositories
data SourceRepo =
    -- | "New" style Hackage
    --
    -- (after the introduction of hackage-server, but before the introduction
    -- of security)
    SourceHackage2 {
        sourceRepoURI       :: URI
      , sourceRepoCachePath :: FilePath
      }

    -- | Secure repo
  | forall down. SourceSecure {
        sourceRepository    :: Sec.Repository down
      , sourceRepoCache     :: Sec.Cache
      , sourceRepoRootKeys  :: [Sec.KeyId]
      , sourceRepoThreshold :: Sec.KeyThreshold
      }

-- | Target repositories
data TargetRepo =
    -- | "New" style Hackage (hackage-server)
    TargetHackage2 {
        targetRepoURI       :: URI
      , targetRepoCachePath :: FilePath
      }

    -- | Local repository
  | TargetLocal {
        targetRepoPath      :: FilePath
      , targetRepoCachePath :: FilePath
      }

-- | File name of the cached index
--
-- NOTE: This stays the same whether it's actually 00-index or 01-index format
targetCachedIndexPath :: FilePath -> FilePath
targetCachedIndexPath targetCache = targetCache </> "cached-index.tar.gz"
