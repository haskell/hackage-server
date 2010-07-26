module Distribution.Server.Features.DownloadCount where

import Distribution.Server.Feature
import Distribution.Server.Resource
import Distribution.Server.Features.Core
import Distribution.Server.Types
import Distribution.Server.Hook
import Distribution.Server.Packages.Downloads

import Distribution.Package
import Distribution.Text (display)

import Happstack.State hiding (Version)
import Happstack.Server
import Data.Time.Clock
import Control.Monad (liftM, forever)
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Data.Function (fix)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Control.Monad.Trans (MonadIO)

data DownloadFeature = DownloadFeature {
    downloadResource :: DownloadResource,
    downloadStream :: Chan PackageId
}

data DownloadResource = DownloadResource {
    topDownloads :: Resource
}

instance HackageFeature DownloadFeature where
    getFeature download = HackageModule
      { featureName = "download"
      , resources   = map (\x -> x $ downloadResource download) [topDownloads]
      , dumpBackup    = Nothing -- TODO
      , restoreBackup = Nothing
      }
    initHooks down = [forkIO transferDownloads >> return ()]
      where transferDownloads = forever $ do
                pkg <- readChan (downloadStream down)
                time <- getCurrentTime
                update $ RegisterDownload (utctDay time) pkg 1

initDownloadFeature :: Config -> CoreFeature -> IO DownloadFeature
initDownloadFeature _ core = do
    downChan <- newChan
    registerHook (tarballDownload core) $ writeChan downChan
    return DownloadFeature
      { downloadResource = fix $ \_ -> DownloadResource
          { topDownloads = (resourceAt "/packages/top.:format") { resourceGet = [("txt", \_ -> textTop)] }
          }
      , downloadStream = downChan
      }
  where
    -- FIXME: main issue here: these give the bottom 25 downloaded :)
    -- the IntMap is not sufficient for ordering (see Downloads.hs)
    -- because it goes up rather than down, and there's not too efficient
    -- of a way to query it in descending order. maybe a newtype is needed
    textTop = do
        count <- totalDownloadCount
        pkgs <- fmap (take 25) sortedPackages
        ok . toResponse . unlines $
            [show count ++ " total downloads", "The top downloaded packages are:"]
            ++ map ((" * "++) . display) pkgs


totalDownloadCount :: MonadIO m => m Int
totalDownloadCount = liftM totalDownloads $ query GetDownloadCounts

-- sortedPackages and sortByDownloads both order packages by total downloads without exposing download data

-- A lazy list of the top packages, which can be filtered, taken from, etc.
-- Does not include packages with no downloads.
sortedPackages :: MonadIO m => m [PackageName]
sortedPackages = liftM (concat . IntMap.elems . downloadHistogram) $ query GetDownloadCounts

-- Sorts a list of package-y items by their download count.
-- The best I can do with the current data model, it seems, is j*log(k)*log(n) with GHC's sortBy.
-- * j = # of elements of result list that are taken
-- * k = length of argument list
-- * n = total packages with download counts
sortByDownloads :: MonadIO m => (a -> PackageName) -> [a] -> m [a]
sortByDownloads nameFunc pkgs = query GetDownloadCounts >>= \counts -> do
    let -- note: map is lazily created, not necessarily O(n)
        modMap = Map.map (allDownloads . packageDownloads) (downloadMap counts)
        getCount pkg = Map.findWithDefault 0 (nameFunc pkg) modMap
    return $ sortBy (comparing getCount) pkgs

-- For at-a-glance download information.
perVersionDownloads :: (MonadIO m, Package pkg) => pkg -> m (Int, Int)
perVersionDownloads pkg = do
    info <- query $ GetDownloadInfo (packageName pkg)
    let (PackageDownloads total perVersion) = packageDownloads info
    return (total, Map.findWithDefault 0 (packageVersion pkg) perVersion)

