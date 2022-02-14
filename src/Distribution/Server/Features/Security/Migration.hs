{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
-- | Security specific migration
module Distribution.Server.Features.Security.Migration (
    migratePkgTarball_v1_to_v2
  , migrateCandidatePkgTarball_v1_to_v2
  ) where

import Distribution.Server.Prelude

-- stdlib
import Control.DeepSeq
import Control.Exception
import Data.Map (Map)
import System.IO
import System.IO.Error
import qualified Data.Map    as Map
import qualified Data.Vector as Vec

-- Cabal
import Distribution.Package (PackageId)

-- hackage
import Distribution.Server.Features.Core.State
import Distribution.Server.Features.PackageCandidates.State
import Distribution.Server.Features.PackageCandidates.Types
import Distribution.Server.Features.Security.Layout
import Distribution.Server.Framework hiding (Length)
import Distribution.Server.Framework.BlobStorage
import Distribution.Server.Packages.Types
import Distribution.Server.Util.ReadDigest
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

{-------------------------------------------------------------------------------
  Migration of core data structures
-------------------------------------------------------------------------------}

-- | Migrate from BlobId to BlobInfo in PkgTarball
--
-- This is part of the security feature because this computes the additional
-- information (SHA hashes) that we need for the TUF target files.
migratePkgTarball_v1_to_v2 :: ServerEnv
                           -> StateComponent AcidState PackagesState
                           -> IO ()
migratePkgTarball_v1_to_v2 env@ServerEnv{ serverVerbosity = verbosity }
                           packagesState
                           = do
    precomputedHashes <- readPrecomputedHashes env
    PackagesState{packageIndex} <- queryState packagesState GetPackagesState
    let allPackages = PackageIndex.allPackages packageIndex
        partitionSz = PackageIndex.numPackageVersions packageIndex `div` 10
        partitioned = partition partitionSz allPackages
    stats <- forM (zip [1..] partitioned) $ \(i, pkgs) ->
      logTiming verbosity (partitionLogMsg i (length partitioned)) $
        migratePkgs env updatePackage precomputedHashes pkgs
    loginfo verbosity $ prettyMigrationStats (mconcat stats)
  where
    updatePackage :: PackageId -> PkgInfo -> IO ()
    updatePackage pkgId pkgInfo = updateState packagesState
                                $ UpdatePackageInfo pkgId pkgInfo

    partitionLogMsg :: Int -> Int -> String
    partitionLogMsg i n = "Computing blob info "
                       ++ "(" ++ show i ++ "/" ++ show n ++ ")"

-- | Similar migration for candidates
migrateCandidatePkgTarball_v1_to_v2 :: ServerEnv
                                    -> StateComponent AcidState CandidatePackages
                                    -> IO ()
migrateCandidatePkgTarball_v1_to_v2 env@ServerEnv{ serverVerbosity = verbosity }
                                    candidatesState
                                    = do
    precomputedHashes <- readPrecomputedHashes env
    CandidatePackages{candidateList} <- queryState candidatesState GetCandidatePackages
    let allCandidates = PackageIndex.allPackages candidateList
        partitionSz   = PackageIndex.numPackageVersions candidateList `div` 10
        partitioned   = partition partitionSz allCandidates
    stats <- forM (zip [1..] partitioned) $ \(i, candidates) -> do
      let pkgs = map candPkgInfo candidates
      logTiming verbosity (partitionLogMsg i (length partitioned)) $
        migratePkgs env updatePackage precomputedHashes pkgs
    loginfo verbosity $ prettyMigrationStats (mconcat stats)
  where
    updatePackage :: PackageId -> PkgInfo -> IO ()
    updatePackage pkgId pkgInfo = do
      _didUpdate <- updateState candidatesState $
                      UpdateCandidatePkgInfo pkgId pkgInfo
      return ()

    partitionLogMsg :: Int -> Int -> String
    partitionLogMsg i n = "Computing candidates blob info "
                       ++ "(" ++ show i ++ "/" ++ show n ++ ")"

migratePkgs :: ServerEnv
            -> (PackageId -> PkgInfo -> IO ())
            -> Precomputed
            -> [PkgInfo]
            -> IO MigrationStats
migratePkgs ServerEnv{ serverBlobStore = store } updatePackage precomputed =
    liftM mconcat . mapM migratePkg
  where
    migratePkg :: PkgInfo -> IO MigrationStats
    migratePkg pkg = do
        tarballs' <- forM tarballs $ \(tarball, uploadInfo) -> do
          tarball' <- migrateTarball tarball
          return $ (, uploadInfo) <$> tarball'
        -- Avoid updating the state of all packages already migrated
        case sequence tarballs' of
          AlreadyMigrated _ ->
            return mempty
          Migrated stats tarballs'' -> do
            let pkg' = pkg { pkgTarballRevisions = Vec.fromList tarballs'' }
            updatePackage (pkgInfoId pkg) pkg'
            return stats
      where
        tarballs = Vec.toList (pkgTarballRevisions pkg)

    migrateTarball :: PkgTarball -> IO (Migrated PkgTarball)
    migrateTarball pkgTarball@PkgTarball{} =
        return $ AlreadyMigrated pkgTarball
    migrateTarball (PkgTarball_v2_v1 PkgTarball_v1{..}) =
        case Map.lookup (blobMd5 v1_pkgTarballGz) precomputed of
          Just (strSHA256, len) -> do
            -- We assume all SHA hashes in the precomputed list parse OK
            let Right sha256 = readDigest strSHA256
                stats  = MigrationStats 1 0
                infoGz = BlobInfo {
                    blobInfoId         = v1_pkgTarballGz
                  , blobInfoLength     = len
                  , blobInfoHashSHA256 = sha256
                  }
            return $ Migrated stats PkgTarball {
                pkgTarballGz   = infoGz
              , pkgTarballNoGz = v1_pkgTarballNoGz
              }
          Nothing -> do
            infoGz <- blobInfoFromId store v1_pkgTarballGz
            let stats = MigrationStats 0 1
            return $ Migrated stats PkgTarball {
                pkgTarballGz   = infoGz
              , pkgTarballNoGz = v1_pkgTarballNoGz
              }

{-------------------------------------------------------------------------------
  Precomputed hashes
-------------------------------------------------------------------------------}

type MD5         = String
type SHA256      = String
type Length      = Int
type Precomputed = Map MD5 (SHA256, Length)

-- | Read precomputed hashes (if any)
--
-- The result is guaranteed to be in normal form.
readPrecomputedHashes :: ServerEnv -> IO Precomputed
readPrecomputedHashes env@ServerEnv{ serverVerbosity = verbosity } = do
    precomputed <- handle emptyOnError $
      withFile (onDiskPrecomputedHashes env) ReadMode $ \h -> do
        hashes <- Map.fromList . map parseEntry . lines <$> hGetContents h
        evaluate $ rnf hashes
        return hashes
    loginfo verbosity $ "Found " ++ show (Map.size precomputed)
                     ++ " precomputed hashes"
    return precomputed
  where
    emptyOnError :: IOException -> IO Precomputed
    emptyOnError err = if isDoesNotExistError err then return Map.empty
                                                  else throwIO err

    parseEntry :: String -> (MD5, (SHA256, Length))
    parseEntry line = let [md5, sha256, len] = words line
                      in (md5, (sha256, read len))

{-------------------------------------------------------------------------------
  Migration infrastructure
-------------------------------------------------------------------------------}

data MigrationStats = MigrationStats {
    -- | Number of hashes we lookup up in the precomputed map
    migrationStatsPrecomputed :: !Int

    -- | Number of hashes we had to compute
  , migrationStatsComputed :: !Int
  }

prettyMigrationStats :: MigrationStats -> String
prettyMigrationStats MigrationStats{..} = unwords [
      show migrationStatsPrecomputed
    , "hashes were precomputed, computed"
    , show migrationStatsComputed
    ]

instance Monoid MigrationStats where
  mempty = MigrationStats 0 0
  mappend = (<>)

instance Semigroup MigrationStats where
  (MigrationStats a b) <> (MigrationStats a' b') =
    MigrationStats (a + a') (b + b')

data Migrated a = Migrated MigrationStats a | AlreadyMigrated a
  deriving (Functor)

instance Applicative Migrated where
  pure    = return
  f <*> x = do f' <- f ; x' <- x ; return $ f' x'

instance Monad Migrated where
  return = AlreadyMigrated
  AlreadyMigrated a >>= f = f a
  Migrated stats  a >>= f =
    case f a of
      AlreadyMigrated b -> Migrated stats b
      Migrated stats' b -> Migrated (stats <> stats') b

{-------------------------------------------------------------------------------
  Additional auxiliary
-------------------------------------------------------------------------------}

-- | Partition list
--
-- > partition 2 [1..5] = [[1,2],[3,4],[5]]
--
-- If partition size is 0, returns a single partition
partition :: Int -> [a] -> [[a]]
partition 0  xs = [xs]
partition _  [] = []
partition sz xs = let (firstPartition, xs') = splitAt sz xs
                  in firstPartition : partition sz xs'
