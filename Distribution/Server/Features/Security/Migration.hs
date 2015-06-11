{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
-- | Security specific migration
module Distribution.Server.Features.Security.Migration (
    migratePkgTarball_v1_to_v2
  ) where

-- stdlib
import Control.DeepSeq
import Control.Exception
import Data.Binary (Binary)
import Data.Bits
import Data.Map (Map)
import Data.Word (Word8)
import System.IO
import System.IO.Error
import qualified Data.Binary          as Binary
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Map             as Map
import qualified Data.Vector          as Vec

-- hackage
import Distribution.Server.Features.Core.State
import Distribution.Server.Features.Security.Layout
import Distribution.Server.Framework hiding (Length)
import Distribution.Server.Framework.BlobStorage
import Distribution.Server.Packages.Types
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

-- hackage-security
import qualified Data.Digest.Pure.SHA as SHA

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
    precomputedHashes <- readPrecomputedHashes (onDiskPrecomputedHashes env)
    loginfo verbosity $ "Migrating using "
                     ++ show (Map.size precomputedHashes)
                     ++ " precomputed hashes"
    PackagesState {packageIndex} <- queryState packagesState GetPackagesState
    let allPackages = PackageIndex.allPackages packageIndex
        partitionSz = PackageIndex.numPackageVersions packageIndex `div` 10
        partitioned = partition partitionSz allPackages
    stats <- forM (zip [1..] partitioned) $ \(i, pkgs) ->
      logTiming verbosity (partitionLogMsg i) $
        migratePkgs env packagesState precomputedHashes pkgs
    loginfo verbosity $ prettyMigrationStats (mconcat stats)
  where
    partitionLogMsg :: Int -> String
    partitionLogMsg i = "Computing blob info (" ++ show i ++ "0%)"

    partition :: Int -> [a] -> [[a]]
    partition _  [] = []
    partition sz xs = let (firstPartition, xs') = splitAt sz xs
                      in firstPartition : partition sz xs'

migratePkgs :: ServerEnv
            -> StateComponent AcidState PackagesState
            -> Precomputed
            -> [PkgInfo]
            -> IO MigrationStats
migratePkgs ServerEnv{ serverBlobStore = store } packagesState precomputed =
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
            updateState packagesState $ UpdatePackageInfo (pkgInfoId pkg) pkg'
            return stats
      where
        tarballs = Vec.toList (pkgTarballRevisions pkg)

    migrateTarball :: PkgTarball -> IO (Migrated PkgTarball)
    migrateTarball pkgTarball@PkgTarball{} =
        return $ AlreadyMigrated pkgTarball
    migrateTarball (PkgTarball_v2_v1 PkgTarball_v1{..}) =
        case Map.lookup (blobMd5 v1_pkgTarballGz) precomputed of
          Just (sha256, len) -> do
            let stats  = MigrationStats 1 0
                infoGz = BlobInfo {
                    blobInfoId         = v1_pkgTarballGz
                  , blobInfoLength     = len
                  , blobInfoHashSHA256 = readDigest sha256
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
readPrecomputedHashes :: FilePath -> IO Precomputed
readPrecomputedHashes fp =
    handle emptyOnError $
      withFile fp ReadMode $ \h -> do
        hashes <- Map.fromList . map parseEntry . lines <$> hGetContents h
        evaluate $ rnf hashes
        return hashes
  where
    emptyOnError :: IOException -> IO Precomputed
    emptyOnError err = if isDoesNotExistError err then return Map.empty
                                                  else throwIO err

    parseEntry :: String -> (MD5, (SHA256, Length))
    parseEntry line = let [md5, sha256, len] = words line
                      in (md5, (sha256, read len))

-- | Read a digest
--
-- When the digest is forced the whole thing is forced.
--
-- TODO: Is this predefined somewhere?
readDigest :: Binary (SHA.Digest t) => String -> SHA.Digest t
readDigest = Binary.decode . force . BS.L.pack . translate
  where
    translate :: [Char] -> [Word8]
    translate []       = []
    translate [_]      = error "readSHA256: trailing character"
    translate (a:b:xs) = toByte a b : translate xs

    toByte :: Char -> Char -> Word8
    toByte a b = shift (charValue a) 4 .|. charValue b

    charValue :: Char -> Word8
    charValue '0' = 0
    charValue '1' = 1
    charValue '2' = 2
    charValue '3' = 3
    charValue '4' = 4
    charValue '5' = 5
    charValue '6' = 6
    charValue '7' = 7
    charValue '8' = 8
    charValue '9' = 9
    charValue 'A' = 10
    charValue 'B' = 11
    charValue 'C' = 12
    charValue 'D' = 13
    charValue 'E' = 14
    charValue 'F' = 15
    charValue 'a' = 10
    charValue 'b' = 11
    charValue 'c' = 12
    charValue 'd' = 13
    charValue 'e' = 14
    charValue 'f' = 15
    charValue c   = error $ "readSHA256: invalid character " ++ show c

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
  (MigrationStats a b) `mappend` (MigrationStats a' b') =
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
      Migrated stats' b -> Migrated (stats `mappend` stats') b
