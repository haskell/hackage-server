{-# LANGUAGE BangPatterns #-}

-- TODO change the module name probably Distribution.Server.Features.PackageList.PackageRank

module Distribution.Server.Features.PackageRank
  ( rankPackage
  ) where

import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.Server.Features.Documentation
                                                ( DocumentationFeature(..) )
import           Distribution.Server.Features.PreferredVersions
import           Distribution.Server.Features.PreferredVersions.State
import           Distribution.Server.Features.TarIndexCache
import qualified Distribution.Server.Framework.BlobStorage
                                               as BlobStorage
import           Distribution.Server.Framework.ServerEnv
                                                ( ServerEnv(..) )
import           Distribution.Server.Packages.Types
import           Distribution.Simple.Utils      ( safeHead
                                                , safeLast
                                                )
import           Distribution.Types.Version
import qualified Distribution.Utils.ShortText  as S

import qualified Codec.Archive.Tar             as Tar
import qualified Data.ByteString.Lazy          as BSL
import           Data.List                      ( maximumBy
                                                , sortBy
                                                )
import           Data.Maybe                     ( isNothing )
import           Data.Ord                       ( comparing )
import qualified Data.Time.Clock               as CL
import           GHC.Float                      ( int2Float )
import           System.FilePath                ( isExtensionOf )

-- import Debug.Trace (trace)

data Scorer = Scorer
  { maximumS :: !Float
  , score    :: !Float
  }
  deriving Show

instance Semigroup Scorer where
  (Scorer a b) <> (Scorer c d) = Scorer (a + c) (b + d)

scorer :: Float -> Float -> Scorer
scorer maxim scr =
  if maxim >= scr then Scorer maxim scr else Scorer maxim maxim

fracScor :: Float -> Float -> Scorer
fracScor maxim frac = scorer maxim (maxim * frac)

boolScor :: Float -> Bool -> Scorer
boolScor k True  = Scorer k k
boolScor k False = Scorer k 0

total :: Scorer -> Float
total (Scorer a b) = b / a

major :: Num a => [a] -> a
major (x : _) = x
major _       = 0
minor :: Num a => [a] -> a
minor (_ : y : _) = y
minor _           = 0
patches :: Num a => [a] -> a
patches (_ : _ : xs) = sum xs
patches _            = 0

numDays :: Maybe CL.UTCTime -> Maybe CL.UTCTime -> Float
numDays (Just first) (Just end) =
  fromRational $ toRational $ CL.diffUTCTime first end / fromRational
    (toRational CL.nominalDay)
numDays _ _ = 0

freshness :: [Version] -> CL.UTCTime -> Bool -> IO Float
freshness [] _ _ = return 0
freshness (x : xs) lastUpd app =
  daysPastExpiration
    >>= (\dExp -> return $ max 0 $ (decayDays - dExp) / decayDays)
 where
  versionLatest = versionNumbers x
  daysPastExpiration =
    age >>= (\a -> return $ max 0 a - expectedUpdateInterval)
  expectedUpdateInterval =
    int2Float (min (versionStabilityInterval versionLatest) $ length (x : xs))
  versionStabilityInterval v | patches v > 3 && major v > 0 = 700
                             | patches v > 3                = 450
                             | patches v > 0                = 300
                             | major v > 0                  = 200
                             | minor v > 3                  = 140
                             | otherwise                    = 80
  age       = flip numDays (Just lastUpd) . Just <$> CL.getCurrentTime
  decayDays = expectedUpdateInterval / 2 + (if app then 300 else 200)

-- lookupPackageId
-- queryHasDocumentation

-- TODO CoreFeature can be substituted by CoreResource
rankIO
  :: VersionsFeature
  -> Int
  -> Int
  -> DocumentationFeature
  -> ServerEnv
  -> TarIndexCacheFeature
  -> [PkgInfo]
  -> Maybe PkgInfo
  -> IO Scorer

rankIO _ _ _ _ _ _ _ Nothing = return (Scorer (118 + 16 + 4 + 1) 0)
rankIO vers recentDownloads maintainers docs env tarCache pkgs (Just pkgI) = do
  temp  <- temporalScore pkg lastUploads versionList recentDownloads
  versS <- versionScore versionList vers lastUploads pkg
  codeS <- codeScore documSize srcLines
  return $ temp <> versS <> codeS <> authorScore maintainers pkg

 where
  pkg   = packageDescription $ pkgDesc pkgI
  pkgId = package pkg
  lastUploads =
    sortBy (flip compare)
      $  (fst . pkgOriginalUploadInfo <$> pkgs)
      ++ (fst . pkgLatestUploadInfo <$> pkgs)
  versionList :: [Version]
  versionList = sortBy (flip compare)
    $ map (pkgVersion . package . packageDescription) (pkgDesc <$> pkgs)
  srcLines = do
    Right (path, _, _) <- packageTarball tarCache pkgI
    filterLines (isExtensionOf ".hs") countLines
      .   Tar.read
      <$> BSL.readFile path
  documSize = do
    path <- documentPath
    case path of
      Nothing -> return 0
      Just pth ->
        filterLines (isExtensionOf ".html") countSize
          .   Tar.read
          <$> BSL.readFile pth

  filterLines f g = Tar.foldEntries (g f) 0 (const 0)
  countLines :: (FilePath -> Bool) -> Tar.Entry -> Float -> Float
  countLines f entry l = if not . f . Tar.entryPath $ entry then l else lns
   where
    !lns = case Tar.entryContent entry of
      (Tar.NormalFile str _) -> l + (int2Float . length $ BSL.split 10 str)
      _                      -> l
  countSize :: (FilePath -> Bool) -> Tar.Entry -> Float -> Float
  countSize f entry l = if not . f . Tar.entryPath $ entry then l else s
   where
    !s = case Tar.entryContent entry of
      (Tar.NormalFile _ siz) -> l + fromInteger (toInteger siz)
      _                      -> l

  documentBlob :: IO (Maybe BlobStorage.BlobId)
  documentBlob = queryDocumentation docs pkgId
  documentPath = do
    blob <- documentBlob
    return $ BlobStorage.filepath (serverBlobStore env) <$> blob

authorScore :: Int -> PackageDescription -> Scorer
authorScore maintainers desc =
  boolScor 1 (not $ S.null $ author desc) <> maintScore
 where
  maintScore = boolScor 3 (maintainers > 1) <> scorer 5 (int2Float maintainers)

codeScore :: IO Float -> IO Float -> IO Scorer
codeScore documentL haskellL = do
  docum   <- documentL
  haskell <- haskellL
  return
    $  boolScor 1 (haskell > 700)
    <> boolScor 1 (haskell < 80000)
    <> fracScor 2 (min 1 (haskell / 5000))
    <> fracScor 2 (min 1 (10 * docum) / (3000 + haskell))

versionScore
  :: [Version]
  -> VersionsFeature
  -> [CL.UTCTime]
  -> PackageDescription
  -> IO Scorer
versionScore versionList versions lastUploads desc = do
  use   <- intUsable
  depre <- deprec
  return $ calculateScore depre lastUploads use
 where
  pkgNm = pkgName $ package desc
  partVers =
    flip partitionVersions versionList <$> queryGetPreferredInfo versions pkgNm
  intUsable = do
    (norm, _, unpref) <- partVers
    return $ versionNumbers <$> norm ++ unpref
  deprec = do
    (_, deprecN, _) <- partVers
    return deprecN
  calculateScore :: [Version] -> [CL.UTCTime] -> [[Int]] -> Scorer
  calculateScore [] _ _ = Scorer 118 0
  calculateScore depre lUps intUse =
    boolScor 20 (length intUse > 1)
      <> scorer 40 (numDays (safeHead lUps) (safeLast lUps))
      <> scorer
           15
           (int2Float $ length $ filter (\x -> major x > 0 || minor x > 0)
                                        intUse
           )
      <> scorer
           20
           (int2Float $ 4 * length
             (filter (\x -> major x > 0 && patches x > 0) intUse)
           )
      <> scorer 10 (int2Float $ patches $ maximumBy (comparing patches) intUse)
      <> boolScor 8 (any (\x -> major x == 0 && patches x > 0) intUse)
      <> boolScor 10 (any (\x -> major x > 0 && major x < 20) intUse)
      <> boolScor 5  (not $ null depre)

temporalScore
  :: PackageDescription -> [CL.UTCTime] -> [Version] -> Int -> IO Scorer
temporalScore p lastUploads versionList recentDownloads = do
  fresh <- freshnessScore
  tract <- tractionScore
  return $ tract <> fresh <> downloadScore
 where
  isApp         = (isNothing . library) p && (not . null . executables) p
  downloadScore = calcDownScore recentDownloads
  calcDownScore i = Scorer 5 $ min
    ( (logBase 2 (int2Float $ max 0 (i - 100) + 100) - 6.6)
    / (if isApp then 5 else 6)
    )
    5
  packageFreshness = case safeHead lastUploads of
    Nothing  -> return 0
    (Just l) -> freshness versionList l isApp
  freshnessScore = fracScor 10 <$> packageFreshness
-- Missing dependencyFreshnessScore for reasonable effectivity needs caching
  tractionScore  = do
    fresh <- packageFreshness
    return $ boolScor 1 (fresh * int2Float recentDownloads > 1000)

rankPackagePage :: Maybe PackageDescription -> Scorer
rankPackagePage Nothing  = Scorer 233 0
rankPackagePage (Just p) = tests <> benchs <> desc <> homeP <> sourceRp <> cats
 where
  tests    = boolScor 50 (hasTests p)
  benchs   = boolScor 10 (hasBenchmarks p)
  desc     = scorer 30 (min 1 (int2Float (S.length $ description p) / 300))
  -- documentation = boolScor 30 ()
  homeP    = boolScor 30 (not $ S.null $ homepage p)
  sourceRp = boolScor 8 (not $ null $ sourceRepos p)
  cats     = boolScor 5 (not $ S.null $ category p)

-- TODO fix the function Signature replace PackageDescription to PackageName/Identifier

rankPackage
  :: VersionsFeature
  -> Int
  -> Int
  -> DocumentationFeature
  -> TarIndexCacheFeature
  -> ServerEnv
  -> [PkgInfo]
  -> IO Float
rankPackage versions recentDownloads maintainers docs tarCache env pkgs =
  total . (<>) (rankPackagePage pkgD) <$> rankIO versions
                                                 recentDownloads
                                                 maintainers
                                                 docs
                                                 env
                                                 tarCache
                                                 pkgs
                                                 (safeLast pkgs)
  where pkgD = packageDescription . pkgDesc <$> safeLast pkgs
