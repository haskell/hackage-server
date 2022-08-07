{-# LANGUAGE TupleSections #-}

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
import qualified Codec.Archive.Tar.Entry       as Tar
import           Control.Monad                  ( join
                                                , liftM2
                                                )
import qualified Data.ByteString.Lazy          as BSL
import           Data.List                      ( maximumBy
                                                , sortBy
                                                )
import           Data.Maybe                     ( isNothing )
import           Data.Ord                       ( comparing )
import qualified Data.TarIndex                 as T
import qualified Data.Time.Clock               as CL
import           GHC.Float                      ( int2Float )
import           System.FilePath                ( isExtensionOf )
import qualified System.IO                     as SIO

data Scorer = Scorer
  { maximum :: Float
  , score   :: Float
  }
  deriving Show

instance Semigroup Scorer where
  (Scorer a b) <> (Scorer c d) = Scorer (a + b) (c + d)

scorer :: Float -> Float -> Scorer
scorer maxim scr =
  if maxim >= scr then Scorer maxim scr else Scorer maxim maxim

fracScor :: Float -> Float -> Scorer
fracScor maxim frac = scorer maxim (maxim * frac)

boolScor :: Float -> Bool -> Scorer
boolScor k True  = Scorer k k
boolScor k False = Scorer k 0

total :: Scorer -> Float
total (Scorer a b) = a / b

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
  expectedUpdateInterval = int2Float
    (min (versionStabilityInterval versionLatest) $ length (x : xs))
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
  -> IO Scorer

rankIO vers recentDownloads maintainers docs env tarCache pkgs = do
  temp  <- temporalScore pkg lastUploads versionList recentDownloads
  versS <- versionScore versionList vers lastUploads pkg
  codeS <- codeScore documentLines srcLines
  return (temp <> versS <> codeS <> authorScore maintainers pkg)

 where
  pkg   = packageDescription <$> pkgDesc $ last pkgs
  pkgId = package pkg
  lastUploads =
    sortBy (flip compare)
      $  (fst . pkgOriginalUploadInfo <$> pkgs)
      ++ (fst . pkgLatestUploadInfo <$> pkgs)
  versionList :: [Version]
  versionList = sortBy (flip compare)
    $ map (pkgVersion . package . packageDescription) (pkgDesc <$> pkgs)
  packageEntr = do
    tarB <- packageTarball tarCache . head $ pkgs
    return
      $   (\(path, _, index) -> (path, ) <$> T.lookup index path)
      =<< rightToMaybe tarB
  rightToMaybe (Right a) = Just a
  rightToMaybe (Left  _) = Nothing

  documentBlob :: IO (Maybe BlobStorage.BlobId)
  documentBlob      = queryDocumentation docs pkgId
  documentIndex     = documentBlob >>= mapM (cachedTarIndex tarCache)
  documentationEntr = do
    index <- documentIndex
    path  <- documentPath
    return $ liftM2 (,) path (join $ liftM2 T.lookup index path)
  documentLines :: IO Float
  documentLines = documentationEntr >>= filterLinesTar (const True)
  srcLines :: IO Float
  srcLines = packageEntr >>= filterLinesTar (isExtensionOf ".hs")

  filterLinesTar
    :: (FilePath -> Bool) -> Maybe (FilePath, T.TarIndexEntry) -> IO Float
  filterLinesTar f (Just (path, T.TarFileEntry offset)) =
    if f path then getLines path offset else return 0
  filterLinesTar f (Just (_, T.TarDir dir)) =
    sum <$> mapM (filterLinesTar f . Just) dir
  filterLinesTar _ _ = return 0

  -- TODO if size is too big give it a good score and do not read the file
  getLines path offset = do
    handle <- SIO.openFile path SIO.ReadMode
    SIO.hSeek handle SIO.AbsoluteSeek (fromIntegral $ offset * 512)
    header <- BSL.hGet handle 512
    case Tar.read header of
      (Tar.Next Tar.Entry { Tar.entryContent = Tar.NormalFile _ siz } _) -> do
        body <- BSL.hGet handle (fromIntegral siz)
        return $ int2Float . length . BSL.split 10 $ body
      _ -> return 0

  documentPath = do
    blob <- documentBlob
    return $ BlobStorage.filepath (serverBlobStore env) <$> blob

authorScore :: Int -> PackageDescription -> Scorer
authorScore maintainers desc =
  boolScor 1 (not $ S.null $ author desc) <> maintScore
 where
  maintScore =
    boolScor 3 (maintainers > 1) <> scorer 5 (int2Float maintainers)

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
  packageFreshness = case lastUploads of
    [] -> return 0
    _  -> freshness versionList (head lastUploads) isApp
  freshnessScore = fracScor 10 <$> packageFreshness
-- Missing dependencyFreshnessScore for reasonable effectivity needs caching
  tractionScore  = do
    fresh <- packageFreshness
    return $ boolScor 1 (fresh * int2Float recentDownloads > 1000)

rankPackagePage :: PackageDescription -> Scorer
rankPackagePage p = tests <> benchs <> desc <> homeP <> sourceRp <> cats
 where
  tests    = boolScor 50 (hasTests p)
  benchs   = boolScor 10 (hasBenchmarks p)
  desc     = Scorer 30 (min 1 (int2Float (S.length $ description p) / 300))
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
  total
    .   (<>) (rankPackagePage pkgD)
    <$> rankIO versions recentDownloads maintainers docs env tarCache pkgs
  where pkgD = packageDescription $ pkgDesc $ last pkgs
