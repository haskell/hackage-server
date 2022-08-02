{-# LANGUAGE TupleSections #-}

module Distribution.Server.Features.PackageRank
  ( rankPackage
  ) where

import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.Server.Features.Core
import           Distribution.Server.Features.Documentation
                                                ( DocumentationFeature(..) )
import           Distribution.Server.Features.PackageList
import           Distribution.Server.Features.PreferredVersions
import           Distribution.Server.Features.PreferredVersions.State
import           Distribution.Server.Features.TarIndexCache
import           Distribution.Server.Framework  ( ServerPartE )
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
import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.ByteString.Lazy          as BSL
import           Data.List                      ( maximumBy
                                                , sortBy
                                                )
import           Data.Maybe                     ( isNothing )
import           Data.Ord                       ( comparing )
import qualified Data.TarIndex                 as T
import           Data.Time.Clock                ( UTCTime(..)
                                                , diffUTCTime
                                                , getCurrentTime
                                                , nominalDay
                                                )
import           GHC.Float                      ( int2Double )
import           System.FilePath                ( isExtensionOf )
import qualified System.IO                     as SIO

data Scorer = Scorer
  { maximum :: Double
  , score   :: Double
  }
  deriving Show

instance Semigroup Scorer where
  (Scorer a b) <> (Scorer c d) = Scorer (a + b) (c + d)

scorer :: Double -> Double -> Scorer
scorer maxim scr =
  if maxim >= scr then Scorer maxim scr else Scorer maxim maxim

fracScor :: Double -> Double -> Scorer
fracScor maxim frac = scorer maxim (maxim * frac)

boolScor :: Double -> Bool -> Scorer
boolScor k True  = Scorer k k
boolScor k False = Scorer k 0

total :: Scorer -> Double
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

numDays :: Maybe UTCTime -> Maybe UTCTime -> Double
numDays (Just first) (Just end) =
  fromRational $ toRational $ diffUTCTime first end / fromRational
    (toRational nominalDay)
numDays _ _ = 0

freshness :: [Version] -> UTCTime -> Bool -> IO Double
freshness [] _ _ = return 0
freshness (x : xs) lastUpd app =
  daysPastExpiration
    >>= (\dExp -> return $ max 0 $ (decayDays - dExp) / decayDays)
 where
  versionLatest = versionNumbers x
  daysPastExpiration =
    age >>= (\a -> return $ max 0 a - expectedUpdateInterval)
  expectedUpdateInterval = int2Double
    (min (versionStabilityInterval versionLatest) $ length (x : xs))
  versionStabilityInterval v | patches v > 3 && major v > 0 = 700
                             | patches v > 3                = 450
                             | patches v > 0                = 300
                             | major v > 0                  = 200
                             | minor v > 3                  = 140
                             | otherwise                    = 80
  age       = flip numDays (Just lastUpd) . Just <$> getCurrentTime
  decayDays = expectedUpdateInterval / 2 + (if app then 300 else 200)

-- lookupPackageId
-- queryHasDocumentation

rankIO
  :: CoreResource
  -> VersionsFeature
  -> DocumentationFeature
  -> ServerEnv
  -> TarIndexCacheFeature
  -> ListFeature
  -> PackageDescription
  -> ServerPartE Scorer

rankIO core vers docs env tarCache list pkg = do
  temp  <- temporalScore pkg lastUploads versionList downloadsPerMonth
  versS <- versionScore versionList vers lastUploads pkg
  auth  <- authorScore pkg pkgIt
  codeS <- codeScore documentLines srcLines packageLines
  return (temp <> versS <> auth <> codeS)

 where
  pkgId        = package pkg
  pkgNm        = pkgName pkgId
  info         = lookupPackageName core pkgNm
  pkgIt        = safeHead <$> makeItemList list [pkgNm]
  descriptions = do
    infPkg <- info
    return (pkgDesc <$> infPkg)
  lastUploads = do
    infPkg <- info
    return $ sortBy (flip compare) $ fst . pkgOriginalUploadInfo <$> infPkg
  versionList =
    do
        sortBy (flip compare)
      .   map (pkgVersion . package . packageDescription)
      <$> descriptions
  downloadsPerMonth :: ServerPartE (Maybe Int)
  downloadsPerMonth = liftIO $ do
    items <- pkgIt
    return (itemDownloads <$> items)
  -- TODO get appropriate pkgInfo (head might fail)
  packageEntr = do
    inf  <- info
    tarB <- liftIO $ mapM (packageTarball tarCache) (safeHead inf)
    return
      $   (\(path, _, index) -> (path, ) <$> T.lookup index path)
      =<< (join $ rightToMaybe <$> tarB)

  rightToMaybe (Right a) = Just a
  rightToMaybe (Left  _) = Nothing

  documentBlob :: ServerPartE (Maybe BlobStorage.BlobId)
  documentBlob      = queryDocumentation docs pkgId
  documentIndex     = documentBlob >>= liftIO . mapM (cachedTarIndex tarCache)
  documentationEntr = do
    index <- documentIndex
    path  <- documentPath
    return $ liftM2 (,) path (join $ liftM2 T.lookup index path)
  documentLines = documentationEntr >>= liftIO . filterLinesTar (const True)
  srcLines      = packageEntr >>= liftIO . filterLinesTar (isExtensionOf ".hs")
  packageLines  = packageEntr >>= liftIO . filterLinesTar (const True)

  filterLinesTar
    :: (FilePath -> Bool) -> Maybe (FilePath, T.TarIndexEntry) -> IO Double
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
        return
          $ int2Double
          . length
          . filter (not . BSL.null)
          . BSL.split 10
          $ body
      _ -> return 0

  documentPath = do
    blob <- documentBlob
    return $ BlobStorage.filepath (serverBlobStore env) <$> blob

authorScore
  :: PackageDescription -> IO (Maybe PackageItem) -> ServerPartE Scorer
authorScore desc item =
  liftIO maintScore
    >>= (\x -> return $ boolScor 1 (not $ S.null $ author desc) <> x)
 where
  pkgNm = pkgName $ package desc
  maintScore :: IO Scorer
  maintScore = do
    it <- item
    return $ boolScor 3 (nMaint it > 1) <> scorer 5 (int2Double $ nMaint it)
  nMaint (Just iT) = length $ itemMaintainer iT
  nMaint Nothing   = 0

codeScore
  :: ServerPartE Double
  -> ServerPartE Double
  -> ServerPartE Double
  -> ServerPartE Scorer
codeScore documentL haskellL packageL = do
  docum   <- documentL
  haskell <- haskellL
  pkg     <- packageL
  return
    $  boolScor 1 (pkg > 700)
    <> boolScor 1 (pkg < 80000)
    <> fracScor 2 (min 1 (haskell / 5000))
    <> fracScor 2 (min 1 (10 * docum) / (3000 + haskell))

versionScore
  :: ServerPartE [Version]
  -> VersionsFeature
  -> ServerPartE [UTCTime]
  -> PackageDescription
  -> ServerPartE Scorer
versionScore versionList versions lastUploads desc = do
  intUse <- intUsable
  depre  <- deprec
  lUps   <- lastUploads
  return $ calculateScore depre lUps intUse
 where
  pkgNm = pkgName $ package desc
  partVers =
    versionList
      >>= (\y ->
            liftIO
              $   queryGetPreferredInfo versions pkgNm
              >>= (\x -> return $ partitionVersions x y)
          )
  intUsable = do
    (norm, _, unpref) <- partVers
    return $ versionNumbers <$> norm ++ unpref
  deprec = do
    (_, deprecN, _) <- partVers
    return deprecN
  calculateScore :: [Version] -> [UTCTime] -> [[Int]] -> Scorer
  calculateScore [] _ _ = Scorer 118 0
  calculateScore depre lUps intUse =
    boolScor 20 (length intUse > 1)
      <> scorer 40 (numDays (safeHead lUps) (safeLast lUps))
      <> scorer
           15
           (int2Double $ length $ filter (\x -> major x > 0 || minor x > 0)
                                         intUse
           )
      <> scorer
           20
           (int2Double $ 4 * length
             (filter (\x -> major x > 0 && patches x > 0) intUse)
           )
      <> scorer 10 (int2Double $ patches $ maximumBy (comparing patches) intUse)
      <> boolScor 8 (any (\x -> major x == 0 && patches x > 0) intUse)
      <> boolScor 10 (any (\x -> major x > 0 && major x < 20) intUse)
      <> boolScor 5  (not $ null depre)

temporalScore
  :: PackageDescription
  -> ServerPartE [UTCTime]
  -> ServerPartE [Version]
  -> ServerPartE (Maybe Int)
  -> ServerPartE Scorer
temporalScore p lastUploads versionList downloadsPM = do
  download <- downloadsPM
  fresh    <- freshnessScore
  downS    <- downloadScore download
  tract    <- tractionScore download
  return $ tract <> fresh <> downS
 where
  isApp = (isNothing . library) p && (not . null . executables) p
  downloadScore Nothing          = return $ scorer 5 0
  downloadScore (Just downloads) = return $ calcDownScore downloads
  calcDownScore i = Scorer 5 $ min
    ( (logBase 2 (int2Double $ max 0 (i - 100) + 100) - 6.6)
    / (if isApp then 5 else 6)
    )
    5
  packageFreshness = do
    ups  <- lastUploads
    vers <- versionList
    case ups of
      [] -> return 0
      _  -> liftIO $ freshness vers (head ups) isApp
  freshnessScore = fracScor 10 <$> packageFreshness
-- Missing dependencyFreshnessScore for reasonable effectivity needs caching
  tractionScore Nothing          = return $ scorer 1 0
  tractionScore (Just downloads) = do
    fresh <- packageFreshness
    return $ boolScor 1 (fresh * int2Double downloads > 1000)

rankPackagePage :: PackageDescription -> Scorer
rankPackagePage p = tests <> benchs <> desc <> homeP <> sourceRp <> cats
 where
  tests    = boolScor 50 (hasTests p)
  benchs   = boolScor 10 (hasBenchmarks p)
  desc     = Scorer 30 (min 1 (int2Double (S.length $ description p) / 300))
  -- documentation = boolScor 30 ()
  homeP    = boolScor 30 (not $ S.null $ homepage p)
  sourceRp = boolScor 8 (not $ null $ sourceRepos p)
  cats     = boolScor 5 (not $ S.null $ category p)

-- TODO fix the function Signature replace PackageDescription to PackageName/Identifier

rankPackage
  :: CoreResource
  -> VersionsFeature
  -> DocumentationFeature
  -> ServerEnv
  -> TarIndexCacheFeature
  -> ListFeature
  -> PackageDescription
  -> ServerPartE Double
rankPackage core versions docs env tarCache list p =
  rankIO core versions docs env tarCache list p
    >>= (\x -> return $ total x + total (rankPackagePage p))
