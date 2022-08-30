{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Distribution.Server.Features.PackageList.PackageRank
  ( rankPackage
  ) where

import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.Server.Features.Documentation
                                                ( DocumentationFeature(..) )
import           Distribution.Server.Features.PackageList.MStats
import           Distribution.Server.Features.PreferredVersions
import           Distribution.Server.Features.PreferredVersions.State
import           Distribution.Server.Features.TarIndexCache
import qualified Distribution.Server.Framework.BlobStorage
                                               as BlobStorage
import           Distribution.Server.Framework.ServerEnv
                                                ( ServerEnv(..) )
import           Distribution.Server.Packages.Types
import           Distribution.Server.Util.Markdown
                                                ( supposedToBeMarkdown )
import           Distribution.Server.Util.ServeTarball
                                                ( loadTarEntry )
import           Distribution.Simple.Utils      ( safeHead
                                                , safeLast
                                                )
import           Distribution.Types.Version
import qualified Distribution.Utils.ShortText  as S

import qualified Codec.Archive.Tar             as Tar
import           Control.Exception              ( SomeException(..)
                                                , handle
                                                )
import qualified Data.ByteString.Lazy          as BSL
import           Data.List                      ( maximumBy
                                                , sortBy
                                                )
import           Data.Maybe                     ( isNothing )
import           Data.Ord                       ( comparing )
import qualified Data.Time.Clock               as CL
import           Distribution.Server.Packages.Readme
import           GHC.Float                      ( int2Float )
import           System.FilePath                ( isExtensionOf )

handleConst :: a -> IO a -> IO a
handleConst c = handle (\(_ :: SomeException) -> return c)

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
fracScor maxim frac = scorer maxim (min (maxim * frac) maxim)

boolScor :: Float -> Bool -> Scorer
boolScor k True  = Scorer k k
boolScor k False = Scorer k 0

total :: Scorer -> Float
total (Scorer a b) = b / a

scale :: Float -> Scorer -> Scorer
scale mx sc = fracScor mx (total sc)

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

cabalScore :: PackageDescription -> Bool -> Scorer
cabalScore p docum =
  tests <> benchs <> desc <> homeP <> sourceRp <> cats <> boolScor 30 docum
 where
  tests    = boolScor 30 (hasTests p)
  benchs   = boolScor 10 (hasBenchmarks p)
  desc     = scorer 30 (min 1 (int2Float (S.length $ description p) / 300))
  homeP    = boolScor 30 (not $ S.null $ homepage p)
  sourceRp = boolScor 8 (not $ null $ sourceRepos p)
  cats     = boolScor 5 (not $ S.null $ category p)

readmeScore :: TarIndexCacheFeature -> PkgInfo -> Bool -> IO Scorer
readmeScore tarCache pkgI app = do
  Just (tarfile, _, offset, name) <- readme
  entr                            <- loadTarEntry tarfile offset
  case entr of
    (Right (size, str)) -> return $ calcScore str size name
    _                   -> return $ Scorer 1 0
 where
  readme = findToplevelFile tarCache pkgI isReadmeFile
    >>= either (\_ -> return Nothing) (return . Just)
  calcScore str size filename =
    scorer 75 (min 1 (fromInteger (toInteger size) / 3000))
      <> if supposedToBeMarkdown filename
           then case parseM str filename of
             Left  _       -> Scorer 0 0
             Right mdStats -> format mdStats
           else Scorer 0 0
  format stats =
    fracScor (if app then 25 else 100) (min 1 $ int2Float hlength / 2000)
      <> scorer (if app then 15 else 27) (int2Float blocks * 3)
      <> boolScor (if app then 10 else 30) (clength > 150)
      <> scorer 35 (int2Float images * 10)
      <> scorer 30 (int2Float sections * 4)
      <> scorer 25 (int2Float rows * 2)
   where
    (blocks, clength) = getCode stats
    (_     , hlength) = getHCode stats
    MStats _ images   = sumMStat stats
    rows              = getListsTables stats
    sections          = getSections stats

baseScore
  :: VersionsFeature
  -> Int
  -> DocumentationFeature
  -> ServerEnv
  -> TarIndexCacheFeature
  -> [Version]
  -> [CL.UTCTime]
  -> PkgInfo
  -> IO Scorer

baseScore vers maintainers docs env tarCache versionList lastUploads pkgI = do

  hasDocum <- handleConst False documHas -- Probably redundant
  documS   <- handleConst 0 documSize
  srcL     <- handleConst 0 srcLines

  versS    <- handleConst (Scorer 1 0)
                          (versionScore versionList vers lastUploads pkg)
  readmeS <- handleConst (Scorer 1 0) (readmeScore tarCache pkgI isApp)
  return
    $  scale 5 versS
    <> scale 2 (codeScore documS srcL)
    <> scale 3 (authorScore maintainers pkg)
    <> scale 2 (cabalScore pkg hasDocum)
    <> scale 5 readmeS
 where
  pkg      = packageDescription $ pkgDesc pkgI
  pkgId    = package pkg
  isApp    = (isNothing . library) pkg && (not . null . executables) pkg
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
      -- TODO might need to decode/add the other separator
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
  documHas = queryHasDocumentation docs pkgId

authorScore :: Int -> PackageDescription -> Scorer
authorScore maintainers desc =
  boolScor 1 (not $ S.null $ author desc) <> maintScore
 where
  maintScore = boolScor 3 (maintainers > 1) <> scorer 5 (int2Float maintainers)

codeScore :: Float -> Float -> Scorer
codeScore documentS haskellL =
  boolScor 1 (haskellL > 700)
    <> boolScor 1 (haskellL < 80000)
    <> fracScor 2 (min 1 (haskellL / 5000))
    <> fracScor 2 (min 1 (documentS / ((3000 + haskellL) * 1600)))

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
  calculateScore depre lUps intUse =
    boolScor 20 (length intUse > 1)
      <> scorer 40 (numDays (safeHead lUps) (safeLast lUps) / 11)
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
  -- Reverse dependencies are added
  return $ tract <> fresh <> downloadScore
 where
  isApp         = (isNothing . library) p && (not . null . executables) p
  downloadScore = calcDownScore recentDownloads
  calcDownScore i = fracScor
    5
    ( (logBase 2 (int2Float $ max 0 (i - 32) + 32) - 5)
    / (if isApp then 6 else 8)
    )
  packageFreshness = case safeHead lastUploads of
    Nothing  -> return 0
    (Just l) -> freshness versionList l isApp -- Getting time hopefully does not throw Exc.
  freshnessScore = fracScor 10 <$> packageFreshness
  -- Missing dependencyFreshnessScore for reasonable effectivity needs caching
  tractionScore  = do
    fresh <- packageFreshness
    return $ boolScor 1 (fresh * int2Float recentDownloads > 200)

rankPackage
  :: VersionsFeature
  -> Int
  -> Int
  -> DocumentationFeature
  -> TarIndexCacheFeature
  -> ServerEnv
  -> [PkgInfo]
  -> Maybe PkgInfo
  -> IO Float
rankPackage _ _ _ _ _ _ _ Nothing = return 0
rankPackage versions recentDownloads maintainers docs tarCache env pkgs (Just pkgUsed)
  = do
    t <- temporalScore pkgD uploads versionList recentDownloads

    b <- baseScore versions
                   maintainers
                   docs
                   env
                   tarCache
                   versionList
                   uploads
                   pkgUsed
    depr <- handleConst Nothing deprP
    return $ sAverage t b * case depr of
      Nothing -> 1
      _       -> 0.2
 where
  pkgname = pkgName . package $ pkgD
  pkgD    = packageDescription . pkgDesc $ pkgUsed
  deprP   = queryGetDeprecatedFor versions pkgname
  sAverage x y = (total x + total y) * 0.5

  versionList :: [Version]
  versionList = sortBy (flip compare)
    $ map (pkgVersion . package . packageDescription) (pkgDesc <$> pkgs)
  uploads =
    sortBy (flip compare)
      $  (fst . pkgOriginalUploadInfo <$> pkgs)
      ++ (fst . pkgLatestUploadInfo <$> pkgs)
