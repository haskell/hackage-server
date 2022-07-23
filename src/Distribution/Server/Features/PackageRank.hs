module Distribution.Server.Features.PackageRank
  ( rankPackage
  ) where

import           Distribution.Package
import           Distribution.PackageDescription
import           Distribution.Server.Features.Core
import           Distribution.Server.Features.Documentation
                                                ( DocumentationFeature(..) )
import           Distribution.Server.Features.DownloadCount
import           Distribution.Server.Features.PreferredVersions
import           Distribution.Server.Features.PreferredVersions.State
import           Distribution.Server.Features.Upload
import           Distribution.Server.Framework  ( ServerPartE )
import           Distribution.Server.Framework.BlobStorage
                                                ( BlobId )
import           Distribution.Server.Framework.Feature
                                                ( queryState )
import           Distribution.Server.Packages.Types
import           Distribution.Server.Users.Group
                                                ( queryUserGroups
                                                , size
                                                )
import           Distribution.Server.Util.CountingMap
                                                ( cmFind )
import           Distribution.Types.Version

import           Control.Monad.IO.Class         ( liftIO )
import           Data.List                      ( maximumBy
                                                , sortBy
                                                )
import           Data.Maybe                     ( isNothing )
import           Data.Ord                       ( comparing
                                                , max
                                                , min
                                                )
import           Data.Time.Clock                ( UTCTime(..)
                                                , diffUTCTime
                                                , getCurrentTime
                                                , nominalDay
                                                )
import           Distribution.Simple.Utils      ( safeHead
                                                , safeLast
                                                )
import qualified Distribution.Utils.ShortText  as S
import           GHC.Float                      ( int2Double )

data Scorer = Scorer
  { maximum :: Double
  , score   :: Double
  }

instance Semigroup Scorer where
  (Scorer a b) <> (Scorer c d) = Scorer (a + b) (c + d)

scorer :: Double -> Double -> Scorer
scorer maxim scr = if maxim >= scr then (Scorer maxim scr) else (Scorer maxim maxim)

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

-- TODO CoreFeature can be substituted by CoreResource
rankIO
  :: CoreResource
  -> VersionsFeature
  -> DownloadFeature
  -> UploadFeature
  -> DocumentationFeature
  -> PackageDescription
  -> ServerPartE Scorer

rankIO core vers downs upl docs pkg = do
  temp  <- temporalScore pkg lastUploads versionList downloadsPerMonth
  versS <- versionScore versionList vers lastUploads pkg
  auth  <- authorScore upl pkg
  return (temp <> versS <> auth)

 where
  pkgId        = package pkg
  pkgNm        = pkgName pkgId
  info         = lookupPackageName core pkgNm
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
  downloadsPerMonth = liftIO $ cmFind pkgNm <$> recentPackageDownloads downs
  -- TODO get appropriate pkgInfo (head might fail)
  packageTarball    = pkgLatestTarball . head <$> info
  documentTarball :: ServerPartE (Maybe BlobId)
  documentTarball = queryDocumentation docs pkgId

--      mdocs <- queryState documentationState $ LookupDocumentation pkgid
--           case mdocs of
--             Nothing ->
--               errNotFoundH "Not Found"
--                 [ MText "There is no documentation for "
--                 , MLink (display pkgid) ("/package/" ++ display pkgid)
--                 , MText ". See "
--                 , MLink canonicalLink canonicalLink
--                 , MText " for the latest version."
--                 ]
--               where
--                 -- Essentially errNotFound, but overloaded to specify a header.
--                 -- (Needed since errNotFound throws away result of setHeaderM)
--                 errNotFoundH title message = throwError
--                   (ErrorResponse 404
--                   [("Link", canonicalHeader)]
--                   title message)
--             Just blob -> do
--               index <- liftIO $ cachedTarIndex blob
--               func pkgid blob index

authorScore :: UploadFeature -> PackageDescription -> ServerPartE Scorer
authorScore upload desc =
  liftIO maintScore
    >>= (\x -> return $ boolScor 1 (not $ S.null $ author desc) <> x)
 where
  pkgNm = pkgName $ package desc
  maintScore :: IO Scorer
  maintScore = do
    maint <- queryUserGroups [maintainersGroup upload pkgNm]

    return $ boolScor 3 (size maint > 1) <> scorer 5 (int2Double $ size maint)


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
  -> ServerPartE Int
  -> ServerPartE Scorer
temporalScore p lastUploads versionList downloadsPerMonth = do
  fresh <- freshnessScore
  downs <- downloadScore
  tract <- tractionScore
  return $ tract <> fresh <> downs
 where
  isApp         = (isNothing . library) p && (not . null . executables) p
  downloadScore = calcDownScore <$> downloadsPerMonth
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
  tractionScore  = do
    fresh <- packageFreshness
    downs <- downloadsPerMonth
    return $ boolScor 1 (fresh * int2Double downs > 1000)

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
  -> DownloadFeature
  -> UploadFeature
  -> DocumentationFeature
  -> PackageDescription
  -> ServerPartE Double
rankPackage core versions download upload docs p =
  rankIO core versions download upload docs p
    >>= (\x -> return $ total x + total (rankPackagePage p))


