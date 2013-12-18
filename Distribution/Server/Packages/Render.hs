-- TODO: Review and possibly move elsewhere. This code was part of the
-- RecentPackages (formerly "Check") feature, but that caused some cyclic
-- dependencies.
module Distribution.Server.Packages.Render (
    -- * Package render
    PackageRender(..)
  , doPackageRender

    -- * Utils
  , categorySplit,
  ) where

import Data.Maybe (catMaybes)
import Control.Monad (guard, liftM2)
import Data.Char (toLower, isSpace)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
import Data.Ord (comparing)
import Data.List (sort, sortBy, partition)
import Data.Time.Clock (UTCTime)

-- Cabal
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Package
import Distribution.Text
import Distribution.Version

-- hackage-server
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.ModuleForest
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Types

-- This should provide the caller enough information to encode the package information
-- in its particular format (text, html, json) with minimal effort on its part.
-- This is why some fields of PackageDescription are preprocessed, and others aren't.
data PackageRender = PackageRender {
    rendPkgId        :: PackageIdentifier,
    rendDepends      :: [[Dependency]],
    rendExecNames    :: [String],
    rendLicenseName  :: String,
    rendMaintainer   :: Maybe String,
    rendCategory     :: [String],
    rendRepoHeads    :: [(RepoType, String, SourceRepo)],
    rendModules      :: Maybe ModuleForest,
    rendHasTarball   :: Bool,
    rendHasChangeLog :: Bool,
    rendUploadInfo   :: (UTCTime, Maybe UserInfo),
    rendPkgUri       :: String,
    rendFlags        :: [Flag],
    -- rendOther contains other useful fields which are merely strings, possibly empty
    --     for example: description, home page, copyright, author, stability
    -- If PackageRender is the One True Resource Representation, should they
    -- instead be fields of PackageRender?
    rendOther        :: PackageDescription
} deriving (Show)

doPackageRender :: Users.Users -> PkgInfo -> Bool -> IO PackageRender
doPackageRender users info hasChangeLog = return $ PackageRender
    { rendPkgId        = pkgInfoId info
    , rendDepends      = flatDependencies genDesc
    , rendExecNames    = map exeName (executables flatDesc)
    , rendLicenseName  = display (license desc) -- maybe make this a bit more human-readable
    , rendMaintainer   = case maintainer desc of
                           "None" -> Nothing
                           "none" -> Nothing
                           ""     -> Nothing
                           person -> Just person
    , rendCategory     = case category desc of
                           []  -> []
                           str -> categorySplit str
    , rendRepoHeads    = catMaybes (map rendRepo $ sourceRepos desc)
    , rendModules      = fmap (moduleForest . exposedModules) (library flatDesc)
    , rendHasTarball   = not . null $ pkgTarball info
    , rendHasChangeLog = hasChangeLog
    , rendUploadInfo   = let (utime, uid) = pkgUploadData info
                         in (utime, Users.lookupUserId uid users)
    , rendPkgUri       = pkgUri
    , rendFlags        = genPackageFlags genDesc
    , rendOther        = desc
    }
  where
    genDesc  = pkgDesc info
    flatDesc = flattenPackageDescription genDesc
    desc     = packageDescription genDesc
    pkgUri   = "/package/" ++ display (pkgInfoId info)

    rendRepo r = do
        guard $ repoKind r == RepoHead
        ty <- repoType r
        loc <- repoLocation r
        return (ty, loc, r)

{-------------------------------------------------------------------------------
  Util
-------------------------------------------------------------------------------}

categorySplit :: String -> [String]
categorySplit xs | all isSpace xs = []
categorySplit xs = map (dropWhile isSpace) $ splitOn ',' xs
  where
    splitOn x ys = front : case back of
                           [] -> []
                           (_:ys') -> splitOn x ys'
      where (front, back) = break (== x) ys

-----------------------------------------------------------------------
--
-- Flatten the dependencies of a GenericPackageDescription into
-- disjunctive normal form.
--
-- This could be in its own module.
--
flatDependencies :: GenericPackageDescription -> [[Dependency]]
flatDependencies pkg =
    map (map asDependency . sortOn (map toLower . display . fst)) $ sort $
    map get_deps $
    foldr reduceDisjunct [] $
    foldr intersectDisjunct head_deps $
        maybe id ((:) . fromCondTree) (condLibrary pkg) $
        map (fromCondTree . snd) (condExecutables pkg)
  where
    -- put the constrained ones first, for sorting purposes
    get_deps m = ranges ++ others
      where (others, ranges) = partition (simple . snd) (Map.toList m)

    asDependency (pkgname, interval) = Dependency pkgname $ intervalToRange interval

    intervalToRange :: VersionInterval -> VersionRange
    intervalToRange (lower, upper) = simplifyVersionRange $
        intersectVersionRanges (rangeLower lower) (rangeUpper upper)
      where rangeLower (LowerBound v bound) = case bound of
                InclusiveBound -> orLaterVersion v
                ExclusiveBound -> laterVersion v
            rangeUpper (UpperBound v bound) = case bound of
                InclusiveBound -> orEarlierVersion v
                ExclusiveBound -> earlierVersion v
            rangeUpper NoUpperBound = anyVersion

    simple :: VersionInterval -> Bool
    simple (l, NoUpperBound) = isMinLowerBound l
    simple _ = False

    head_deps = fromDependencies (buildDepends (packageDescription pkg))

    fromDependencies :: [Dependency] -> Disjunct
    fromDependencies = foldr addDep unitDisjunct
      where addDep (Dependency p vr) = liftM2 (\ x y -> Map.alter (add x) p y)
                (asVersionIntervals vr)
            add x Nothing = Just x
            add x (Just y) = intersectInterval x y

    fromCondTree :: CondTree v [Dependency] a -> Disjunct
    fromCondTree (CondNode _ ds comps) =
        foldr intersectDisjunct (fromDependencies ds) $
            map fromComponent comps

    fromComponent (_, _, Nothing) = unitDisjunct
    fromComponent (_, then_part, Just else_part) =
        unionDisjunct (fromCondTree then_part)
            (fromCondTree else_part)

    reduceDisjunct :: Conjunct -> [Conjunct] -> [Conjunct]
    reduceDisjunct c cs
      | any (c `subConjunct`) cs = cs
      | otherwise = c : filter (not . (`subConjunct` c)) cs

-- Same as @sortBy (comparing f)@, but without recomputing @f@.
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f xs = map snd (sortBy (comparing fst) [(f x, x) | x <- xs])

isMinLowerBound :: LowerBound -> Bool
isMinLowerBound (LowerBound (Version [0] _) InclusiveBound) = True
isMinLowerBound _ = False

{-
isWildcardRange :: Version -> Version -> Bool
isWildcardRange (Version branch1 _) (Version branch2 _) = check branch1 branch2
  where check (n:[]) (m:[]) | n+1 == m = True
        check (n:ns) (m:ms) | n   == m = check ns ms
        check _      _                 = False

withinInterval :: Version -> VersionInterval -> Bool
withinInterval v (lowerBound, upperBound)    = withinLower lowerBound
                                              && withinUpper upperBound
  where
    withinLower (LowerBound v' ExclusiveBound) = v' <  v
    withinLower (LowerBound v' InclusiveBound) = v' <= v

    withinUpper NoUpperBound                   = True
    withinUpper (UpperBound v' ExclusiveBound) = v' >  v
    withinUpper (UpperBound v' InclusiveBound) = v' >= v
-}

intersectInterval :: VersionInterval -> VersionInterval -> Maybe VersionInterval
intersectInterval (l1, u1) (l2, u2)
  | below u1 l2 || below u2 l1 = Nothing
  | otherwise = Just (max l1 l2, min u1 u2)

below :: UpperBound -> LowerBound -> Bool
below NoUpperBound _ = False
below (UpperBound v1 InclusiveBound) (LowerBound v2 InclusiveBound) = v1 < v2
below (UpperBound v1 _) (LowerBound v2 _) = v1 <= v2

subInterval :: VersionInterval -> VersionInterval -> Bool
subInterval (l1, u1) (l2, u2) = l2 <= l1 && u1 <= u2

-- Constraints in disjunctive normal form

type Conjunct = Map PackageName VersionInterval

unitConjunct :: Conjunct
unitConjunct = Map.empty

intersectConjunct :: Conjunct -> Conjunct -> Maybe Conjunct
intersectConjunct m1 m2 =
    Traversable.sequence $
        Map.unionWith inters (fmap Just m1) (fmap Just m2)
  where
    inters mx my = do
      x <- mx
      y <- my
      intersectInterval x y

subConjunct :: Conjunct -> Conjunct -> Bool
subConjunct m1 m2 =
    Map.null (Map.difference m2 m1) &&
    Foldable.and (Map.intersectionWith subInterval m1 m2)

type Disjunct = [Conjunct]

unitDisjunct :: Disjunct
unitDisjunct = [unitConjunct]

intersectDisjunct :: Disjunct -> Disjunct -> Disjunct
intersectDisjunct xs ys = catMaybes (liftM2 intersectConjunct xs ys)

-- eliminate any Conjunct list that is more restrictive than another
unionDisjunct :: Disjunct -> Disjunct -> Disjunct
unionDisjunct xs ys = xs' ++ ys'
  where ys' = [y | y <- ys, not (or [subConjunct y x | x <- xs])]
        xs' = [x | x <- xs, not (or [subConjunct x y | y <- ys'])]
