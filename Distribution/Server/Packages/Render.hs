-- TODO: Review and possibly move elsewhere. This code was part of the
-- RecentPackages (formerly "Check") feature, but that caused some cyclic
-- dependencies.
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Distribution.Server.Packages.Render (
    -- * Package render
    PackageRender(..)
  , DependencyTree
  , IsBuildable (..)
  , doPackageRender

    -- * Utils
  , categorySplit,
  ) where

import Data.Maybe (catMaybes, isJust, maybeToList)
import Control.Monad (guard, mzero)
import Control.Arrow (second)
import Data.Aeson ((.=),(.:),(.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Char (toLower, isSpace)
import qualified Data.Map as Map
import qualified Data.Vector as Vec
import Data.Ord (comparing)
import Data.List (sortBy, intercalate)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import System.FilePath.Posix ((</>), (<.>))
import Text.Read             (readMaybe)

-- Cabal
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.Package
import Distribution.Text
import Distribution.Version
import Distribution.ModuleName as ModuleName

-- hackage-server
import Distribution.Server.Framework.CacheControl (ETag)
import Distribution.Server.Packages.Types
import Distribution.Server.Packages.ModuleForest
import qualified Distribution.Server.Users.Users as Users
import Distribution.Server.Users.Types
import qualified Data.TarIndex as TarIndex
import Data.TarIndex (TarIndex, TarEntryOffset)

-- This should provide the caller enough information to encode the package information
-- in its particular format (text, html, json) with minimal effort on its part.
-- This is why some fields of PackageDescription are preprocessed, and others aren't.
data PackageRender = PackageRender {
    rendPkgId        :: PackageIdentifier,
    rendDepends      :: [Dependency],
    rendExecNames    :: [String],
    rendLibraryDeps  :: Maybe DependencyTree,
    rendExecutableDeps :: [(String, DependencyTree)],
    rendLicenseName  :: String,
    rendLicenseFiles :: [FilePath],
    rendMaintainer   :: Maybe String,
    rendCategory     :: [String],
    rendRepoHeads    :: [(RepoType, String, SourceRepo)],
    rendModules      :: Maybe TarIndex -> Maybe ModuleForest,
    rendHasTarball   :: Bool,
    rendChangeLog    :: Maybe (FilePath, ETag, TarEntryOffset, FilePath),
    rendReadme       :: Maybe (FilePath, ETag, TarEntryOffset, FilePath),
    rendUploadInfo   :: (UTCTime, Maybe UserInfo),
    rendUpdateInfo   :: Maybe (Int, UTCTime, Maybe UserInfo),
    rendPkgUri       :: String,
    rendFlags        :: [Flag],
    -- rendOther contains other useful fields which are merely strings, possibly empty
    --     for example: description, home page, copyright, author, stability
    -- If PackageRender is the One True Resource Representation, should they
    -- instead be fields of PackageRender?
    rendOther        :: PackageDescription
} deriving (Show)

instance A.ToJSON PackageRender where
  toJSON p = A.object [
      "pkgId"          .= toJSONPackageIdentifier (rendPkgId p)
    , "depends"        .= map toJSONDependency (rendDepends p)
    , "execNames"      .= (rendExecNames p)
    , "libraryDeps"    .= fmap toJSONDependencyTree (rendLibraryDeps p)
    , "executableDeps" .= map (\(str,dep) -> [A.String (T.pack str), toJSONDependencyTree dep]) (rendExecutableDeps p)
    , "licenseName"    .= rendLicenseName p
    , "licenseFiles"   .= rendLicenseFiles p
    , "maintainer"     .= rendMaintainer p
    , "category"       .= rendCategory p
    , "repoHeads"      .= map toJSONRepoTuple (rendRepoHeads p)
    , "modules"        .= show ((rendModules p) Nothing) -- TODO probably wrong?
    , "hasTarball"     .= rendHasTarball p
    , "changeLog"      .= show (rendChangeLog p)
    , "readme"         .= show (rendReadme p)
    , "uploadInfo"     .= show (rendUploadInfo p)
    , "updateInfo"     .= show (rendUpdateInfo p)
    , "pkgUri"         .= rendPkgUri p
    , "flags"          .= map show (rendFlags p)
    , "other"          .= show (rendOther p)
    ]

doPackageRender :: Users.Users -> PkgInfo -> PackageRender
doPackageRender users info = PackageRender
    { rendPkgId        = pkgInfoId info
    , rendDepends      = flatDependencies genDesc
    , rendExecNames    = map exeName (executables flatDesc)
    , rendLibraryDeps  = depTree libBuildInfo `fmap` condLibrary genDesc
    , rendExecutableDeps = second (depTree buildInfo) `map` condExecutables genDesc
    , rendLicenseName  = display (license desc) -- maybe make this a bit more human-readable
    , rendLicenseFiles = licenseFiles desc
    , rendMaintainer   = case maintainer desc of
                           "None" -> Nothing
                           "none" -> Nothing
                           ""     -> Nothing
                           person -> Just person
    , rendCategory     = case category desc of
                           []  -> []
                           str -> categorySplit str
    , rendRepoHeads    = catMaybes (map rendRepo $ sourceRepos desc)
    , rendModules      = \docindex ->
                             fmap (moduleForest
                           . map (\m -> (m, moduleHasDocs docindex m))
                           . exposedModules)
                          (library flatDesc)
    , rendHasTarball   = not . Vec.null $ pkgTarballRevisions info
    , rendChangeLog    = Nothing -- populated later
    , rendReadme       = Nothing -- populated later
    , rendUploadInfo   = let (utime, uid) = pkgOriginalUploadInfo info
                         in (utime, Users.lookupUserId uid users)
    , rendUpdateInfo   = let maxrevision  = Vec.length (pkgMetadataRevisions info) - 1
                             (utime, uid) = pkgLatestUploadInfo info
                             uinfo        = Users.lookupUserId uid users
                         in if maxrevision > 0
                              then Just (maxrevision, utime, uinfo)
                              else Nothing
    , rendPkgUri       = pkgUri
    , rendFlags        = genPackageFlags genDesc
    , rendOther        = desc
    }
  where
    genDesc  = pkgDesc info
    flatDesc = flattenPackageDescription genDesc
    desc     = packageDescription genDesc
    pkgUri   = "/package/" ++ display (pkgInfoId info)

    depTree :: (a -> BuildInfo) -> CondTree ConfVar [Dependency] a -> DependencyTree
    depTree getBuildInfo = mapTreeData isBuildable . mapTreeConstrs simplifyDeps
      where
        simplifyDeps = sortDeps . combineDepsBy intersectVersionIntervals
        isBuildable ctData = if buildable $ getBuildInfo ctData
                               then Buildable
                               else NotBuildable

    moduleHasDocs :: Maybe TarIndex -> ModuleName -> Bool
    moduleHasDocs Nothing       = const False
    moduleHasDocs (Just doctar) = isJust . TarIndex.lookup doctar
                                         . moduleDocTarPath (packageId genDesc)

    moduleDocTarPath :: PackageId -> ModuleName -> FilePath
    moduleDocTarPath pkgid modname =
      display pkgid ++ "-docs" </>
      intercalate "-" (ModuleName.components modname) <.> "html"

    rendRepo r = do
        guard $ repoKind r == RepoHead
        ty <- repoType r
        loc <- repoLocation r
        return (ty, loc, r)

type DependencyTree = CondTree ConfVar [Dependency] IsBuildable

data IsBuildable = Buildable
                 | NotBuildable
                   deriving (Eq, Show, Read)

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
-- Flatten the dependencies of a GenericPackageDescription into a
-- simple summary form. Library and executable dependency ranges
-- are combined using intersection, except for dependencies within
-- if and else branches, which are unioned together.
--
flatDependencies :: GenericPackageDescription -> [Dependency]
flatDependencies =
      sortOn (\(Dependency pkgname _) -> map toLower (display pkgname))
    . pkgDeps
  where
    pkgDeps :: GenericPackageDescription -> [Dependency]
    pkgDeps pkg = fromMap $ Map.unionsWith intersectVersions $
                      map condTreeDeps (maybeToList $ condLibrary pkg)
                   ++ map (condTreeDeps . snd) (condExecutables pkg)
      where
        fromMap = map fromPair . Map.toList
        fromPair (pkgname, Versions _ ver) =
            Dependency pkgname $ fromVersionIntervals ver

    condTreeDeps :: CondTree v [Dependency] a -> PackageVersions
    condTreeDeps (CondNode _ ds comps) =
        Map.unionsWith intersectVersions $
          toMap ds : map fromComponent comps
      where
        fromComponent (_, then_part, else_part) =
            unionDeps (condTreeDeps then_part)
                      (maybe Map.empty condTreeDeps else_part)
        toMap = Map.fromListWith intersectVersions . map toPair
        toPair (Dependency pkgname ver) =
            (pkgname, Versions All $ toVersionIntervals ver)

    unionDeps :: PackageVersions -> PackageVersions -> PackageVersions
    unionDeps ds1 ds2 = Map.unionWith unionVersions
                        (Map.union ds1 defaults) (Map.union ds2 defaults)
      where
        defaults = Map.map (const notSpecified) $ Map.union ds1 ds2
        notSpecified = Versions Some $ toVersionIntervals noVersion

-- | Version intervals for a dependency that also indicate whether the
-- dependency has been specified on all branches. For example, package x's
-- version intervals use 'All' while package y's version intervals use
-- 'Some':
--
-- > if flag(f)
-- >   build-depends: x < 1, y < 1
-- > else
-- >   build-depends: x >= 1
--
-- This distinction affects the intersection of intervals.
data Versions = Versions Branches VersionIntervals

data Branches = All | Some deriving Eq

type PackageVersions = Map.Map PackageName Versions

unionVersions :: Versions -> Versions -> Versions
unionVersions (Versions b1 v1) (Versions b2 v2) =
    let b3 = if b1 == Some || b2 == Some
                then Some
                else All
    in Versions b3 $ unionVersionIntervals v1 v2

intersectVersions :: Versions -> Versions -> Versions
intersectVersions (Versions Some v1) (Versions Some v2) =
    Versions Some $ unionVersionIntervals v1 v2
intersectVersions (Versions Some _) v@(Versions All _) = v
intersectVersions v@(Versions All _) (Versions Some _) = v
intersectVersions (Versions All v1) (Versions All v2) =
    Versions All $ intersectVersionIntervals v1 v2

sortDeps :: [Dependency] -> [Dependency]
sortDeps = sortOn $ \(Dependency pkgname _) -> map toLower (display pkgname)

combineDepsBy :: (VersionIntervals -> VersionIntervals -> VersionIntervals)
              -> [Dependency] -> [Dependency]
combineDepsBy f =
    map (\(pkgname, ver) -> Dependency pkgname (fromVersionIntervals ver))
  . Map.toList
  . Map.fromListWith f
  . map (\(Dependency pkgname ver) -> (pkgname, toVersionIntervals ver))

-- Same as @sortBy (comparing f)@, but without recomputing @f@.
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f xs = map snd (sortBy (comparing fst) [(f x, x) | x <- xs])

-- Orphan Aeson 'instances' for Cabal's `PackageIdentifier`
toJSONPackageIdentifier :: PackageIdentifier -> A.Value
toJSONPackageIdentifier (PackageIdentifier n v) =
  A.object ["name" .= unPackageName n
           ,"version" .= toJSONVersion v
           ]

fromJSONPackageIdentifier :: A.Value -> A.Parser PackageIdentifier
fromJSONPackageIdentifier (A.Object v) =
  PackageIdentifier
  <$> (PackageName <$> v .: "name")
  <*> (fromJSONVersion =<< v .: "version")
fromJSONPackageIdentifier _ = mzero

-- Orphan Aeson 'instances' for Cabal's `Version`
toJSONVersion :: Version -> A.Value
toJSONVersion (Version brnch tgs) =
  A.object ["branch" .= brnch
           ,"tags"   .= tgs
           ]

fromJSONVersion :: A.Value -> A.Parser Version
fromJSONVersion (A.Object v) = Version <$> v .: "branch" <*> v .: "tags"
fromJSONVersion _          = mzero

toJSONDependency :: Dependency -> A.Value
toJSONDependency (Dependency pName vRange) =
  A.object ["name"         .= unPackageName pName
           ,"versionrange" .= show          vRange
           ]

fromJSONDependency :: A.Value -> A.Parser Dependency
fromJSONDependency (A.Object v) = do
  nm     <- PackageName <$> v .: "name"
  mRange <- v .: "versionrange"
  case readMaybe mRange of
    Nothing -> mzero
    Just r  -> return (Dependency nm r)
fromJSONDependency _ = mzero

toJSONRepoTuple :: (RepoType,String,SourceRepo) -> A.Value
toJSONRepoTuple (rt,s,sr) =
  A.object [ "repotype"   .= show rt
           , "s"          .= s
           , "sourcerepo" .= toJSONSourceRepo sr
           ]

fromJSONRepoTuple :: A.Value -> A.Parser (RepoType, String, SourceRepo)
fromJSONRepoTuple (A.Object v) = do
  mRT <- readMaybe <$> v .: "repotype"
  s   <- v .: "s"
  sr  <- fromJSONSourceRepo =<< v .: "sourcerepo"
  case mRT of
    Nothing -> mzero
    Just rt -> return (rt,s,sr)

toJSONSourceRepo :: SourceRepo -> A.Value
toJSONSourceRepo (SourceRepo rk mRT mLoc mMod mBranch mTag mSubdir) =
    A.object .catMaybes $
    [ Just ("repokind" .= show rk)
    , (("repotype" .=) . show) <$> mRT
    , ("repolocation" .=)      <$> mLoc
    , ("repomodule" .=)        <$> mMod
    , ("repobranch" .=)        <$> mBranch
    , ("repotag" .=)           <$> mTag
    , ("reposubdir" .=)        <$> mSubdir
    ]

fromJSONSourceRepo :: A.Value -> A.Parser SourceRepo
fromJSONSourceRepo (A.Object v) =
  SourceRepo
  <$> (read <$> v .:  "repokind")
  <*> ((maybe Nothing read) <$> v .:? "repotype")
  <*> (v .:? "repolocation")
  <*> (v .:? "repomodule")
  <*> (v .:? "repobranch")
  <*> (v .:? "repotag")
  <*> (v .:? "reposubdir")

toJSONCondTree :: (Condition v -> A.Value)
               -> (c -> A.Value)
               -> (a -> A.Value)
               -> CondTree v c a
               -> A.Value
toJSONCondTree sV sC sA (CondNode a c comps) =
  A.object [ "data" .= sA a
           , "constraints" .= sC c
           , "components"  .= map toJSONComponents comps
           ]
 where toJSONComponents (cond, subTree, mSubTree) =
         let mSubPart = case mSubTree of
                          Nothing -> []
                          Just t  -> [toJSONCondTree sV sC sA t]
         in [sV cond, toJSONCondTree sV sC sA subTree] ++  mSubPart
        --  in A.Array (Vec.fromList $ [sV cond, toJSONCondTree sV sC sA subTree]
        --                           ++ mSubPart)

fromJSONCondTree :: (A.Value -> A.Parser (Condition v))
                 -> (A.Value -> A.Parser c)
                 -> (A.Value -> A.Parser a)
                 -> A.Value
                 -> A.Parser (CondTree v c a)
fromJSONCondTree toV toC toA (A.Object v) = do
  dataPart <- v .: "data"
  consrPart <- v .: "constraints"
  compsPart <- v .: "components"
  case compsPart of
    A.Array cs -> do
      comps <- mapM toTuple (Vec.toList cs)
      CondNode <$> (toA dataPart) <*> (toC consrPart) <*> pure (comps)
  -- CondNode
  -- <$> toA     =<< v .: "data"
  -- <*> toC     =<< v .: "constraints"
  -- <*> (mapM toTuple =<< v .: "components")
  where --toTuple
        --  :: A.Value
        --  -> A.Parser (Condition v, CondTree v c a, Maybe (CondTree v c a))
        toTuple (A.Array arr) = case Vec.toList arr of
          [valCond, valSubTree, valMSubTree] ->
            (,,)
            <$> toV valCond
            <*> fromJSONCondTree      toV toC toA valSubTree
            <*> fromJSONMaybeCondTree toV toC toA valMSubTree
          [valCond, valSubTree] ->
            (,,)
            <$> toV valCond
            <*> fromJSONCondTree toV toC toA valSubTree
            <*> pure Nothing
          _ -> mzero

fromJSONMaybeCondTree :: (A.Value -> A.Parser (Condition v))
                      -> (A.Value -> A.Parser c)
                      -> (A.Value -> A.Parser a)
                      -> A.Value
                      -> A.Parser (Maybe (CondTree v c a))
fromJSONMaybeCondTree toV toC toA v = do
  parsedV <- A.parseJSON v :: A.Parser A.Value
  parsedT <- fromJSONCondTree toV toC toA v -- :: A.Parser (CondTree v c a)
  case parsedV of
    A.Null     -> return Nothing
    A.Object _ -> Just <$> return parsedT

toJSONDependencyTree :: DependencyTree -> A.Value
toJSONDependencyTree cTree =
  toJSONCondTree toJSONConditionConfVar
                 (\v -> A.Array . Vec.fromList . map toJSONDependency $ v)
                 toJSONIsBuildable
                 cTree

toJSONConditionConfVar :: Condition ConfVar -> A.Value
toJSONConditionConfVar x = case x of
  Var c -> A.object ["tag" .= ("Var" :: String)
                    ,"value" .= toJSONConfVar c]
  Lit b -> A.object ["tag" .= ("Lit" :: String)
                    ,"value" .= b]
  CNot c -> A.object ["tag" .= ("CNot" :: String)
                     ,"value" .= toJSONConditionConfVar c]
  COr a b -> A.object ["tag" .= ("COr" :: String)
                      ,"value" .= [toJSONConditionConfVar a
                                  ,toJSONConditionConfVar b]
                      ]
  CAnd a b -> A.object ["tag" .= ("CAnd" :: String)
                       ,"value" .= [toJSONConditionConfVar a
                                   ,toJSONConditionConfVar b]
                       ]

fromJSONConditionConfVar :: A.Value -> A.Parser (Condition ConfVar)
fromJSONConditionConfVar (A.Object v) = do
  t <- v .: "tag"
  case (t :: String) of
    --"Var" -> Var <$> fromJSONConfVar =<< v .: "value"
    "Lit" -> Lit <$> v .: "value"
    -- "CNot" -> CNot <$> fromJSONConfVar =<< v .: "value"
    -- TODO finish
    -- "COr"  -> do
    --    xs <- v .: "value"
    --    case xs of
    --      A.Array vs ->
    -- (fromJSONConfVar =<< V.:)
    --         "Var" -> Var <$> fromJSONConfVar =<< v .: "value"

fromJSONDependencyTree :: A.Value -> A.Parser DependencyTree
fromJSONDependencyTree v =
  fromJSONCondTree fromJSONConditionConfVar fromJSONDependencyList fromJSONIsBuildable v

fromJSONDependencyList :: A.Value -> A.Parser [Dependency]
fromJSONDependencyList (A.Array xs) = mapM fromJSONDependency . Vec.toList $ xs

toJSONConfVar :: ConfVar -> A.Value
toJSONConfVar (OS o) = A.object ["tag" .= ("OS" :: String), "value" .= show o]
toJSONConfVar (Arch a) = A.object ["tag" .= ("Arch" :: String), "value" .= show a]
toJSONConfVar (Flag f) = A.object ["tag" .= ("Flag" :: String), "value" .= show f]
toJSONConfVar (Impl cf vr) =
  A.object ["tag" .= ("Impl" :: String)
           , "value" .= A.object ["compilerflavor" .= show cf
                                 ,"versionrange"   .= show vr]
           ]

fromJSONConfVar :: A.Value -> A.Parser ConfVar
fromJSONConfVar (A.Object v) = do
  t <- v .: "tag"
  case (t :: String) of
    "OS"   -> OS   <$> (read <$> v .: "value")
    "Arch" -> Arch <$> (read <$> v .: "value")
    "Flag" -> Flag <$> (read <$> v .: "value")
    "Impl" -> Impl <$> (read <$> v .: "compilerflavor") <*> (read <$> v .: "versionrange")


toJSONIsBuildable :: IsBuildable -> A.Value
toJSONIsBuildable = A.String . T.pack . show

fromJSONIsBuildable :: A.Value -> A.Parser IsBuildable
fromJSONIsBuildable (A.String s) = maybe mzero return (readMaybe $ T.unpack s)
fromJSONIsBuildable _          = mzero
