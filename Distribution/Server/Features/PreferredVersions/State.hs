{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

module Distribution.Server.Features.PreferredVersions.State where

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import Distribution.Package
import Distribution.Version

import Data.Acid  (Query, Update, makeAcidic)
import Data.Maybe (isJust, fromMaybe)
import Data.Typeable (Typeable)
import Control.Arrow (second)
import Control.Monad (ap)
import Control.Monad.State (put, modify)
import Control.Monad.Reader (ask, asks)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.SafeCopy (Migrate(..), base, extension, deriveSafeCopy)
import Data.Set (Set)
import qualified Data.Set as Set

data PreferredVersions = PreferredVersions {
    preferredMap  :: Map PackageName PreferredInfo,
    deprecatedMap :: Map PackageName [PackageName],
    migratedEphemeralPrefs :: Bool
} deriving (Typeable, Show, Eq)

data PreferredInfo = PreferredInfo {
    preferredRanges :: [VersionRange],
    deprecatedVersions :: [Version],
    sumRange :: Maybe VersionRange -- cached form of 'consolidateRanges' below
} deriving (Typeable, Show, Eq)

emptyPreferredInfo :: PreferredInfo
emptyPreferredInfo = PreferredInfo [] [] Nothing

consolidateRanges :: [VersionRange] -> [Version] -> Maybe VersionRange
consolidateRanges ranges depr =
    let range = simplifyVersionRange $ foldr intersectVersionRanges anyVersion (map notThisVersion depr ++ ranges)
    in if isAnyVersion range || isNoVersion range
        then Nothing
        else Just range

data VersionStatus = NormalVersion | DeprecatedVersion | UnpreferredVersion deriving (Show, Typeable, Eq, Ord, Enum)

getVersionStatus :: PreferredInfo -> Version -> VersionStatus
getVersionStatus info version = case version `elem` deprecatedVersions info of
    True  -> DeprecatedVersion
    False -> case maybe True (withinRange version) (sumRange info) of
        True  -> NormalVersion
        False -> UnpreferredVersion

classifyVersions :: PreferredInfo -> [Version] -> [(Version, VersionStatus)]
classifyVersions (PreferredInfo [] [] _) = map (flip (,) NormalVersion)
classifyVersions info = map ((,) `ap` getVersionStatus info)

partitionVersions :: PreferredInfo -> [Version] -> ([Version], [Version], [Version])
partitionVersions info versions = if (not . isJust $ sumRange info) then (versions, [], []) else go versions
  where go :: [Version] -> ([Version], [Version], [Version]) -- foldr-type approach
        go (v:vs) = let ~(norm, depr, unpref) = go vs in case getVersionStatus info v of
            NormalVersion -> (v:norm, depr, unpref)
            DeprecatedVersion -> (norm, v:depr, unpref)
            UnpreferredVersion -> (norm, depr, v:unpref)
        go [] = ([], [], [])
------------------------------------------
$(deriveSafeCopy 1 'extension ''PreferredVersions)
$(deriveSafeCopy 0 'base ''PreferredInfo)
$(deriveSafeCopy 0 'base ''VersionStatus)

instance MemSize PreferredVersions where
    memSize (PreferredVersions a b c) = memSize3 a b c

instance MemSize PreferredInfo where
    memSize (PreferredInfo a b c) = memSize3 a b c

-- | Initial PreferredVersions
--
-- NOTE: If we are starting from a fresh DB, obviously migration is not needed.
-- However, if we are not, but we _are_ starting from an initial DB value, this
-- must mean we are starting a server with an existing DB but no checkpoint. In
-- this case we might have old transactions to replay, so we might have to
-- migrate.
--
-- If we failed to migrate these old transactions, we would end up with a
-- 'packageUpdateLog' without entries for @preferred-versions@.
initialPreferredVersions :: Bool -> PreferredVersions
initialPreferredVersions freshDB = PreferredVersions {
    preferredMap           = Map.empty
  , deprecatedMap          = Map.empty
  , migratedEphemeralPrefs = freshDB
  }

setPreferredInfo :: PackageName -> [VersionRange] -> [Version]
                                  -> Update PreferredVersions PreferredInfo
setPreferredInfo name ranges versions = do
    let prefinfo =  PreferredInfo {
          preferredRanges    = ranges,
          deprecatedVersions = versions,
          sumRange           = consolidateRanges ranges versions
        }
    if null ranges && null versions
      then modify $ \p -> p {
             preferredMap = Map.delete name (preferredMap p)
           }
      else modify $ \p -> p {
             preferredMap = Map.insert name prefinfo (preferredMap p)
           }
    return prefinfo

getPreferredInfo :: PackageName -> Query PreferredVersions PreferredInfo
getPreferredInfo name = asks $ Map.findWithDefault emptyPreferredInfo name . preferredMap

setDeprecatedFor :: PackageName -> Maybe [PackageName] -> Update PreferredVersions ()
setDeprecatedFor name forName = modify $ \p -> p { deprecatedMap = Map.alter (const forName) name $ deprecatedMap p }

getDeprecatedFor :: PackageName -> Query PreferredVersions (Maybe [PackageName])
getDeprecatedFor name = asks $ Map.lookup name . deprecatedMap

isDeprecated :: PackageName -> Query PreferredVersions Bool
isDeprecated name = asks $ Map.member name . deprecatedMap

getPreferredVersions :: Query PreferredVersions PreferredVersions
getPreferredVersions = ask

replacePreferredVersions :: PreferredVersions -> Update PreferredVersions ()
replacePreferredVersions = put

setMigratedEphemeralPrefs :: Update PreferredVersions ()
setMigratedEphemeralPrefs = modify $ \p -> p { migratedEphemeralPrefs = True }

---------------
-- old, for old acid-state logs only
--

setPreferredRanges :: PackageName -> [VersionRange] -> Update PreferredVersions ()
setPreferredRanges name ranges =
    alterPreferredInfo name $ \p -> p { preferredRanges = ranges }

setDeprecatedVersions :: PackageName -> [Version] -> Update PreferredVersions ()
setDeprecatedVersions name versions =
    alterPreferredInfo name $ \p -> p { deprecatedVersions = versions }

alterPreferredInfo :: PackageName -> (PreferredInfo -> PreferredInfo)
                   -> Update PreferredVersions ()
alterPreferredInfo name func =
    modify $ \p -> p {
      preferredMap = Map.alter (res . func . fromMaybe emptyPreferredInfo)
                               name (preferredMap p)
    }
  where res (PreferredInfo [] [] _)       = Nothing -- ie delete
        res (PreferredInfo ranges depr _) =
          Just (PreferredInfo ranges depr (consolidateRanges ranges depr))


makeAcidic ''PreferredVersions ['setPreferredInfo
                               ,'setPreferredRanges
                               ,'setDeprecatedVersions
                               ,'getPreferredInfo
                               ,'setDeprecatedFor
                               ,'getDeprecatedFor
                               ,'isDeprecated
                               ,'getPreferredVersions
                               ,'replacePreferredVersions
                               ,'setMigratedEphemeralPrefs
                               ]


data PreferredVersions_v0
   = PreferredVersions_v0 (Map PackageName PreferredInfo)
                          (Map PackageName [PackageName])

deriveSafeCopy 0 'base ''PreferredVersions_v0

instance Migrate PreferredVersions where
    type MigrateFrom PreferredVersions = PreferredVersions_v0
    migrate (PreferredVersions_v0 prefs deprs) =
      PreferredVersions {
        preferredMap  = prefs,
        deprecatedMap = deprs,
        migratedEphemeralPrefs = False
      }

---------------
maybeBestVersion :: PreferredInfo -> [Version] -> Set Version -> Maybe (Version, Maybe VersionStatus)
maybeBestVersion info allVersions versions = if null allVersions || Set.null versions then Nothing else Just $ findBestVersion info allVersions versions

{-
findBestVersion attempts to find the best version to display out of a set
of versions. The quality of a given version is encoded in a pair (VersionStatus,
Bool). If the version is a NormalVersion, then the boolean indicates whether if
it the most recently uploaded preferred version (and all higher versions are
either deprecated or unpreferred). Otherwise, if it  is a DeprecatedVersion or
UnpreferredVersion, the boolean indicates that it is the maximum of all uploaded
versions.

The list of available versions is scanned from the back (most recent) to the
front (first one uploaded). If a 'better' version is found than the current
best version, it is replaced. If no better version can be found, the algorithm
finishes up. The exact ordering is defined as:

1. (NormalVersion, True) means the latest preferred version of the package is
available. This option may appear anywhere, although it is always seen before
(NormalVersion, False). In this case, the algorithm finishes up.

2. (UnpreferredVersion, True) means the latest available version of the package
is not preferred, but the latest preferred version is not available. If this
option appears anywhere, it will be the most recent version in the set,
excluding deprecated versions.

3. (NormalVersion, False) means neither the actual latest version nor the
preferred latest version are available, but there is some preferred version
that's available. It can only be scanned after (NormalVersion, True) and
(UnpreferredVersion, True), so the algorithm finishes up in this case.
4. (UnpreferredVersion, False) means no preferred versions are available, and
only an older version is available. It is still possible to see a NormalVersion
after this option, so the algorithm continues.

5. (DeprecatedVersion, True) and (DeprecatedVersion, False) mean only a
deprecated version is available. This is not so great.

This is a bit complex but I think it has the most intuitive result, and is
rather efficient in 99% of cases.

The version set and version list should both be non-empty; otherwise this
function is partial. Use maybeBestVersion for a safe check.

-}
findBestVersion :: PreferredInfo -> [Version] -> Set Version -> (Version, Maybe VersionStatus)
findBestVersion info allVersions versions =
    let topStatus = getVersionStatus info maxVersion
    in if maxAllVersion == maxVersion && topStatus == NormalVersion
        then (maxVersion, Just NormalVersion) -- most common case
        else second classifyOpt $ newSearch (reverse $ Set.toList versions) (maxVersion, (topStatus, True))
  where
    maxVersion = Set.findMax versions
    maxAllVersion = last allVersions

    newestPreferred = case filter ((==NormalVersion) . (infoMap Map.!)) $ allVersions of
        []    -> Nothing
        prefs -> Just $ last prefs

    infoMap = Map.fromDistinctAscList $ classifyVersions info allVersions

    newSearch (v:vs) _ = case infoMap Map.! v of
        NormalVersion | v == maxAllVersion -> (v, (NormalVersion, True))
        NormalVersion -> oldSearch vs (v, (NormalVersion, False))
        DeprecatedVersion -> newSearch vs (v, (DeprecatedVersion, True))
        UnpreferredVersion -> oldSearch vs (v, (UnpreferredVersion, True))
    newSearch [] opt = opt

    oldSearch (v:vs) opt = case infoMap Map.! v of
        NormalVersion -> replaceBetter opt (v, (NormalVersion, newestPreferred == Just v))
        other -> oldSearch vs $ replaceBetter opt (v, (other, False))
    oldSearch [] opt = opt

    replaceBetter keep@(_, old) replace@(_, new) = if optionPrefs new > optionPrefs old then replace else keep

    optionPrefs :: (VersionStatus, Bool) -> Int
    optionPrefs opt = case opt of
        (NormalVersion, True) -> 4
        (UnpreferredVersion, True) -> 3
        (NormalVersion, False) -> 2
        (UnpreferredVersion, False) -> 1
        _ -> 0

    classifyOpt opt = case opt of
        (NormalVersion, True) -> Just NormalVersion
        (UnpreferredVersion, True) -> Just UnpreferredVersion
        (DeprecatedVersion, _) -> Just DeprecatedVersion
        _ -> Nothing
