{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

module Distribution.Server.Features.PreferredVersions.State where

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import Distribution.Package
import Distribution.Version

import Data.Acid  (Query, Update, makeAcidic)
import Data.Maybe (fromMaybe)
import Control.Monad.State (put, modify)
import Control.Monad.Reader (ask, asks)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.SafeCopy (Migrate(..), base, extension, deriveSafeCopy)

data PreferredVersions = PreferredVersions {
    preferredMap  :: Map PackageName PreferredInfo,
    deprecatedMap :: Map PackageName [PackageName],
    migratedEphemeralPrefs :: Bool
} deriving (Show, Eq)

data PreferredInfo = PreferredInfo {
    preferredRanges :: [VersionRange],
    deprecatedVersions :: [Version],
    sumRange :: Maybe VersionRange -- cached form of 'consolidateRanges' below
} deriving (Show, Eq)

emptyPreferredInfo :: PreferredInfo
emptyPreferredInfo = PreferredInfo [] [] Nothing

consolidateRanges :: [VersionRange] -> [Version] -> Maybe VersionRange
consolidateRanges ranges depr =
    let range = simplifyVersionRange $ foldr intersectVersionRanges anyVersion (map notThisVersion depr ++ ranges)
    in if isAnyVersion range || isNoVersion range
        then Nothing
        else Just range

data VersionStatus = NormalVersion | DeprecatedVersion | UnpreferredVersion deriving (Show, Eq, Ord, Enum)


data PreferredVersions_v0
   = PreferredVersions_v0 (Map PackageName PreferredInfo)
                          (Map PackageName [PackageName])

$(deriveSafeCopy 0 'base ''PreferredInfo)
$(deriveSafeCopy 0 'base ''PreferredVersions_v0)

instance Migrate PreferredVersions where
    type MigrateFrom PreferredVersions = PreferredVersions_v0
    migrate (PreferredVersions_v0 prefs deprs) =
      PreferredVersions {
        preferredMap  = prefs,
        deprecatedMap = deprs,
        migratedEphemeralPrefs = False
      }

------------------------------------------
$(deriveSafeCopy 1 'extension ''PreferredVersions)

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


