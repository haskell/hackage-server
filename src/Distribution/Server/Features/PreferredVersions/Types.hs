{-# LANGUAGE TemplateHaskell #-}

module Distribution.Server.Features.PreferredVersions.Types where

import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import Distribution.Version

import Data.SafeCopy (base, deriveSafeCopy)

data VersionStatus = NormalVersion | DeprecatedVersion deriving (Show, Eq, Ord, Enum)

-- NOTE: preferred versions no longer exist; this structure is actually only
-- used to keep around 'deprecatedVersions'.
--
-- The unused fields are kept around so as to not change the
-- automatically-derived serialization format.
data PreferredInfo = PreferredInfo {
    unused_preferredRanges :: [VersionRange],
    deprecatedVersions :: [Version],
    -- | Use 'sumRange' instead.
    unused_sumRange :: Maybe VersionRange
} deriving (Show, Eq)

{-# DEPRECATED
      unused_preferredRanges
      "This field is completely unused, but is kept around to not change the automatically derived serialization format." #-}
{-# DEPRECATED
      unused_sumRange
      "This field is completely unused, but is kept around to not change the automatically derived serialization format." #-}

emptyPreferredInfo :: PreferredInfo
emptyPreferredInfo = PreferredInfo [] [] Nothing


sumRange :: PreferredInfo -> Maybe VersionRange
sumRange (PreferredInfo ranges depr _) =
    let range = simplifyVersionRange $ foldr intersectVersionRanges anyVersion (map notThisVersion depr ++ ranges)
    in if isAnyVersion range || isNoVersion range
        then Nothing
        else Just range

$(deriveSafeCopy 0 'base ''PreferredInfo)

instance MemSize PreferredInfo where
    memSize (PreferredInfo a b c) = memSize3 a b c
