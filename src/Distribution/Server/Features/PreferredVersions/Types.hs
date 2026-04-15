{-# LANGUAGE TemplateHaskell #-}

module Distribution.Server.Features.PreferredVersions.Types where

import Data.SafeCopy (base, deriveSafeCopy)

data VersionStatus = NormalVersion | DeprecatedVersion | UnpreferredVersion deriving (Show, Eq, Ord, Enum)

$(deriveSafeCopy 0 'base ''VersionStatus)
