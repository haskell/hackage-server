{-# LANGUAGE TemplateHaskell #-}

module Distribution.Server.Features.PreferredVersions.Types where

data VersionStatus = NormalVersion | DeprecatedVersion deriving (Show, Eq, Ord, Enum)
