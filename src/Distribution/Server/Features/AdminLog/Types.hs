{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

module Distribution.Server.Features.AdminLog.Types where

import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Framework

import Data.SafeCopy (base, deriveSafeCopy)
import qualified Data.ByteString.Lazy.Char8 as BS

data GroupDesc = MaintainerGroup BS.ByteString | AdminGroup | TrusteeGroup | OtherGroup BS.ByteString deriving (Eq, Ord, Read, Show)

deriveSafeCopy 0 'base ''GroupDesc

instance MemSize GroupDesc where
    memSize (MaintainerGroup x) = memSize x
    memSize _ = 0

data AdminAction = Admin_GroupAddUser UserId GroupDesc | Admin_GroupDelUser UserId GroupDesc deriving (Eq, Ord, Read, Show)

instance MemSize AdminAction where
    memSize (Admin_GroupAddUser x y) = memSize2 x y
    memSize (Admin_GroupDelUser x y) = memSize2 x y

deriveSafeCopy 0 'base ''AdminAction
