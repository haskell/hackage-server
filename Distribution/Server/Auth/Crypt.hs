{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Distribution.Server.Auth.Crypt (
   PasswdPlain(..),
   PasswdHash(..),
   newPasswdHash,
   checkPasswdBasicAuth,
  ) where

import Distribution.Server.Auth.Types
import Distribution.Server.Users.Types (UserName(..))

import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy
import Data.List (intercalate)

type RealmName = String

newPasswdHash :: RealmName -> UserName -> PasswdPlain -> PasswdHash
newPasswdHash realmName (UserName userName) (PasswdPlain passwd) =
    PasswdHash
  . show . md5
  . BS.Lazy.pack
  $ intercalate ":" [userName, realmName, passwd]

checkPasswdBasicAuth :: RealmName -> UserName -> PasswdHash -> PasswdPlain -> Bool
checkPasswdBasicAuth realmName userName hash pass =
    newPasswdHash realmName userName pass == hash
