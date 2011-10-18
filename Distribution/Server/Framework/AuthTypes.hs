{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Distribution.Server.Framework.AuthTypes where

import Data.Binary (Binary)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)

-- | A plain, unhashed password. Careful what you do with them.
--
newtype PasswdPlain = PasswdPlain String
  deriving Eq

-- | A password hash. It actually contains the hash of the username, passowrd
-- and realm.
--
-- Hashed passwords are stored in the format
-- @md5 (username ++ ":" ++ realm ++ ":" ++ password)@. This format enables
-- us to use either the basic or digest HTTP authentication methods.
--
newtype PasswdHash = PasswdHash String
  deriving (Eq, Ord, Show, Binary, Typeable)

-- | These are the *old* crypt format password hashes (salted DES: perl crypt).
-- Not the same as the new hashes we store in 'PasswdHash'.
newtype HtPasswdHash = HtPasswdHash String
  deriving (Eq, Show)

newtype RealmName = RealmName String
  deriving (Show, Eq)

$(deriveSafeCopy 0 'base ''PasswdPlain)
$(deriveSafeCopy 0 'base ''PasswdHash)
$(deriveSafeCopy 0 'base ''HtPasswdHash)