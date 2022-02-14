module Distribution.Server.Framework.AuthCrypt (
   PasswdPlain(..),
   PasswdHash(..),
   newPasswdHash,
   checkBasicAuthInfo,
   BasicAuthInfo(..),
   checkDigestAuthInfo,
   DigestAuthInfo(..),
   QopInfo(..),
  ) where

import Distribution.Server.Features.Security.MD5
import Distribution.Server.Framework.AuthTypes
import Distribution.Server.Users.Types (UserName(..))

import qualified Data.ByteString.Lazy.Char8 as BS.Lazy -- Only used for ASCII data
import Data.List (intercalate)

-- Hashed passwords are stored in the format:
--
-- @md5 (username ++ ":" ++ realm ++ ":" ++ password)@.
--
-- This format enables us to use either the basic or digest
-- HTTP authentication methods.

-- | Create a new 'PasswdHash' suitable for safe permanent storage.
--
newPasswdHash :: RealmName -> UserName -> PasswdPlain -> PasswdHash
newPasswdHash (RealmName realmName) (UserName userName) (PasswdPlain passwd) =
    PasswdHash $ md5HexDigest [userName, realmName, passwd]

------------------
-- HTTP Basic auth
--

data BasicAuthInfo = BasicAuthInfo {
       basicRealm    :: RealmName,
       basicUsername :: UserName,
       basicPasswd   :: PasswdPlain
     }

checkBasicAuthInfo :: PasswdHash -> BasicAuthInfo -> Bool
checkBasicAuthInfo hash (BasicAuthInfo realmName userName pass) =
    newPasswdHash realmName userName pass == hash

------------------
-- HTTP Digest auth
--

data DigestAuthInfo = DigestAuthInfo {
       digestUsername :: UserName,
       digestNonce    :: String,
       digestResponse :: String,
       digestURI      :: String,
       digestRqMethod :: String,
       digestQoP      :: QopInfo
     }
  deriving Show

data QopInfo = QopNone
             | QopAuth {
                 digestNonceCount  :: String,
                 digestClientNonce :: String
               }
  deriving Show

-- See RFC 2617 http://www.ietf.org/rfc/rfc2617
--
checkDigestAuthInfo :: PasswdHash -> DigestAuthInfo -> Bool
checkDigestAuthInfo (PasswdHash passwdHash)
                (DigestAuthInfo _username nonce response uri method qopinfo) =
    hash3 == response
  where
    hash1  = passwdHash
    hash2  = md5HexDigest [method, uri]
    hash3  = case qopinfo of
               QopNone           -> md5HexDigest [hash1, nonce, hash2]
               QopAuth nc cnonce -> md5HexDigest [hash1, nonce, nc, cnonce, "auth", hash2]

------------------
-- Utils
--

md5HexDigest :: [String] -> String
md5HexDigest = show . md5 . BS.Lazy.pack . intercalate ":"
