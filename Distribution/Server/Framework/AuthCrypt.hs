{-# LANGUAGE ForeignFunctionInterface #-}
module Distribution.Server.Framework.AuthCrypt (
   PasswdPlain(..),
   checkCryptAuthInfo,
   PasswdHash(..),
   newPasswdHash,
   checkBasicAuthInfo,
   BasicAuthInfo(..),
   DigestAuthInfo(..),
   QopInfo(..),
   checkDigestAuthInfo,
  ) where

import Distribution.Server.Framework.AuthTypes
import Distribution.Server.Users.Types (UserName(..))

import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy
import Data.List (intercalate)

import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)

import Control.Concurrent.MVar (MVar, newMVar, withMVar)

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
-- Crypt auth
--

checkCryptAuthInfo :: HtPasswdHash -> PasswdPlain -> Bool
checkCryptAuthInfo (HtPasswdHash hash) (PasswdPlain passwd)
  = crypt passwd hash == hash

foreign import ccall unsafe "crypt" cCrypt :: CString-> CString -> CString

crypt :: String -- ^ Payload
      -> String -- ^ Salt
      -> String -- ^ Hash
crypt key seed = unsafePerformIO $ withMVar cryptMVar $ \_ -> do
    k <- newCAString key
    s <- newCAString seed
    peekCAString $ cCrypt k s

cryptMVar :: MVar ()
cryptMVar = unsafePerformIO $ newMVar ()
{-# NOINLINE cryptMVar #-}

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
          -- | QopAuthInt
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
