{-# LANGUAGE ForeignFunctionInterface, CPP #-}
module Distribution.Server.Auth.Crypt (
   PasswdPlain(..),
   PasswdHash(..),
   newBasicPass,
   newDigestPass,
   checkPasswd,

   -- * raw crypt
   crypt
  ) where

import Distribution.Server.Auth.Types
import Distribution.Server.Users.Types (UserName(..))

import System.Random (Random(randomRs), RandomGen)
import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy
import Data.List (intercalate)

import Foreign (unsafePerformIO)
import Foreign.C (CString, withCAString, peekCAString)

newDigestPass :: UserName -> PasswdPlain -> String -> PasswdHash
newDigestPass (UserName userName) (PasswdPlain passwd) realm =
    PasswdHash . show . md5 . BS.Lazy.pack $ intercalate ":" [userName, realm, passwd]

newBasicPass :: RandomGen rnd => rnd -> PasswdPlain -> PasswdHash
newBasicPass rnd (PasswdPlain plain) = PasswdHash (crypt plain salt)
  where
    salt :: [Char]
    salt = take 2 (randomRs ('\1', '\255') rnd)

checkPasswd :: PasswdPlain -> PasswdHash -> Bool
checkPasswd (PasswdPlain plain) (PasswdHash hash) = crypt plain hash == hash

crypt :: String -> String -> String
crypt key salt =
  unsafePerformIO $
    withCAString key  $ \keyPtr ->
    withCAString salt $ \saltPtr ->
      peekCAString =<< ccrypt keyPtr saltPtr

foreign import ccall "crypt"
  ccrypt :: CString -> CString -> IO CString
