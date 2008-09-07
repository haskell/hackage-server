{-# LANGUAGE ForeignFunctionInterface #-}
module Distribution.Server.Auth.Crypt (
   PasswdPlain(..),
   PasswdHash(..),
   newPasswd,
   checkPasswd,

   -- * raw crypt
   crypt
  ) where

import Distribution.Server.Auth.Types

import System.Random (Random(randomRs), RandomGen)

import Foreign (unsafePerformIO)
import Foreign.C (CString, withCAString, peekCAString)

newPasswd :: RandomGen rnd => rnd -> PasswdPlain -> PasswdHash
newPasswd rnd (PasswdPlain plain) = PasswdHash (crypt plain salt)
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

foreign import ccall "crypt.h crypt"
  ccrypt :: CString -> CString -> IO CString
