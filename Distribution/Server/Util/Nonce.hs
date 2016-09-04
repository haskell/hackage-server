{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.Server.Util.Nonce
    ( newRandomNonce
    , renderNonce, parseNonce, parseNonceM
    , getRawNonceBytes
    , Nonce
    )
where

import Distribution.Server.Framework.MemSize

import qualified Data.Char as Char
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS -- Only used for ASCII data
import qualified Data.ByteString.Base16 as Base16
import Data.Typeable
import Data.SafeCopy (base, extension, deriveSafeCopy, Migrate(..))
import System.IO

newtype Nonce = Nonce ByteString
  deriving (Eq, Ord, Show, Typeable, MemSize)

newRandomNonce :: Int -> IO Nonce
newRandomNonce len = do
  raw <- withFile "/dev/urandom" ReadMode $ \h ->
           BS.hGet h len
  return $! Nonce raw

getRawNonceBytes :: Nonce -> ByteString
getRawNonceBytes (Nonce b) = b

renderNonce :: Nonce -> String
renderNonce (Nonce nonce) = BS.unpack (Base16.encode nonce)

parseNonce :: String -> Either String Nonce
parseNonce t
    | not (all Char.isHexDigit t) = Left "only hex digits are allowed in tokens"
    | otherwise = Right (Nonce $ fst $ Base16.decode $ BS.pack t)

parseNonceM :: Monad m => String -> m Nonce
parseNonceM t =
    case parseNonce t of
      Left err -> fail err
      Right ok -> return ok

-- Nonce and Nonce_v0 have the same type, but the "new" nonce is
-- internally NOT base16 encoded
newtype Nonce_v0 = Nonce_v0 ByteString
  deriving (Eq, Ord, Show, Typeable, MemSize)

instance Migrate Nonce where
    type MigrateFrom Nonce = Nonce_v0
    migrate (Nonce_v0 x) = Nonce $ fst $ Base16.decode x

$(deriveSafeCopy 0 'base ''Nonce_v0)
$(deriveSafeCopy 1 'extension ''Nonce)
