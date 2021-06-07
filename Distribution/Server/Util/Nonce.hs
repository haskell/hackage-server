{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Util.Nonce
    ( newRandomNonce
    , renderNonce, parseNonce, parseNonceM
    , getRawNonceBytes
    , Nonce
    )
where

import Distribution.Server.Framework.MemSize

import Data.ByteString (ByteString)
import Data.SafeCopy (base, extension, deriveSafeCopy, Migrate(..))
import Data.Typeable
import System.IO
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS -- Only used for ASCII data
import qualified Data.Char as Char

newtype Nonce = Nonce ByteString
  deriving (Eq, Ord, Show, Typeable, MemSize)

newRandomNonce :: Int -> IO Nonce
newRandomNonce len =
    withFile "/dev/urandom" ReadMode $ \h ->
      fmap Nonce (BS.hGet h len)

getRawNonceBytes :: Nonce -> ByteString
getRawNonceBytes (Nonce b) = b

renderNonce :: Nonce -> String
renderNonce (Nonce nonce) = BS.unpack (Base16.encode nonce)

parseNonce :: String -> Either String Nonce
parseNonce t
    | not (all Char.isHexDigit t) = Left "only hex digits are allowed in tokens"
    | otherwise = Nonce <$> Base16.decode (BS.pack t)

parseNonceM :: (Monad m, MonadFail m) => String -> m Nonce
parseNonceM = either fail return . parseNonce


-- | Nonce and Nonce_v0 have the same type, but the "new" nonce is
-- internally NOT base16 encoded
newtype Nonce_v0 = Nonce_v0 ByteString
  deriving (Eq, Ord, Show, Typeable, MemSize)

instance Migrate Nonce where
    type MigrateFrom Nonce = Nonce_v0
    migrate (Nonce_v0 x) = either (const $ Nonce x) Nonce $ Base16.decode x

$(deriveSafeCopy 0 'base ''Nonce_v0)
$(deriveSafeCopy 1 'extension ''Nonce)
