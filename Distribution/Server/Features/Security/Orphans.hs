-- | Orphan definitions for some of the types in hackage-security
--
-- We don't add these to hackage-security itself because it would increment
-- the required dependencies for hackage-security, which can cause problems
-- for other clients (in particular, cabal-install).
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Server.Features.Security.Orphans where

-- stdlib
import Control.DeepSeq
import Data.SafeCopy
import Data.Serialize
import qualified Data.ByteString.Lazy as BS.L
import qualified Crypto.Sign.Ed25519  as Ed25519

-- hackage
import Distribution.Server.Framework.MemSize

-- hackage-security
import Hackage.Security.Util.Some
import Text.JSON.Canonical (Int54)
import qualified Hackage.Security.Server      as Sec
import qualified Hackage.Security.Util.Pretty as Sec

{-------------------------------------------------------------------------------
  SafeCopy instances
-------------------------------------------------------------------------------}


instance SafeCopy (Some Sec.Key) where
    getCopy = contain get
    putCopy = contain . put
     -- use default Serialize instance

instance Serialize (Some Sec.Key) where
  put = put . Sec.renderJSON_NoLayout
  get = aux =<< get
    where
      aux :: BS.L.ByteString -> Get (Some Sec.Key)
      aux enc = case Sec.parseJSON_NoKeys_NoLayout enc of
                  Left  err -> fail $ Sec.pretty err
                  Right key -> return key

instance SafeCopy Sec.FileVersion where
    getCopy = contain get
    putCopy = contain . put
  -- use default Serialize instance

instance Serialize Sec.FileVersion where
  put (Sec.FileVersion v) = put v
  get = Sec.FileVersion `fmap` get

-- Before hackage-security moved to Int64, it was using Int, so in order to
-- keep the Serialize instance the same, that's what we translate to here.
instance SafeCopy Int54 where
    getCopy = contain get
    putCopy = contain . put
  -- use default Serialize instance

instance Serialize Int54 where
   put = put . (fromIntegral :: Int54 -> Int)
   get = (fromIntegral :: Int -> Int54) `fmap` get

{-------------------------------------------------------------------------------
  MemSize instances
-------------------------------------------------------------------------------}

instance MemSize (Some Sec.Key) where
  memSize (Some key) = memSize key

instance MemSize (Sec.Key typ) where
  memSize (Sec.KeyEd25519 pub pri) = memSize pub + memSize pri

instance MemSize (Ed25519.PublicKey) where
  memSize = memSize . Ed25519.unPublicKey

instance MemSize (Ed25519.SecretKey) where
  memSize = memSize . Ed25519.unSecretKey

instance MemSize Sec.FileVersion where
  memSize (Sec.FileVersion v) = memSize v

instance MemSize Int54 where
  memSize _ = 4

{-------------------------------------------------------------------------------
  NFData instances
-------------------------------------------------------------------------------}

instance NFData Int54 where
  rnf a = a `seq` ()
