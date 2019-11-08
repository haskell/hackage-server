{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC8
import Data.Bits

import qualified Data.Serialize as Ser

import Distribution.Server.Util.ReadDigest

-- IUT
import Distribution.Server.Features.Security.MD5
import Distribution.Server.Features.Security.SHA256

main :: IO ()
main = defaultMain tests

data4KiB :: BL.ByteString
data4KiB = BL.take 4096 $ BL.fromChunks (repeat ("Some Data 123456\NUL" :: BC8.ByteString))

data1MiB :: BL.ByteString
data1MiB = BL.take (1024*1024) $ BL.fromChunks (repeat ("Some Data 123456\NUL" :: BC8.ByteString))

data1MiB' :: BL.ByteString
data1MiB' = BL.fromChunks [mconcat (BL.toChunks data1MiB)]

hex2bs :: String -> BL.ByteString
hex2bs s = BL.fromChunks [fst $ B16.decode (BC8.pack s)]

-- Unfortunately, our SHA256Digest Put instance uses a redundant size-prefix
hex2bsPfx :: String -> BL.ByteString
hex2bsPfx s = BL.fromChunks [sizePfx, fst $ B16.decode (BC8.pack s)]
  where
    sizePfx = case finiteBitSize (0::Int) of
        64 -> "\NUL\NUL\NUL\NUL\NUL\NUL\NUL "
        32 -> "\NUL\NUL\NUL "
        _ -> undefined

tests, sha256tests, md5tests :: TestTree
tests = testGroup "Tests" [sha256tests, md5tests]

sha256tests = testGroup "Distribution.Server.Features.Security.SHA256"
    [ testGroup l
      [ testCase "Show"          $ show digest @?= hexref -- NB: w/o quotation marks
        -- SHA256Digest has no Binary instance
      , testCase "Serialize.put" $ Ser.runPutLazy (Ser.put digest) @?= hex2bsPfx hexref
      , testCase "Serialize.get" $ Ser.runGetLazy (Ser.get :: Ser.Get SHA256Digest) (hex2bsPfx hexref) @?= Right digest
      , testCase "readDigest"    $ readDigest hexref @?= Right digest
      ]
    | (l, digest, hexref) <- sha256vecs ]
  where
    sha256vecs :: [(String, SHA256Digest, String)]
    sha256vecs = [ ("mempty",    sha256 Data.Monoid.mempty, "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
                 , ("data4KiB",  sha256 data4KiB,           "491f738dfe6eef1182de33234018eac245b177b254397ffa05017d0dc84b41bb")
                 , ("data1MiB",  sha256 data1MiB,           "a1dffb64f57d6271cc2c97084636778c604e5ceeeae5e29e52097e6fdb7dce8a")
                 , ("data1MiB'", sha256 data1MiB',          "a1dffb64f57d6271cc2c97084636778c604e5ceeeae5e29e52097e6fdb7dce8a")
                 ]

md5tests = testGroup "Distribution.Server.Features.Security.MD5"
    [ testGroup l
      [ testCase "Show"          $ show digest @?= hexref -- NB: w/o quotation marks
      -- MD5Digest has no Binary instances anymore
      -- , testCase "Binary.put"    $ Bin.runPut (Bin.put digest) @?= hex2bs hexref
      -- , testCase "Binary.get"    $ Bin.runGet (Bin.get :: Bin.Get MD5Digest) (hex2bs hexref) @?= digest
      , testCase "Serialize.put" $ Ser.runPutLazy (Ser.put digest) @?= hex2bs hexref
      , testCase "Serialize.get" $ Ser.runGetLazy (Ser.get :: Ser.Get MD5Digest) (hex2bs hexref) @?= Right digest
      , testCase "readDigest"    $ readDigest hexref @?= Right digest
      , testCase "runLazyMD5"    $ runLazyMD5 dat @?= digest
      ]
    | (l, dat, hexref) <- md5vecs, let digest = md5 dat ]
  where
    md5vecs :: [(String, BL.ByteString, String)]
    md5vecs = [ ("mempty",    Data.Monoid.mempty, "d41d8cd98f00b204e9800998ecf8427e")
              , ("data4KiB",  data4KiB,           "fe5307f3190928af1831e54ce50b213e")
              , ("data1MiB",  data1MiB,           "200c265d65faf6319ddbbd47535f07bc")
              , ("data1MiB'", data1MiB',          "200c265d65faf6319ddbbd47535f07bc")
              ]

    runLazyMD5 :: BL.ByteString -> MD5Digest
    runLazyMD5 bs0 = go [] (lazyMD5 bs0)
      where
        go acc (BsChunk chunk next) = go (chunk:acc) next
        go acc (BsEndMd5 digest)
            | BL.fromChunks (reverse acc) /= bs0 = error "runLazyMD5: payload mismatch"
            | otherwise = digest
