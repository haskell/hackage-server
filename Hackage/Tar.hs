-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Tar
-- Copyright   :  (c) 2007 Bjorn Bringert, 2008 Andrea Vezzosi
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Simplistic TAR archive reading and writing
--
-- Only handles the file names and file contents, ignores other file metadata.
--
-----------------------------------------------------------------------------
module Hackage.Tar (
  -- * Tar file entry
  Entry(..),
  FileType(..),
  
  -- * Reading tar format
  read,
  Entries(..), foldEntries, unfoldEntries,
  
  -- * Writing tar format
  write
  ) where

import Data.Char (ord)
import Data.Int  (Int8, Int64)
import Data.List (foldl')
import Numeric   (readOct, showOct)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)
import qualified System.FilePath as FilePath
         ( splitDirectories )
import qualified System.FilePath.Posix as FilePath.Posix
         ( joinPath, pathSeparator )
import System.Posix.Types (FileMode)

import Prelude hiding (read, fail)

data Entry = Entry {
    fileName    :: FilePath,
    fileMode    :: FileMode,
    fileType    :: FileType,
    linkTarget  :: FilePath,
    fileModTime :: EpochTime,
    fileSize    :: Int64,
    fileContent :: ByteString
  } deriving Show

type EpochTime = Int

data FileType = NormalFile
              | HardLink
              | SymbolicLink
              | Directory
              | Other Char
  deriving (Eq,Show)

--
-- * Reading
--

data Entries = Next Entry Entries | Done | Fail String

read :: ByteString -> Entries
read = unfoldEntries getEntry

unfoldEntries :: (a -> Either String (Maybe (Entry, a))) -> a -> Entries
unfoldEntries f = unfold
  where
    unfold x = case f x of
      Left err             -> Fail err
      Right Nothing        -> Done
      Right (Just (e, x')) -> Next e (unfold x')

foldEntries :: (Entry -> a -> a) -> a -> (String -> a) -> Entries -> a
foldEntries next done fail = fold
  where
    fold (Next e es) = next e (fold es)
    fold Done        = done
    fold (Fail err)  = fail err

getEntry :: ByteString -> Either String (Maybe (Entry, ByteString))
getEntry bs | endBlock = Right Nothing
            | BS.length hdr < 512 = Left "Truncated TAR archive"
            | not (checkChkSum hdr chkSum) = Left "TAR checksum error"
            | otherwise = Right (Just (entry, bs'''))
  where
   (hdr,bs') = BS.splitAt 512 bs

   endBlock     = getByte 0 hdr == '\0'

   fileSuffix = getString   0 100 hdr
   mode       = getOct    100   8 hdr
   size       = getOct    124  12 hdr
   modTime    = getOct    136  12 hdr
   chkSum     = getOct    148   8 hdr
   typ        = getByte   156     hdr
   linkTarget_ = getString 157 100 hdr
   filePrefix = getString 345 155 hdr

   padding    = (512 - size) `mod` 512
   (cnt,bs'') = BS.splitAt (fromIntegral size) bs'
   bs'''      = BS.drop (fromIntegral padding) bs''

   fileType_  = case typ of
                  '\0'-> NormalFile
                  '0' -> NormalFile
                  '1' -> HardLink
                  '2' -> SymbolicLink
                  '5' -> Directory
                  c   -> Other c

   path       = filePrefix ++ fileSuffix
   entry      = Entry {
     fileName    = path, 
     fileMode    = mode,
     fileType    = fileType_,
     linkTarget  = linkTarget_,
     fileModTime = modTime,
     fileSize    = size,
     fileContent = cnt
   }

checkChkSum :: ByteString -> Int -> Bool
checkChkSum hdr s = s == chkSum hdr' || s == signedChkSum hdr'
  where 
    -- replace the checksum with spaces
    hdr' = BS.concat [BS.take 148 hdr, BS.Char8.replicate 8 ' ', BS.drop 156 hdr]
    -- tar.info says that Sun tar is buggy and 
    -- calculates the checksum using signed chars
    chkSum = BS.Char8.foldl' (\x y -> x + ord y) 0
    signedChkSum = BS.Char8.foldl' (\x y -> x + (ordSigned y)) 0

ordSigned :: Char -> Int
ordSigned c = fromIntegral (fromIntegral (ord c) :: Int8)

-- * TAR format primitive input

getOct :: Integral a => Int64 -> Int64 -> ByteString -> a
getOct off len = parseOct . getString off len
  where parseOct "" = 0
        parseOct s = case readOct s of
                       [(x,_)] -> x
                       _       -> error $ "Number format error: " ++ show s

getBytes :: Int64 -> Int64 -> ByteString -> ByteString
getBytes off len = BS.take len . BS.drop off

getByte :: Int64 -> ByteString -> Char
getByte off bs = BS.Char8.index bs off

getString :: Int64 -> Int64 -> ByteString -> String
getString off len = BS.Char8.unpack . BS.Char8.takeWhile (/='\0') . getBytes off len

--
-- * Writing
--

-- | Creates an uncompressed archive
write :: [Entry] -> ByteString
write es = BS.concat $ map putEntry es ++ [BS.replicate (512*2) 0]

putEntry :: Entry -> ByteString
putEntry entry = BS.concat [ header, content, padding ]
  where
    header  = putHeader entry
    content = fileContent entry
    padding = BS.replicate paddingSize 0
    paddingSize = fromIntegral $ negate (fileSize entry) `mod` 512

putHeader :: Entry -> BS.ByteString
putHeader entry =  
     BS.Char8.pack $ take 148 block
  ++ putOct 8 chkSum
  ++ drop 156 block
  where
    block  = putHeaderNoChkSum entry
    chkSum = foldl' (\x y -> x + ord y) 0 block
        
putHeaderNoChkSum :: Entry -> String
putHeaderNoChkSum entry = concat
  [ putString  100 $ fileSuffix
  , putOct       8 $ fileMode entry
  , putOct       8 $ zero --tarOwnerID hdr
  , putOct       8 $ zero --tarGroupID hdr
  , putOct      12 $ fileSize entry
  , putOct      12 $ fileModTime entry
  , fill         8 $ ' ' -- dummy checksum
  , putFileType    $ fileType entry
  , putString  100 $ linkTarget entry -- FIXME: take suffix split at / if too long
  , putString    6 $ "ustar"
  , putString    2 $ "00" -- no nul byte
  , putString   32 $ "" --tarOwnerName hdr
  , putString   32 $ "" --tarGroupName hdr
  , putOct       8 $ zero --tarDeviceMajor hdr
  , putOct       8 $ zero --tarDeviceMinor hdr
  , putString  155 $ filePrefix
  , fill        12 $ '\NUL'
  ]
  where
    (filePrefix, fileSuffix) =
      splitLongPath (nativePathToTarPath (fileType entry) (fileName entry))
    zero :: Int
    zero = 0

putFileType :: FileType -> String
putFileType t = 
    putChar8 $ case t of
                 NormalFile      -> '0'
                 HardLink        -> '1'
                 SymbolicLink    -> '2'
                 Directory       -> '5'
                 Other c         -> c

-- | Convert a native path to a unix/posix style path
-- and for directories add a trailing @\/@.
--
nativePathToTarPath :: FileType -> FilePath -> FilePath
nativePathToTarPath ftype = addTrailingSep ftype
                          . FilePath.Posix.joinPath
                          . FilePath.splitDirectories
  where 
    addTrailingSep Directory path = path ++ [FilePath.Posix.pathSeparator]
    addTrailingSep _         path = path

-- | Takes a sanitized path, i.e. converted to Posix form
splitLongPath :: FilePath -> (String,String)
splitLongPath path =
    let (x,y) = splitAt (length path - 101) path 
              -- 101 since we will always move a separator to the prefix  
     in if null x 
         then if null y then err "Empty path." else ("", y)
         else case break (==FilePath.Posix.pathSeparator) y of
              --TODO: convert this to use FilePath.Posix.splitPath
                (_,"")    -> err "Can't split path." 
                (_,_:"")  -> err "Can't split path." 
                (y1,s:y2) | length p > 155 || length y2 > 100 -> err "Can't split path."
                          | otherwise -> (p,y2)
                      where p = x ++ y1 ++ [s]
  where err e = error $ show path ++ ": " ++ e

-- * TAR format primitive output

type FieldWidth = Int

putString :: FieldWidth -> String -> String
putString n s = take n s ++ fill (n - length s) '\NUL'

putOct :: Integral a => FieldWidth -> a -> String
putOct n x =
  let octStr = take (n-1) $ showOct x ""
   in fill (n - length octStr - 1) '0'
   ++ octStr
   ++ putChar8 '\NUL'

putChar8 :: Char -> String
putChar8 c = [c]

fill :: FieldWidth -> Char -> String
fill n c = replicate n c
