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
  write,
  simpleFileEntry,
  ) where

import Data.Char (ord)
import Data.Int  (Int64)
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

-- | A TAR archive is a sequence of entries.
data Entries = Next Entry Entries
             | Done
             | Fail String

type UserId    = Int
type GroupId   = Int
type EpochTime = Int -- ^ The number of seconds since the UNIX epoch
type DevMajor  = Int
type DevMinor  = Int

-- | TAR archive entry
data Entry = Entry {

    -- | Path of the file or directory. The path separator should be @/@ for
    -- portable TAR archives.
    fileName :: FilePath,

    -- | UNIX file mode.
    fileMode :: FileMode,

    -- | Numeric owner user id. Should be set to @0@ if unknown.
    ownerId :: UserId,

    -- | Numeric owner group id. Should be set to @0@ if unknown.
    groupId :: GroupId,

    -- | File size in bytes. Should be 0 for entries other than normal files.
    fileSize :: Int64,

    -- | Last modification time.
    modTime :: EpochTime,

    -- | Type of this entry.
    fileType :: FileType,

    -- | If the entry is a hard link or a symbolic link, this is the path of
    -- the link target. For all other entry types this should be @\"\"@.
    linkTarget :: FilePath,
    
    -- | The remaining meta-data is in the V7, ustar/posix or gnu formats
    -- For V7 there is no extended info at all and for posix/ustar the
    -- information is the same though the kind affects the way the information
    -- is encoded. 
    headerExt :: ExtendedHeader,

    -- | Entry contents. For entries other than normal 
    -- files, this should be an empty string.
    fileContent :: ByteString
  } 

data ExtendedHeader
   = V7
   | Ustar {
    -- | The owner user name. Should be set to @\"\"@ if unknown.
    ownerName :: String,

    -- | The owner group name. Should be set to @\"\"@ if unknown.
    groupName :: String,

    -- | For character and block device entries, this is the 
    -- major number of the device. For all other entry types, it
    -- should be set to @0@.
    deviceMajor :: DevMajor,

    -- | For character and block device entries, this is the 
    -- minor number of the device. For all other entry types, it
    -- should be set to @0@.
    deviceMinor :: DevMinor
   }
   | Gnu {
    -- | The owner user name. Should be set to @\"\"@ if unknown.
    ownerName :: String,

    -- | The owner group name. Should be set to @\"\"@ if unknown.
    groupName :: String,

    -- | For character and block device entries, this is the 
    -- major number of the device. For all other entry types, it
    -- should be set to @0@.
    deviceMajor :: DevMajor,

    -- | For character and block device entries, this is the 
    -- minor number of the device. For all other entry types, it
    -- should be set to @0@.
    deviceMinor :: DevMinor
   }

-- | TAR archive entry types.
data FileType = NormalFile
              | HardLink
              | SymbolicLink
              | CharacterDevice
              | BlockDevice
              | Directory
              | FIFO
              | ExtendedHeader
              | GlobalHeader
              | Custom Char   -- 'A' .. 'Z'
              | Reserved Char -- other / reserved / unknown

toFileTypeCode :: FileType -> Char
toFileTypeCode NormalFile      = '0'
toFileTypeCode HardLink        = '1'
toFileTypeCode SymbolicLink    = '2'
toFileTypeCode CharacterDevice = '3'
toFileTypeCode BlockDevice     = '4'
toFileTypeCode Directory       = '5'
toFileTypeCode FIFO            = '6'
toFileTypeCode ExtendedHeader  = 'x'
toFileTypeCode GlobalHeader    = 'g'
toFileTypeCode (Custom   c)    = c
toFileTypeCode (Reserved c)    = c

fromFileTypeCode :: Char -> FileType 
fromFileTypeCode '0'  = NormalFile
fromFileTypeCode '\0' = NormalFile
fromFileTypeCode '1'  = HardLink
fromFileTypeCode '2'  = SymbolicLink
fromFileTypeCode '3'  = CharacterDevice
fromFileTypeCode '4'  = BlockDevice
fromFileTypeCode '5'  = Directory
fromFileTypeCode '6'  = FIFO
fromFileTypeCode '7'  = NormalFile
fromFileTypeCode 'x'  = ExtendedHeader
fromFileTypeCode 'g'  = GlobalHeader
fromFileTypeCode  c   | c >= 'A' && c <= 'Z'
                      = Custom c
fromFileTypeCode  c   = Reserved c

simpleFileEntry :: FilePath -> ByteString -> Entry
simpleFileEntry name content = Entry {
    fileName = name,
    fileMode = 493, -- = 755 octal
    ownerId = 0,
    groupId  = 0,
    fileSize = BS.length content,
    modTime  = 0,
    fileType = NormalFile,
    linkTarget = "",
    headerExt = Ustar {
      ownerName = "",
      groupName = "",
      deviceMajor = 0,
      deviceMinor = 0
    },
    fileContent = content
  }

--
-- * Reading
--

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
getEntry bs
  | BS.length header < 512 = Left "Truncated TAR archive"
  | endBlock = Right Nothing
  | not (correctChecksum header chksum)  = Left "TAR checksum error"
  | magic /= "ustar\NUL00"
 && magic /= "ustar  \NUL" = Left $ "TAR entry not ustar format: " ++ show magic
  | otherwise = Right (Just (entry, bs'''))
  where
   (header,bs')  = BS.splitAt 512 bs

   endBlock   = getByte 0 header == '\0'

   name       = getString   0 100 header
   mode       = getOct    100   8 header
   uid        = getOct    108   8 header
   gid        = getOct    116   8 header
   size       = getOct    124  12 header
   mtime      = getOct    136  12 header
   chksum     = getOct    148   8 header
   typecode   = getByte   156     header
   linkname   = getString 157 100 header
   magic      = getChars  257   8 header
   uname      = getString 265  32 header
   gname      = getString 297  32 header
   devmajor   = getOct    329   8 header
   devminor   = getOct    337   8 header
   prefix     = getString 345 155 header
--   trailing   = getBytes  500  12 header --TODO: check all \0's

   padding    = (512 - size) `mod` 512
   (cnt,bs'') = BS.splitAt size bs'
   bs'''      = BS.drop padding bs''

   entry      = Entry {
     fileName    = prefix ++ name,
     fileMode    = mode,
     ownerId     = uid,
     groupId     = gid,
     fileSize    = size,
     modTime     = mtime,
     fileType    = fromFileTypeCode typecode,
     linkTarget  = linkname,
     headerExt   = case magic of
       "\0\0\0\0\0\0\0\0" -> V7
       "ustar\NUL00" -> Ustar {
         ownerName   = uname,
         groupName   = gname,
         deviceMajor = devmajor,
         deviceMinor = devminor
       }
       "ustar  \NUL" -> Gnu {
         ownerName   = uname,
         groupName   = gname,
         deviceMajor = devmajor,
         deviceMinor = devminor
       }
       _ -> V7, --FIXME: fail instead
     fileContent = cnt
   }

correctChecksum :: ByteString -> Int -> Bool
correctChecksum header checksum = checksum == checksum'
  where 
    -- sum of all 512 bytes in the header block,
    -- treating each byte as an 8-bit unsigned value
    checksum' = BS.Char8.foldl' (\x y -> x + ord y) 0 header'
    -- treating the 8 bytes of chksum as blank characters.
    header'   = BS.concat [BS.take 148 header, 
                           BS.Char8.replicate 8 ' ',
                           BS.drop 156 header]

-- * TAR format primitive input

getOct :: Integral a => Int64 -> Int64 -> ByteString -> a
getOct off len = parseOct
               . BS.Char8.unpack
               . BS.Char8.takeWhile (\c -> c /= '\NUL' && c /= ' ')
               . getBytes off len
  where
    parseOct "" = 0
    parseOct s = case readOct s of
                   [(x,[])] -> x
                   _        -> error $ "Number format error: " ++ show s

getBytes :: Int64 -> Int64 -> ByteString -> ByteString
getBytes off len = BS.take len . BS.drop off

getByte :: Int64 -> ByteString -> Char
getByte off bs = BS.Char8.index bs off

getChars :: Int64 -> Int64 -> ByteString -> String
getChars off len = BS.Char8.unpack . getBytes off len

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
  ++ putOct 7 checksum
  ++ ' ' : drop 156 block
  where
    block    = putHeaderNoChkSum entry
    checksum = foldl' (\x y -> x + ord y) 0 block
        
putHeaderNoChkSum :: Entry -> String
putHeaderNoChkSum entry = concat
    [ putString  100 $ name
    , putOct       8 $ fileMode entry
    , putOct       8 $ ownerId entry
    , putOct       8 $ groupId entry
    , putOct      12 $ fileSize entry
    , putOct      12 $ modTime entry
    , fill         8 $ ' ' -- dummy checksum
    , putChar8       $ toFileTypeCode (fileType entry)
    , putString  100 $ linkTarget entry
    ] ++
  case headerExt entry of
  V7    ->
      fill 255 '\NUL'
  ext@Ustar {}-> concat
    [ putString    8 $ "ustar\NUL00"
    , putString   32 $ ownerName ext
    , putString   32 $ groupName ext
    , putOct       8 $ deviceMajor ext
    , putOct       8 $ deviceMinor ext
    , putString  155 $ prefix
    , fill        12 $ '\NUL'
    ]
  ext@Gnu {} -> concat
    [ putString    8 $ "ustar  \NUL"
    , putString   32 $ ownerName ext
    , putString   32 $ groupName ext
    , putGnuDev    8 $ deviceMajor ext
    , putGnuDev    8 $ deviceMinor ext
    , putString  155 $ prefix
    , fill        12 $ '\NUL'
    ]
  where
    (prefix, name) =
      splitLongPath (nativePathToTarPath (fileType entry) (fileName entry))
    putGnuDev w n = case fileType entry of
      CharacterDevice -> putOct w n
      BlockDevice     -> putOct w n
      _               -> replicate w '\NUL'

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
