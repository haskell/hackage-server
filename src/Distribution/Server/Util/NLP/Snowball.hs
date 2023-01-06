module Distribution.Server.Util.NLP.Snowball where

-------------------------------------------------------------------------------
import           Control.Concurrent    (MVar, newMVar, withMVar)
import           Control.Monad         (forM, when)
-------------------------------------------------------------------------------
import           Data.ByteString.Char8 (packCStringLen, useAsCString)
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import qualified Data.Text.Encoding.Error as Text
-------------------------------------------------------------------------------
import           Foreign               (ForeignPtr, FunPtr, Ptr, newForeignPtr,
                                        nullPtr, withForeignPtr)
import           Foreign.C             (CInt (..), CString)
-------------------------------------------------------------------------------
import           System.IO.Unsafe      (unsafePerformIO)
-------------------------------------------------------------------------------

stem :: Text -> Text
stem word = let [a] = stems [word] in a

stems :: [Text] -> [Text]
stems ws =
    unsafePerformIO $
      do stemmer <- newStemmer
         stemsWith stemmer ws

-------------------------------------------------------------------------------

-- | A thread and memory safe Snowball stemmer instance.
newtype Stemmer = Stemmer (MVar (ForeignPtr Struct))

-- | Create a new reusable 'Stemmer' instance.
newStemmer :: IO Stemmer
newStemmer = do 
  struct <- stemmer_new
  when (struct == nullPtr) $
    error "Text.Snowball.newStemmer: nullPtr"
  structPtr <- newForeignPtr stemmer_delete struct
  mvar <- newMVar (structPtr)
  return $ Stemmer mvar

-- | Use a 'Stemmer' to stem a word.  This can be used more efficiently
--   than 'stem' because you can keep a stemmer around and reuse it, but it
--   requires 'IO' to ensure thread safety.
stemWith :: Stemmer -> Text -> IO Text
stemWith stemmer word = do
    [a] <- stemsWith stemmer [word]
    return a

-- | Use a 'Stemmer' to stem multiple words in one go.  This can be more
--   efficient than @'mapM' 'stemWith'@ because the 'Stemmer' is only
--   locked once.
stemsWith :: Stemmer -> [Text] -> IO [Text]
stemsWith (Stemmer mvar) ws =
    withMVar mvar $ \(structPtr) ->
      withForeignPtr structPtr $ \struct ->
        forM ws $ \word ->
          useAsCString (Text.encodeUtf8 word) $ \word' ->
            do ptr <- stemmer_stem struct word' $
                        fromIntegral $ Text.length word
               len <- stemmer_length struct
               bytes <- packCStringLen (ptr,fromIntegral len)
               return $ Text.decodeUtf8With Text.lenientDecode bytes


-------------------------------------------------------------------------------

data Struct

foreign import ccall unsafe "libstemmer.h english_ISO_8859_1_stemmer_new"
    stemmer_new :: IO (Ptr Struct)

foreign import ccall unsafe "libstemmer.h &english_ISO_8859_1_stemmer_delete"
    stemmer_delete :: FunPtr (Ptr Struct -> IO ())

foreign import ccall unsafe "libstemmer.h english_ISO_8859_1_stemmer_stem"
    stemmer_stem :: Ptr Struct -> CString -> CInt -> IO (CString)

foreign import ccall unsafe "libstemmer.h english_ISO_8859_1_stemmer_length"
    stemmer_length :: Ptr Struct -> IO CInt
