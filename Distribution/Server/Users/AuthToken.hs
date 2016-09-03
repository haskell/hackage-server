{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Users.AuthToken
    ( AuthToken(..)
    , parseAuthToken
    , OriginalToken(..)
    , convertToken, viewOriginalToken, generateOriginalToken
    , parseOriginalToken
    )
where

import Distribution.Server.Framework.MemSize

import System.Random.MWC
import Distribution.Text
         ( Text(..) )
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint          as Disp
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Crypto.Hash.SHA256 as SHA256

import Control.Applicative ((<$>))
import Data.Aeson (ToJSON, FromJSON)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)

-- | Contains the original token which will be shown to the user
-- once and is NOT stored on the server. The user is expected
-- to provide this token on each request that should be
-- authed by it
newtype OriginalToken = OriginalToken BS.ByteString
    deriving (Eq, Ord, Show, Typeable)

-- | Contains a hash of the original token
newtype AuthToken = AuthToken T.Text
    deriving (Eq, Ord, Read, Show, Typeable, MemSize, ToJSON, FromJSON)

convertToken :: OriginalToken -> AuthToken
convertToken (OriginalToken bs) =
    AuthToken $ T.decodeUtf8 $ BS16.encode (SHA256.hash bs)

viewOriginalToken :: OriginalToken -> T.Text
viewOriginalToken (OriginalToken ot) = T.decodeUtf8 . BS16.encode $ ot

-- | Generate a random 64 byte auth token. The token is represented as
-- in textual base16 way so it can easily be printed and parsed.
-- Note that this operation is not very efficient because it
-- calls 'withSystemRandom' for each token, but for the current
-- use case we only generate tokens infrequently so this should be fine.
generateOriginalToken :: IO OriginalToken
generateOriginalToken =
    do randomBytes <-
           BS.pack . V.toList <$>
           (withSystemRandom . asGenIO) (`uniformVector` 32)
       return (OriginalToken randomBytes)

parseAuthToken :: T.Text -> Either String AuthToken
parseAuthToken t
    | T.length t /= 64 = Left "auth token must be 32 charaters long"
    | T.all Char.isHexDigit t = Left "only hex digits are allowed in tokens"
    | otherwise = Right (AuthToken t)

parseOriginalToken :: T.Text -> Either String OriginalToken
parseOriginalToken t
    | T.length t /= 64 = Left "original auth token must be 32 charaters long"
    | T.all Char.isHexDigit t = Left "only hex digits are allowed in tokens"
    | otherwise = Right (OriginalToken $ fst $ BS16.decode $ T.encodeUtf8 t)

instance Text AuthToken where
    disp (AuthToken tok) = Disp.text . T.unpack $ tok
    parse =
        Parse.munch1 Char.isHexDigit >>= \x ->
        case parseAuthToken (T.pack x) of
          Left err -> fail err
          Right ok -> return ok

$(deriveSafeCopy 0 'base ''AuthToken)
