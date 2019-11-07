{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Users.AuthToken
    ( AuthToken
    , parseAuthToken, parseAuthTokenM, renderAuthToken
    , OriginalToken
    , convertToken, viewOriginalToken, generateOriginalToken
    , parseOriginalToken
    )
where

import Distribution.Server.Framework.MemSize
import Distribution.Server.Util.Nonce

import qualified Text.PrettyPrint as Disp
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Base16 as BS16
import qualified Crypto.Hash.SHA256 as SHA256
import Distribution.Pretty (Pretty(..))
import Distribution.Parsec (Parsec(..))
import qualified Distribution.Compat.CharParsing as P

import Control.Applicative ((<$>))
import Data.SafeCopy
import Data.Typeable (Typeable)

-- | Contains the original token which will be shown to the user
-- once and is NOT stored on the server. The user is expected
-- to provide this token on each request that should be
-- authed by it
newtype OriginalToken = OriginalToken Nonce
    deriving (Eq, Ord, Show, Typeable)

-- | Contains a hash of the original token
newtype AuthToken = AuthToken BSS.ShortByteString
    deriving (Eq, Ord, Read, Show, Typeable, MemSize)

convertToken :: OriginalToken -> AuthToken
convertToken (OriginalToken bs) =
    AuthToken $ BSS.toShort $ SHA256.hash $ getRawNonceBytes bs

viewOriginalToken :: OriginalToken -> T.Text
viewOriginalToken (OriginalToken ot) = T.pack $ renderNonce ot

-- | Generate a random 32 byte auth token. The token is represented as
-- in textual base16 way so it can easily be printed and parsed.
-- Note that this operation is not very efficient because it
-- calls 'withSystemRandom' for each token, but for the current
-- use case we only generate tokens infrequently so this should be fine.
generateOriginalToken :: IO OriginalToken
generateOriginalToken = OriginalToken <$> newRandomNonce 32

parseOriginalToken :: T.Text -> Either String OriginalToken
parseOriginalToken t = OriginalToken <$> parseNonce (T.unpack t)

parseAuthTokenM :: Monad m => T.Text -> m AuthToken
parseAuthTokenM t =
    case parseAuthToken t of
      Left err -> fail err
      Right ok -> return ok

parseAuthToken :: T.Text -> Either String AuthToken
parseAuthToken t
    | T.length t /= 64 = Left "auth token must be 64 charaters long"
    | not (T.all Char.isHexDigit t) = Left "only hex digits are allowed in tokens"
    | otherwise =
          Right $ AuthToken $ BSS.toShort $ fst $ BS16.decode $ T.encodeUtf8 t

renderAuthToken :: AuthToken -> T.Text
renderAuthToken (AuthToken bss) = T.decodeUtf8 $ BS16.encode $ BSS.fromShort bss

instance Parsec AuthToken where
    parsec =
        P.munch1 Char.isHexDigit >>= \x ->
        case parseAuthToken (T.pack x) of
          Left err -> fail err
          Right ok -> return ok

instance Pretty AuthToken where
    pretty = Disp.text . T.unpack . renderAuthToken

instance SafeCopy AuthToken where
    putCopy (AuthToken bs) = contain $ safePut (BSS.fromShort bs)
    getCopy =
        contain $ AuthToken . BSS.toShort <$> safeGet
