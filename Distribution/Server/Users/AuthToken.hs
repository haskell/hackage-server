{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Server.Users.AuthToken
    ( AuthToken(..)
    , generateAuthToken
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

import Control.Applicative ((<$>))
import Data.Aeson (ToJSON, FromJSON)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)

newtype AuthToken = AuthToken T.Text
    deriving (Eq, Ord, Read, Show, Typeable, MemSize, ToJSON, FromJSON)

-- | Generate a random 64 byte auth token. The token is represented as
-- in textual base16 way so it can easily be printed and parsed.
-- Note that this operation is not very efficient because it
-- calls 'withSystemRandom' for each token, but for the current
-- use case we only generate tokens infrequently so this should be fine.
generateAuthToken :: IO AuthToken
generateAuthToken =
    do randomBytes <-
           BS16.encode . BS.pack . V.toList <$>
           (withSystemRandom . asGenIO) (`uniformVector` 64)
       return (AuthToken $ T.decodeUtf8 randomBytes)

instance Text AuthToken where
    disp (AuthToken tok) = Disp.text . T.unpack $ tok
    parse = AuthToken . T.pack <$> Parse.munch1 Char.isHexDigit

$(deriveSafeCopy 0 'base ''AuthToken)
