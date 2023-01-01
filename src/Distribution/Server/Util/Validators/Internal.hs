{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Purely functional version of "Distribution.Server.Util.Validators"
-- for testing the validators.

module Distribution.Server.Util.Validators.Internal where

import           Control.Monad                   (unless)
import           Control.Monad.Except            (MonadError(..))

import           Data.Char                       (isSpace, isPrint)
import           Data.Text                       (Text)
import qualified Data.Text                       as T

import           Distribution.Pretty             (Pretty(..))
import           Distribution.Server.Users.Types (isValidUserNameChar)

-- Set up doctest to deal with text literals.

-- $setup
-- >>> :set -XOverloadedStrings

-- | Basic sanity checking on names.
--
-- >>> validName "Innocent User"
-- Right ()
--
-- >>> validName "Mr. X is the greatest super duper dude of all!"
-- Right ()
--
-- >>> validName "I am also a developer, maintainer, blogger, for Haskell, Hackage, Cabal, Stackage"
-- Left NameTooLong
--
-- >>> validName "My name has beeps \BEL, newlines \n, and \t tabs"
-- Left NameNotPrintable
--
validName :: Text -> Either InvalidName ()
validName str = do
  unless (T.length str <= 70) $ throwError NameTooLong
  unless (T.all isPrint str)  $ throwError NameNotPrintable

-- | Errors produced by 'validName' check.

data InvalidName
  = NameTooLong       -- ^ More than 70 characters long.
  | NameNotPrintable  -- ^ Contains unprintable characters.
  deriving (Eq, Show)

instance Pretty InvalidName where
  pretty = \case
    NameTooLong      -> "Sorry, we didn't expect names to be longer than 70 characters."
    NameNotPrintable -> "Unexpected character in name, please use only printable Unicode characters."

-- | Basic sanity checking on user names.
--
-- >>> validUserName "innocent_user_42"
-- Right ()
--
-- >>> validUserName "mr_X_stretches_the_Limit_of_50_characters_01234567"
-- Right ()
--
-- >>> validUserName "01234"
-- Right ()
--
-- >>> validUserName "dashes-not-allowed"
-- Left UserNameInvalidChar
--
-- >>> validUserName "questions_not_allowed?"
-- Left UserNameInvalidChar
--
-- >>> validUserName "my_Ego_busts_the_Limit_of_50_characters_01234567890"
-- Left UserNameTooLong
--
validUserName :: T.Text -> Either InvalidUserName ()
validUserName str = do
  unless (T.length str <= 50)            $ throwError UserNameTooLong
  unless (T.all isValidUserNameChar str) $ throwError UserNameInvalidChar

-- | Errors produced by 'validUserName' check.

data InvalidUserName
  = UserNameTooLong      -- ^ More than 50 characters long.
  | UserNameInvalidChar  -- ^ Contains character not matching 'isValidUserNameChar'.
  deriving (Eq, Show)

instance Pretty InvalidUserName where
  pretty = \case
    UserNameTooLong     -> "Sorry, we didn't expect login names to be longer than 50 characters."
    UserNameInvalidChar -> "Sorry, login names have to be ASCII characters only or _, no spaces or other symbols."

-- | Basic sanity checking in email.
--
-- >>> validEmail "Emmanuel.Lauterbach@phantasy-promi.darknet.de"
-- Right ()
--
-- >>> validEmail "gerd.lauchkopf+spam@posteo.de"
-- Right ()
--
-- >>> validEmail "Emmanuel.Lauterbachs.Cousin@mailrelay.tor.amazon-aws.bill-me.cold-fusion.bogus-domain.phantasy-promi.darknet.de"
-- Left EmailTooLong
--
-- >>> validEmail "\BELlingcat@a\nonymous.\to"
-- Left EmailNotPrintable
--
-- >>> validEmail "ich-im-aether"
-- Left EmailBadFormat
--
-- >>> validEmail "ich@guuugle@kom"
-- Left EmailBadFormat
--
-- >>> validEmail "Windows User @ Company . com"
-- Left EmailHasSpace
--
-- >>> validEmail "Name<name@domain.com>"
-- Left EmailHasAngle
--
validEmail :: T.Text -> Either InvalidEmail ()
validEmail str = do
  unless (T.length str <= 100)     $ throwError EmailTooLong
  unless (T.all isPrint str)       $ throwError EmailNotPrintable
  unless hasAtSomewhere            $ throwError EmailBadFormat
  unless (T.all (not.isSpace) str) $ throwError EmailHasSpace
  unless (T.all (not.isAngle) str) $ throwError EmailHasAngle
  where
    isAngle c = c == '<' || c == '>'
    hasAtSomewhere = case T.break (== '@') str of
      (before, rest)
        | Just (_, after) <- T.uncons rest ->
            not $ or
              [ T.null before
              , T.null after
              , '@' `T.elem` after
              ]
        | otherwise -> False

-- | Errors produced by 'validEmail' check.

data InvalidEmail
  = EmailTooLong       -- ^ More than 100 characters long.
  | EmailNotPrintable  -- ^ Contains unprintable characters.
  | EmailBadFormat     -- ^ Doesn't have exactly one @ sign.
  | EmailHasSpace      -- ^ Contains spaces.
  | EmailHasAngle      -- ^ Contains angle brackets.
  deriving (Eq, Show)

instance Pretty InvalidEmail where
  pretty = \case
    EmailTooLong      -> "Sorry, we didn't expect email addresses to be longer than 100 characters."
    EmailNotPrintable -> "Unexpected character in email address, please use only printable Unicode characters."
    EmailBadFormat    -> "Oops, that doesn't look like an email address."
    EmailHasSpace     -> "Oops, no spaces in email addresses please."
    EmailHasAngle     -> "Please use just the email address, not \"name\" <person@example.com> style."
