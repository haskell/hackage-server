module Distribution.Server.Util.Validators
  ( guardValidLookingName
  , guardValidLookingUserName
  , guardValidLookingEmail
  ) where

import           Data.Char (isSpace, isPrint)
import qualified Data.Text as T

import           Distribution.Server.Framework
import           Distribution.Server.Users.Types (isValidUserNameChar)

guardValidLookingName :: T.Text -> ServerPartE ()
guardValidLookingName str = either errBadUserName return $ do
  guard (T.length str <= 70) ?! "Sorry, we didn't expect names to be longer than 70 characters."
  guard (T.all isPrint str)  ?! "Unexpected character in name, please use only printable Unicode characters."

guardValidLookingUserName :: T.Text -> ServerPartE ()
guardValidLookingUserName str = either errBadRealName return $ do
  guard (T.length str <= 50)    ?! "Sorry, we didn't expect login names to be longer than 50 characters."
  guard (T.all isValidUserNameChar str) ?! "Sorry, login names have to be ASCII characters only or _, no spaces or other symbols."

-- Make sure this roughly corresponds to the frontend validation in user-details-form.html.st
guardValidLookingEmail :: T.Text -> ServerPartE ()
guardValidLookingEmail str = either errBadEmail return $ do
  guard (T.length str <= 100)     ?! "Sorry, we didn't expect email addresses to be longer than 100 characters."
  guard (T.all isPrint str)       ?! "Unexpected character in email address, please use only printable Unicode characters."
  guard hasAtSomewhere            ?! "Oops, that doesn't look like an email address."
  guard (T.all (not.isSpace) str) ?! "Oops, no spaces in email addresses please."
  guard (T.all (not.isAngle) str) ?! "Please use just the email address, not \"name\" <person@example.com> style."
  where
    isAngle c = c == '<' || c == '>'
    hasAtSomewhere =
      let (before, after) = T.span (/= '@') str
       in T.length before >= 1
       && T.length after  >  1
       && not ('@' `T.elem` after)

errBadUserName, errBadRealName, errBadEmail :: String -> ServerPartE a
errBadUserName err = errBadRequest "Problem with login name" [MText err]
errBadRealName err = errBadRequest "Problem with name" [MText err]
errBadEmail    err = errBadRequest "Problem with email address" [MText err]
