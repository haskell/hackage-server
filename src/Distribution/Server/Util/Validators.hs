module Distribution.Server.Util.Validators
  ( guardValidLookingName
  , guardValidLookingUserName
  , guardValidLookingEmail
  ) where

import           Data.Text           (Text)
import           Distribution.Pretty (prettyShow)

import           Distribution.Server.Framework
import           Distribution.Server.Util.Validators.Internal (validName, validUserName, validEmail)

guardValidLookingName :: Text -> ServerPartE ()
guardValidLookingName =
  either (errBadUserName . prettyShow) return . validName

guardValidLookingUserName :: Text -> ServerPartE ()
guardValidLookingUserName =
  either (errBadRealName . prettyShow) return . validUserName

-- Make sure this roughly corresponds to the frontend validation in user-details-form.html.st
guardValidLookingEmail :: Text -> ServerPartE ()
guardValidLookingEmail =
  either (errBadEmail . prettyShow) return . validEmail

errBadUserName, errBadRealName, errBadEmail :: String -> ServerPartE a
errBadUserName err = errBadRequest "Problem with login name"    [MText err]
errBadRealName err = errBadRequest "Problem with name"          [MText err]
errBadEmail    err = errBadRequest "Problem with email address" [MText err]
