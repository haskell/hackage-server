-- | Middleware for performing CSRF checks.
module Distribution.Server.Framework.CSRF (csrfMiddleware) where

import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS
import Distribution.Server.Framework.Auth (AuthMethod (AuthToken), probeAttemptedAuthMethod)
import Distribution.Server.Framework.Error
import Distribution.Server.Framework.HtmlFormWrapper (rqRealMethod)
import Happstack.Server

isCsrfSafe :: Request -> Bool
isCsrfSafe req
    | Just AuthToken <- probeAttemptedAuthMethod req = True
    | rqRealMethod req `elem` safeMethods = True
    | Just headerSecFetchSite <- getHeader "Sec-Fetch-Site" req =
        headerSecFetchSite `elem` [BS.pack "same-origin", BS.pack "none"]
    | Just userAgent <- getHeader "User-Agent" req, whitelistedUA userAgent = True
    | otherwise = False
  where
    safeMethods = [GET, HEAD, OPTIONS]
    -- TODO make this whitelist configurable
    whitelistedUA ua =
        any
            (`BS.isPrefixOf` ua)
            -- UA set by `cabal upload` and such
            [ BS.pack "cabal-install/"
              -- UA set by Stack
            , BS.pack "The Haskell Stack"
            , -- Add some other common CLI tools here too?
              BS.pack "curl/"
            , -- referenced in this repository. Unclear whether strictly needed, but whitelisting just in case:
              BS.pack "hackage-import/"
            , BS.pack "hackage-mirror/"
            , BS.pack "hackage-build/"
            , BS.pack "hackage-server-testsuite/"
            , -- default of HTTP library (used by test suite)
              BS.pack "haskell-HTTP/"
            , -- deprecated default of HTTP library
              BS.pack "hs-http-"
            ]

-- | Middleware to check for CSRF safety. If the request fails the checks, then we throw a 403 error
-- with an appropriate message.
csrfMiddleware :: ServerPartE ()
csrfMiddleware = do
    req <- askRq
    unless (isCsrfSafe req) $ do
        throwError $
            ErrorResponse
                403
                []
                "Forbidden"
                [ MText
                    "This request fails CSRF protection checks. For automated use cases consider \
                    \switching to API tokens. For browsers, update to a more recent version of \
                    \your browser which supports sec-fetch headers."
                ]
