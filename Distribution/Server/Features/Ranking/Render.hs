-- | Produces HTML related to the "Stars:" section in the package page.
-- | Should only be used via the Ranking feature (see renderStarsHtml)
module Distribution.Server.Features.Ranking.Render
  ( renderStarsAnon
  , starConfirmationPage
  , alreadyStarredPage
  ) where

import Distribution.Package
import Distribution.Server.Pages.Template
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource

import Text.XHtml.Strict

-- When the user is not authenticated/logged in, simply
-- display the number of stars the package has and a link
-- to add a star (which prompts for authentication).
renderStarsAnon :: Int -> PackageName -> (String, Html)
renderStarsAnon numStars pkgname =
  ( "Stars:",
      form  ! [ action $    "star/" ++ unPackageName pkgname
              , method      "POST" ]
      << thespan <<
      (toHtml $ show numStars  ++ " " +++
        ("[" +++
        input ! [ thetype     "submit"
                , value       "Star this package"
                , theclass    "text-button" ]
        +++ "]")
      )
  )

-- A page that confirms a package was successfully starred and
-- provides a link back to the package page.
starConfirmationPage :: PackageName -> String -> String -> Resource.XHtml
starConfirmationPage pkgname baseuri message =
  Resource.XHtml $ hackagePage "Star a Package"
  [ h3 << message
  , br
  , anchor ! [ href $ baseuri ++ "/package/" ++ unPackageName pkgname ] << "Return"
  ]

-- Shown when a user has already starred a package.
-- Gives an option to remove the star, and provides a link
-- back to the package page.
alreadyStarredPage :: PackageName -> String -> Resource.XHtml
alreadyStarredPage pkgname baseuri =
  Resource.XHtml $ hackagePage "Star a Package"
  [ h3 << "You have already starred this package."
  , form  ! [ action $ baseuri ++ "/package/unstar/" ++ unPackageName pkgname
            , method "POST" ]
      << thespan <<
      ("[" +++
      input ! [ thetype     "submit"
              , value       "Remove star from this package"
              , theclass    "text-button" ]
      +++ "]")
  , br
  , anchor ! [ href $ baseuri ++ "/package/" ++ unPackageName pkgname ]
    << "Return"
  ]
