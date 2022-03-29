-- | Produces HTML related to the "Votes:" section in the package page.
-- | Should only be used via the Votes feature (see renderVotesHtml)
module Distribution.Server.Features.Votes.Render
  ( renderVotesAnon
  , voteConfirmationPage
  , alreadyVotedPage
  ) where

import Distribution.Package
import Distribution.Server.Pages.Template
import qualified Distribution.Server.Framework.ResponseContentTypes as Resource

import Text.XHtml.Strict

-- When the user is not authenticated/logged in, simply
-- display the number of votes the package has and a link
-- to add a vote (which prompts for authentication).
renderVotesAnon :: Int -> PackageName -> Html
renderVotesAnon numVotes pkgname =
  form  ! [ action $ "/package/" ++ unPackageName pkgname ++ "/votes"
          , method      "POST" ]
  << thespan <<
  [ toHtml $  show numVotes ++ " "
  , toHtml $  ("[" +++
      hidden  "_method" "PUT" +++
      input ! [ thetype     "submit"
              , value       "Vote for this package"
              , theclass    "text-button" ]
      +++ "]")
  ]

-- A page that confirms a package was successfully voted for and
-- provides a link back to the package page.
voteConfirmationPage :: PackageName -> String -> Resource.XHtml
voteConfirmationPage pkgname message =
  Resource.XHtml $ hackagePage "Vote for a Package"
  [ h3 << message
  , br
  , anchor ! [ href $ "/package/" ++ unPackageName pkgname ] << "Return"
  ]

-- Shown when a user has already voted for a package.
-- Gives an option to remove the vote, and provides a link
-- back to the package page.
alreadyVotedPage :: PackageName -> Resource.XHtml
alreadyVotedPage pkgname =
  Resource.XHtml $ hackagePage "Vote for a Package"
  [ h3 <<   "You have already voted for this package."
  , form  ! [ action $ "/package/" ++ unPackageName pkgname ++ "/votes"
            , method "POST" ]
      << thespan <<
      ("[" +++
      hidden  "_method" "DELETE" +++
      input ! [ thetype     "submit"
              , value       "Remove your vote from this package"
              , theclass    "text-button" ]
      +++ "]")
  , br
  , anchor ! [ href $ "/package/" ++ unPackageName pkgname ] << "Return"
  ]
