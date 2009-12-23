
module Distribution.Server.Pages.Util
    ( hackageNotFound
    , hackageError
    ) where

import Distribution.Server.Pages.Template (hackagePage)

import Text.XHtml.Strict (Html, HTML, toHtml)

hackageNotFound :: HTML a => a -> Html
hackageNotFound contents
    = hackagePage "Not Found" [toHtml contents]

hackageError :: HTML a => a -> Html
hackageError contents
    = hackagePage "Error" [toHtml contents]
