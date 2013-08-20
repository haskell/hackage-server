
module Distribution.Server.Pages.Util
    ( hackageNotFound
    , hackageError

    , makeInput
    , makeCheckbox
    ) where

import Distribution.Server.Pages.Template (hackagePage)

import Text.XHtml.Strict

hackageNotFound :: HTML a => a -> Html
hackageNotFound contents
    = hackagePage "Not Found" [toHtml contents]

hackageError :: HTML a => a -> Html
hackageError contents
    = hackagePage "Error" [toHtml contents]

makeInput :: [HtmlAttr] -> String -> String -> [Html]
makeInput attrs fname labelName = [label ! [thefor fname] << labelName,
                                   input ! (attrs ++ [name fname, identifier fname])]

makeCheckbox :: Bool -> String -> String -> String -> [Html]
makeCheckbox isChecked fname fvalue labelName = [input ! ([thetype "checkbox", name fname, identifier fname, value fvalue]
                                                 ++ if isChecked then [checked] else []),
                                        toHtml " ",
                                        label ! [thefor fname] << labelName]
