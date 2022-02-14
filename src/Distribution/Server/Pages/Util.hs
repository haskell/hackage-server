
module Distribution.Server.Pages.Util
    ( hackageNotFound
    , hackageError

    , makeInput
    , makeCheckbox

    , packageType
    ) where

import Data.List (intercalate)

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

packageType :: Bool
            -> Int
            -> Int
            -> Int
            -> String
packageType hasLibrary numExes numTests numBenches =
    commaSeparate $ map format components
  where
    components = (if hasLibrary     then [Library]               else [])
              ++ (if numExes    > 0 then [Programs numExes]      else [])
              ++ (if numTests   > 0 then [Tests numTests]        else [])
              ++ (if numBenches > 0 then [Benchmarks numBenches] else [])

    format Library        = "library"
    format (Programs n)   = "program"   ++ suffix n
    format (Tests n)      = "test"      ++ suffix n
    format (Benchmarks n) = "benchmark" ++ suffix n

    suffix n = if n > 1 then "s" else ""

    commaSeparate []  = []
    commaSeparate [x] = x
    commaSeparate xs  = let (init', [last']) = splitAt (length xs - 1) xs
                        in intercalate ", " init' ++ " and " ++ last'

data Component = Library
               | Programs   !Int
               | Tests      !Int
               | Benchmarks !Int
