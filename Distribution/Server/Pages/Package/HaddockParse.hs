module Distribution.Server.Pages.Package.HaddockParse (parse) where

import Documentation.Haddock.Types
import Documentation.Haddock.Parser

-- supposedly never fails
parse :: String -> Maybe (DocH mod String)
parse = Just . toRegular . _doc . parseParas Nothing
