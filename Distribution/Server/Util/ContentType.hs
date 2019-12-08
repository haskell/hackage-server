module Distribution.Server.Util.ContentType (
    parseContentAccept
  ) where

import Happstack.Server.Types (ContentType(..))

import qualified Text.ParserCombinators.ReadP as Parse
import Control.Monad
import Data.List (find, sortBy)
import Data.Char (isAlphaNum, isDigit)
import Data.Ord (comparing)

-- data VaryFormat = Json | Xml | Html | Plain | Other

-- do some special processing here to fix Webkit's effing issues (and IE's, less so)
-- hackageVaryingAccept :: String -> [VaryFormat]

-- | This returns a list of content-types sorted by quality preference (highest qvalue first)
parseContentAccept :: String -> [ContentType]
parseContentAccept = process . maybe [] fst . find (null . snd) . Parse.readP_to_S parser
  where
    process :: [(a, Int)] -> [a]
    process = map fst . sortBy (flip (comparing snd)) . filter ((/=0) . snd)
    parser :: Parse.ReadP [(ContentType, Int)]
    parser = flip Parse.sepBy1 (Parse.char ',') $ do
        Parse.skipSpaces
        -- a more 'accurate' type than (String, String)
        -- might be Maybe (String, Maybe String)
        typ <- parseMediaType
        void $ Parse.char '/'
        subTyp <- parseMediaType
        Parse.skipSpaces
        -- hack to deal with parameter send by Chrome in `application/signed-exchange;v=b3`
        -- TODO: parse other parameters
        params <- Parse.option [] (Parse.string ";v=b3" >> Parse.skipSpaces >> return [("v","b3")])
        quality <- Parse.option 1000 $ do
            Parse.string ";q=" >> Parse.skipSpaces
            parseQuality
            -- TODO: parse optional extensions
        return (ContentType {ctType = typ, ctSubtype = subTyp, ctParameters = params}, quality)

    parseMediaType = (Parse.char '*' >> return []) Parse.<++ Parse.munch1 (\c -> case c of '-' -> True; '.' -> True; '+' -> True; _ -> isAlphaNum c)

    -- other characters technically allowed but never found in the wild: !#$%&^_`|~
    parseQuality :: Parse.ReadP Int -- returns a quality in fixed point (0.75 -> 750)
    parseQuality = (Parse.char '1' >> Parse.optional (Parse.char '.' >> Parse.many (Parse.char '0')) >> return 1000) Parse.<++
                   (Parse.char '0' >> zeroOption (Parse.char '.' >> zeroOption munch3Digits))

    zeroOption :: Parse.ReadP Int -> Parse.ReadP Int
    zeroOption p = p Parse.<++ return 0

    munch3Digits :: Parse.ReadP Int
    munch3Digits = fmap (\s ->  read $ take 3 (s++"00") :: Int) (Parse.munch1 isDigit)

--application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5

