import qualified Text.ParserCombinators.ReadP as Parse
import Control.Monad (liftM2)
import Data.List (intercalate, find, sortBy)
import Data.Monoid (mappend)
import Data.Char (isAlphaNum, isDigit)
import Data.Ord (comparing)

-- data VaryFormat = Json | Xml | Html | Plain | Other

-- do some special processing here to fix Webkit's effing issues (and IE's, less so)
-- hackageVaryingAccept :: String -> [VaryFormat]

-- this just returns a list of content-types with quality peference from 0 to 1000 (0.0 to 1.0)
parseContentAccept :: String -> [((String, String), Int)]
parseContentAccept = maybe [] fst . find (null . snd) . Parse.readP_to_S parser
  where
    process :: [((String, String), Int)] -> [(String, String)]
    process = map fst . sortBy (flip (comparing snd)) . filter ((/=0) . snd)
    parser :: Parse.ReadP [((String, String), Int)]
    parser = flip Parse.sepBy1 (Parse.char ',') $ do
        Parse.skipSpaces
        -- a more 'accurate' type than (String, String)
        -- might be Maybe (String, Maybe String)
        mtype <- liftM2 (,) parseMediaType (Parse.char '/' >> parseMediaType)
        quality <- Parse.option 1000 $ do
            Parse.skipSpaces >> Parse.string ";q=" >> Parse.skipSpaces
            parseQuality
        --extensions aren't parsed
        return (mtype, quality)
    parseMediaType = (Parse.char '*' >> return []) Parse.<++ Parse.munch1 (\c -> case c of '-' -> True; '.' -> True; '+' -> True; _ -> isAlphaNum c)
    -- other characters technically allowed but never found in the wild: !#$%&^_`|~
    parseQuality :: Parse.ReadP Int -- returns a quality in fixed point (0.75 -> 750)
    parseQuality = (Parse.char '1' >> Parse.optional (Parse.char '.' >> Parse.many (Parse.char '0')) >> return 1000) Parse.<++
                   (Parse.char '0' >> zeroOption (Parse.char '.' >> zeroOption munch3Digits))
    zeroOption :: Parse.ReadP Int -> Parse.ReadP Int
    zeroOption parser = parser Parse.<++ return 0
    munch3Digits :: Parse.ReadP Int
    munch3Digits = fmap (\s ->  read $ take 3 (s++"00") :: Int) (Parse.munch1 isDigit)

--application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5

