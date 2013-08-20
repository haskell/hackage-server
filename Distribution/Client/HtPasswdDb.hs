-- | Parsing @.htpasswd@ files
--
module Distribution.Client.HtPasswdDb (
    HtPasswdDb, HtPasswdHash(..),
    parse,
  ) where

import Distribution.Server.Users.Types (UserName(..))

type HtPasswdDb = [(UserName, Maybe HtPasswdHash)]

newtype HtPasswdHash = HtPasswdHash String
  deriving (Eq, Show)

parse :: String -> Either String HtPasswdDb
parse = accum 0 [] . map parseLine . lines
  where
    accum _ pairs []               = Right (reverse pairs)
    accum n pairs (Just pair:rest) = accum (n+1) (pair:pairs) rest
    accum n _     (Nothing  :_   ) = Left errmsg
      where errmsg = "parse error in htpasswd file on line " ++ show (n :: Int)

parseLine :: String -> Maybe (UserName, Maybe HtPasswdHash)
parseLine line = case break (==':') line of

  -- entries like "myName:$apr1$r31.....$HqJZimcKQFAMYayBlzkrA/"
  -- this is a special Apache md5-based format that we do not handle
  (user@(_:_), ':' :'$':_) -> Just (UserName user, Nothing)

  -- entries like "myName:rqXexS6ZhobKA"
  (user@(_:_), ':' : hash) -> Just (UserName user, Just (HtPasswdHash hash))
  _                        -> Nothing
