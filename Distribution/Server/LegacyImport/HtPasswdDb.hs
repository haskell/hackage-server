-- | Parsing @.htpasswd@ files
--
module Distribution.Server.LegacyImport.HtPasswdDb (
    HtPasswdDb,
    parse,
  ) where

import Distribution.Server.Users.Types (UserName(..))

type HtPasswdDb = [(UserName, HtPasswdHash)]

-- | These are the crypt format password hashes. Not the same as what hackage stores.
--
newtype HtPasswdHash = HtPasswdHash String
  deriving (Eq, Show)

parse :: String -> Either String HtPasswdDb
parse = accum 0 [] . map parseLine . lines
  where
    accum _ pairs []               = Right pairs
    accum n pairs (Just pair:rest) = accum (n+1) (pair:pairs) rest
    accum n _     (Nothing  :_   ) = Left errmsg
      where errmsg = "parse error in htpasswd file on line " ++ show (n :: Int)

parseLine :: String -> Maybe (UserName, HtPasswdHash)
parseLine line = case break (==':') line of
  (user@(_:_), ':' : hash) -> Just (UserName user, HtPasswdHash hash)
  _                        -> Nothing
