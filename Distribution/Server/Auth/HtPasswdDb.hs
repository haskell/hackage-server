module Distribution.Server.Auth.HtPasswdDb (
    HtPasswdDb,
    parse,
  ) where

import Distribution.Server.Users.Types
         ( UserName(..), PasswdHash(..) )

-- Parsing .htpasswd files
type HtPasswdDb = [(UserName, PasswdHash)]

parse :: String -> Either String HtPasswdDb
parse = accum 0 [] . map parseLine . lines
  where
    accum _ pairs []               = Right pairs
    accum n pairs (Just pair:rest) = accum (n+1) (pair:pairs) rest
    accum n _     (Nothing  :_   ) = Left errmsg
      where errmsg = "parse error in htpasswd file on line " ++ show (n :: Int)

parseLine :: String -> Maybe (UserName, PasswdHash)
parseLine line = case break (==':') line of
  (user@(_:_), ':' : hash) -> Just (UserName user, PasswdHash hash)
  _                        -> Nothing
