module Distribution.Server.Auth.HtPasswdDb (
    HtPasswdDb,
    passwdCheck,
    parse,
  ) where

import Distribution.Server.Auth.Types
import Distribution.Server.Users.Types
import qualified Distribution.Server.Auth.Crypt as Crypt

import qualified Data.Map as Map

newtype HtPasswdDb = HtPasswdDb (Map.Map UserName PasswdHash)

passwdCheck :: HtPasswdDb -> UserName -> PasswdPlain -> Bool
passwdCheck (HtPasswdDb authMap) username passwd =
  case Map.lookup username authMap of
    Nothing   -> False
    Just hash -> Crypt.checkPasswd passwd hash

parse :: String -> Either String HtPasswdDb
parse = accum 0 [] . map parseLine . lines
  where
    accum _ pairs []               = Right (HtPasswdDb (Map.fromList pairs))
    accum n pairs (Just pair:rest) = accum (n+1) (pair:pairs) rest
    accum n _     (Nothing  :_   ) = Left errmsg
      where errmsg = "parse error in htpasswd file on line " ++ show (n :: Int)

parseLine :: String -> Maybe (UserName, PasswdHash)
parseLine line = case break (==':') line of
  (user@(_:_), ':' : hash) -> Just (UserName user, PasswdHash hash)
  _                        -> Nothing
