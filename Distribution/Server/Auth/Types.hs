module Distribution.Server.Auth.Types where

newtype UserName    = UserName String deriving (Eq, Ord, Show)
newtype PasswdPlain = PasswdPlain String deriving (Eq, Ord, Show)
newtype PasswdHash  = PasswdHash  String deriving (Eq, Ord, Show)

-- | Does the given user really have this as their password?
--
type PasswdCheck    = UserName     -- ^ user name
                   -> PasswdPlain  -- ^ plain text password
                   -> Bool
