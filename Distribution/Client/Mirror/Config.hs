{-# OPTIONS -fno-warn-missing-signatures #-}
module Distribution.Client.Mirror.Config (
    MirrorConfig(..)
  , PreRepo(..)
  , readMirrorConfig
  ) where

-- stdlib
import Distribution.Server.Prelude

import Data.Char (isSpace)
import Network.URI (URI)
import Text.Parsec
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import qualified Network.URI       as URI

-- hackage-security
import qualified Hackage.Security.Client as Sec

{-------------------------------------------------------------------------------
  Configuration data types
-------------------------------------------------------------------------------}

data MirrorConfig = MirrorConfig {
    mirrorSource   :: PreRepo
  , mirrorTarget   :: PreRepo
  , mirrorPostHook :: Maybe String
  }
  deriving Show

data PreRepo = PreRepo {
      preRepoName      :: String
    , preRepoURI       :: Maybe URI
    , preRepoType      :: Maybe String
    , preRepoThreshold :: Maybe Sec.KeyThreshold
    , preRepoKeys      :: Maybe [Sec.KeyId]
    }
  deriving Show

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

readMirrorConfig :: FilePath -> IO (Either String MirrorConfig)
readMirrorConfig = liftM (either (Left . show) Right)
                 . parseFromFile (whiteSpace *> parseMirrorConfig <* eof)

parseMirrorConfig :: Parser MirrorConfig
parseMirrorConfig = MirrorConfig
    <$> parsePreRepo "source"
    <*> parsePreRepo "target"
    <*> (optionMaybe $ reserved "post-mirror-hook" *> reservedOp ":" *> parseArg)

parsePreRepo :: String -> Parser PreRepo
parsePreRepo sourceOrTarget = do
     reserved sourceOrTarget
     repoName <- parseArg
     fields   <- many1 parsePreRepoField

     let emptyRepo = PreRepo {
             preRepoName      = repoName
           , preRepoURI       = Nothing
           , preRepoType      = Nothing
           , preRepoThreshold = Nothing
           , preRepoKeys      = Nothing
           }

     return $ foldr ($) emptyRepo fields

parsePreRepoField :: Parser (PreRepo -> PreRepo)
parsePreRepoField = choice [
      field "uri"       parseURI           $ \x r -> r {preRepoURI       = Just x}
    , field "type"      parseArg           $ \x r -> r {preRepoType      = Just x}
    , field "threshold" parseThreshold     $ \x r -> r {preRepoThreshold = Just x}
    , field "keys"      (many1 parseKeyId) $ \x r -> r {preRepoKeys      = Just x}
    ]
  where
     field :: String
           -> Parser a
           -> (a -> PreRepo -> PreRepo)
           -> Parser (PreRepo -> PreRepo)
     field nm p f = f <$> (reserved nm *> reservedOp ":" *> p)

{-------------------------------------------------------------------------------
  Auxiliary parsec definitions
-------------------------------------------------------------------------------}

parseURI :: Parser URI
parseURI = aux =<< parseArg
  where
     aux :: String -> Parser URI
     aux str = case URI.parseURI str of
                 Nothing  -> fail $ "Invalid URI: " ++ show str
                 Just uri -> return uri

-- | Parse generic argument: either string without spaces or quoted string
parseArg :: Parser String
parseArg = lexeme (quoted Text.Parsec.<|> noSpaces)
  where
    noSpaces, quoted :: Parser String
    noSpaces = many1 (satisfy (not . isSpace))
    quoted   = char '"' *> many1 (satisfy (/= '"')) <* char '"'

parseThreshold :: Parser Sec.KeyThreshold
parseThreshold = (Sec.KeyThreshold . fromInteger) <$> integer

parseKeyId :: Parser Sec.KeyId
parseKeyId = Sec.KeyId <$> parseArg

{-------------------------------------------------------------------------------
  Lexer
-------------------------------------------------------------------------------}

lexer = P.makeTokenParser haskellStyle {
            P.reservedOpNames = [":"]
          , P.reservedNames   = [ "hackage2"
                                , "local"
                                , "post-mirror-hook"
                                , "root"
                                , "source"
                                , "target"
                                , "type"
                                , "url"
                                ]
          }

lexeme     = P.lexeme     lexer
reserved   = P.reserved   lexer
reservedOp = P.reservedOp lexer
whiteSpace = P.whiteSpace lexer
integer    = P.integer    lexer
