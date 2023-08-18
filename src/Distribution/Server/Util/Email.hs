{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Server.Util.Email
  ( EmailContent(..)
  , emailContentStr
  , emailContentLBS
  , emailContentDisplay
  , emailContentIntercalate
  , emailContentUrl

  -- * Rendering email content
  , fromEmailContent
  , toPlainContent
  , toHtmlContent
  ) where

import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.List (intersperse)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL
import Distribution.Pretty (Pretty)
import Distribution.Text (display)
import Network.Mail.Mime
import Network.URI (URI, uriToString)

{- $setup
>>> :set -XOverloadedStrings
>>> import qualified Data.Text.IO as Text
>>> import Network.URI (parseURI)
-}

data EmailContent
  = EmailContentText Text
  | EmailContentLink Text URI
  | EmailContentSoftBreak
  | EmailContentParagraph EmailContent
  | EmailContentList [EmailContent]
  | EmailContentConcat EmailContent EmailContent
  deriving (Show)

instance IsString EmailContent where
  fromString = EmailContentText . Text.pack

instance Semigroup EmailContent where
  (<>) = EmailContentConcat

instance Monoid EmailContent where
  mempty = EmailContentText ""

emailContentStr :: String -> EmailContent
emailContentStr = EmailContentText . Text.pack

emailContentLBS :: Lazy.ByteString -> EmailContent
emailContentLBS = EmailContentText . TextL.toStrict . TextL.decodeUtf8

emailContentDisplay :: Pretty a => a -> EmailContent
emailContentDisplay = EmailContentText . Text.pack . display

emailContentIntercalate :: EmailContent -> [EmailContent] -> EmailContent
emailContentIntercalate x = mconcat . intersperse x

emailContentUrl :: URI -> EmailContent
emailContentUrl uri = EmailContentLink (uriToText uri) uri

fromEmailContent :: EmailContent -> Alternatives
fromEmailContent emailContent =
  [ plainPart $ TextL.fromStrict $ toPlainContent emailContent
  , htmlPart $ TextL.fromStrict $ toHtmlContent emailContent
  ]

-- | Convert an 'EmailContent' to plain text.
--
-- >>> let Just haskellURI = parseURI "https://haskell.org"
-- >>> let Just hackageURI = parseURI "https://hackage.haskell.org"
-- >>> :{
--   Text.putStr . toPlainContent . mconcat $
--     [ EmailContentParagraph "Haskell is fun!"
--     , EmailContentList
--         [ "Website: " <> EmailContentLink "haskell.org" haskellURI
--         , EmailContentLink "Hackage" hackageURI
--         ]
--     ]
-- :}
-- Haskell is fun!
-- <BLANKLINE>
-- * Website: haskell.org (https://haskell.org)
-- * Hackage (https://hackage.haskell.org)
-- <BLANKLINE>
toPlainContent :: EmailContent -> Text
toPlainContent = \case
  EmailContentText s -> s
  EmailContentLink s uri -> s <> " (" <> uriToText uri <> ")"
  EmailContentSoftBreak -> "\n"
  EmailContentParagraph content -> toPlainContent content <> "\n\n"
  EmailContentList items ->
    let renderListItem item = "* " <> toPlainContent item
    in Text.intercalate "\n" (map renderListItem items) <> "\n\n"
  EmailContentConcat a b -> toPlainContent a <> toPlainContent b

-- | Convert an 'EmailContent' to HTML.
--
-- >>> let Just haskellURI = parseURI "https://haskell.org"
-- >>> let Just hackageURI = parseURI "https://hackage.haskell.org"
-- >>> :{
--   Text.putStr . toHtmlContent . mconcat $
--     [ EmailContentParagraph "Haskell is fun!"
--     , EmailContentList
--         [ "Website: " <> EmailContentLink "haskell.org" haskellURI
--         , EmailContentLink "Hackage" hackageURI
--         ]
--     ]
-- :}
-- <BLANKLINE>
-- <p>
-- Haskell is fun!
-- </p>
-- <ul>
--   <li>Website: <a href="https://haskell.org">haskell.org</a></li>
--   <li><a href="https://hackage.haskell.org">Hackage</a></li>
-- </ul>
toHtmlContent :: EmailContent -> Text
toHtmlContent = \case
  EmailContentText s -> s
  EmailContentLink s uri -> "<a href=\"" <> uriToText uri <> "\">" <> s <> "</a>"
  EmailContentSoftBreak -> "\n<br />"
  EmailContentParagraph content -> "\n<p>\n" <> toHtmlContent content <> "\n</p>"
  EmailContentList items ->
    let renderListItem item = "  <li>" <> toHtmlContent item <> "</li>"
    in "\n<ul>\n" <> Text.unlines (map renderListItem items) <> "</ul>"
  EmailContentConcat a b -> toHtmlContent a <> toHtmlContent b

uriToText :: URI -> Text
uriToText uri = Text.pack $ uriToString id uri ""
