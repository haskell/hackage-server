{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Distribution.Server.Util.Markdown
  ( renderMarkdown
  , renderMarkdownRel
  , supposedToBeMarkdown
  ) where

import Commonmark
import Commonmark.Extensions
import Commonmark.Extensions.Footnote()
import Commonmark.Extensions.Math()
import qualified Data.Text as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T (lenientDecode)
import qualified Data.Text.Lazy as TL
import Data.Typeable (Typeable)
import Network.URI (isRelativeReference)
import Control.Monad.Identity
import Text.HTML.SanitizeXSS as XSS
import System.FilePath.Posix  (takeExtension)
import qualified Data.ByteString.Lazy as BS (ByteString, toStrict)
import qualified Text.XHtml.Strict as XHtml

-- Set up doctest to deal with bytestring literals.
-- Need to import a suitable @instance IsString@ for @ByteString@.

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.ByteString.Lazy.Char8 ()

-- HHtml wraps Html, and mostly behaves the same, except that
-- relative links in images and urls have "src/" prepended.
newtype HHtml a = HHtml { unHHtml :: Html a }
  deriving (Show, Semigroup, Monoid)

instance HasAttributes (HHtml a) where
  addAttributes attrs (HHtml x) = HHtml (addAttributes attrs x)

instance ToPlainText (HHtml a) where
  toPlainText (HHtml x) = toPlainText x

instance Rangeable (Html a) => Rangeable (HHtml a) where
  ranged sr (HHtml x) = (HHtml $ ranged sr x)

instance (Rangeable (Html a), Rangeable (HHtml a)) => IsInline (HHtml a) where
  lineBreak = HHtml lineBreak
  softBreak = HHtml softBreak
  str t = HHtml (str t)
  entity t = HHtml (entity t)
  escapedChar c = HHtml (escapedChar c)
  emph ils = HHtml (emph $ unHHtml ils)
  strong ils = HHtml (strong $ unHHtml ils)
  link target title ils = HHtml $
    link (adjustRelativeLink target) title (unHHtml ils)
  image target title ils = HHtml $
    image (adjustRelativeLink target) title (unHHtml ils)
  code t = HHtml $ code t
  rawInline f t = HHtml (rawInline f t)

instance (Rangeable (Html a), IsInline (HHtml a))
     => IsBlock (HHtml a) (HHtml a) where
  paragraph ils = HHtml $ paragraph $ unHHtml ils
  plain ils = HHtml $ plain $ unHHtml ils
  thematicBreak = HHtml thematicBreak
  blockQuote bs = HHtml $ blockQuote $ unHHtml bs
  codeBlock info t = HHtml $ codeBlock info t
  heading level ils = HHtml $ heading level $ unHHtml ils
  rawBlock f t = HHtml $ rawBlock f t
  referenceLinkDefinition x y = HHtml $ referenceLinkDefinition x y
  list lType lSpacing items =
    HHtml $ list lType lSpacing $ map unHHtml items

instance HasEmoji (HHtml a) where
  emoji kw cs = HHtml $ emoji kw cs

instance HasStrikethrough (HHtml a) where
  strikethrough ils = HHtml $ strikethrough $ unHHtml ils

instance HasPipeTable (HHtml a) (HHtml a) where
  pipeTable aligns heads rows =
    HHtml $ pipeTable aligns (map unHHtml heads) (map (map unHHtml) rows)

instance (Rangeable (Html a), Rangeable (HHtml a))
         => HasTaskList (HHtml a) (HHtml a) where
  taskList ltype lspacing items =
    HHtml $ taskList ltype lspacing
          $ map (\(done, bl) -> (done, unHHtml bl)) items

instance HasMath (HHtml a) where
  inlineMath t = HHtml $ inlineMath t
  displayMath t = HHtml $ displayMath t

instance Rangeable (Html a) => HasFootnote (HHtml a) (HHtml a) where
  footnote x y (HHtml t) = HHtml (footnote x y t)
  footnoteList xs = HHtml $ footnoteList (map unHHtml xs)
  footnoteRef x y (HHtml t) = HHtml (footnoteRef x y t)

-- | Prefix relative links with @src/@.
adjustRelativeLink :: T.Text -> T.Text
adjustRelativeLink url
  | isRelativeReference (T.unpack url) &&
    not ("/" `T.isPrefixOf` url)
              = "src/" <> url
  | otherwise = url

-- | Render markdown to HTML.
--
-- >>> renderMarkdown "test" "Please send bug reports to hackage-server@gmail.com."
-- <p>Please send bug reports to <a href="mailto:hackage-server@gmail.com">hackage-server@gmail.com</a>.</p>
-- <BLANKLINE>
--
-- >>> renderMarkdown "test" "Published to http://hackage.haskell.org/foo3/bar."
-- <p>Published to <a href="http://hackage.haskell.org/foo3/bar">http://hackage.haskell.org/foo3/bar</a>.</p>
-- <BLANKLINE>
--
renderMarkdown
  :: String         -- ^ Name or path of input.
  -> BS.ByteString  -- ^ Commonmark text input.
  -> XHtml.Html     -- ^ Rendered HTML.
renderMarkdown = renderMarkdown' (renderHtml :: Html () -> TL.Text)

-- | Render markdown to HTML, prefixing relative links with @src/@.
--
-- >>> renderMarkdownRel "test" "See [world file](world.txt)."
-- <p>See <a href="src/world.txt">world file</a>.</p>
-- <BLANKLINE>
--
renderMarkdownRel
  :: String         -- ^ Name or path of input.
  -> BS.ByteString  -- ^ Commonmark text input.
  -> XHtml.Html     -- ^ Rendered HTML.
renderMarkdownRel = renderMarkdown' (renderHtml . unHHtml :: HHtml () -> TL.Text)

-- | Prerequisites for 'commonmarkWith' with 'gfmExtensions' and 'mathSpec'.
type MarkdownRenderable a =
  ( Typeable a
  , HasEmoji a
  , HasFootnote a a
  , HasMath a
  , HasPipeTable a a
  , HasStrikethrough a
  , HasTaskList a a
  , IsBlock a a
  , IsInline a
  , ToPlainText a
  )

-- | Generic gfm markdown rendering.
renderMarkdown'
  :: MarkdownRenderable a
  => (a -> TL.Text)  -- ^ HTML rendering function.
  -> String          -- ^ Name or path of input.
  -> BS.ByteString   -- ^ Commonmark text input.
  -> XHtml.Html      -- ^ Rendered HTML.
renderMarkdown' render name md =
     either (const $ XHtml.pre XHtml.<< T.unpack txt) (XHtml.primHtml . T.unpack . sanitizeBalance . TL.toStrict . render) $
         runIdentity (commonmarkWith (mathSpec <> gfmExtensions <> defaultSyntaxSpec)
                     name
                     txt)
  where txt = T.decodeUtf8With T.lenientDecode . BS.toStrict $ md

-- | Does the file extension suggest that the file is in markdown syntax?
supposedToBeMarkdown :: FilePath -> Bool
supposedToBeMarkdown fname = takeExtension fname `elem` [".md", ".markdown"]
