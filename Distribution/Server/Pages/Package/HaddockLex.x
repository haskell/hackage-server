--
-- Haddock - A Haskell Documentation Tool
--
-- (c) Simon Marlow 2002
--

{
{-# LANGUAGE BangPatterns #-}
-- Disable warnings that the generated code causes
{-# OPTIONS_GHC -fno-warn-deprecated-flags
                -fno-warn-unused-binds
                -fno-warn-unused-imports
                -fno-warn-unused-matches
                -fno-warn-missing-signatures
                -fno-warn-tabs #-}
module Distribution.Server.Pages.Package.HaddockLex (
        Token(..),
        tokenise
 ) where

import Data.Char
import Data.Word (Word8)
import qualified Data.Bits
import Numeric
import Control.Monad (liftM)
import Distribution.Server.Pages.Package.HaddockTypes (RdrName)
}

$ws    = $white # \n
$digit = [0-9]
$hexdigit = [0-9a-fA-F]
$special =  [\"\@]
$alphanum = [A-Za-z0-9]
$ident    = [$alphanum \'\_\.\!\#\$\%\&\*\+\/\<\=\>\?\@\\\\\^\|\-\~\:]

:-

-- beginning of a paragraph
<0,para> {
 $ws* \n                ;
 $ws* \>                { begin birdtrack }
 $ws* [\*\-]            { token TokBullet `andBegin` string }
 $ws* \[                { token TokDefStart `andBegin` def }
 $ws* \( $digit+ \)     { token TokNumber `andBegin` string }
 $ws*                   { begin string }
}

-- beginning of a line
<line> {
  $ws* \>               { begin birdtrack }
  $ws* \n               { token TokPara `andBegin` para }
  -- Here, we really want to be able to say
  -- $ws* (\n | <eof>)  { token TokPara `andBegin` para}
  -- because otherwise a trailing line of whitespace will result in
  -- a spurious TokString at the end of a docstring.  We don't have <eof>,
  -- though (NOW I realise what it was for :-).  To get around this, we always
  -- append \n to the end of a docstring.
  ()                    { begin string }
}

<birdtrack> .*  \n?     { strtokenNL TokBirdTrack `andBegin` line }

<string,def> {
  $special                      { strtoken $ \s -> TokSpecial (head s) }
  \<\< [^\<\>]* \>\>            { strtoken $ \s -> TokPic (init $ init $ tail $ tail s) }
  \< [^\<\>]* \>                { strtoken $ \s -> TokURL (init (tail s)) }
  \# [^\#]* \#                  { strtoken $ \s -> TokAName (init (tail s)) }
  \/ [^\/]* \/                  { strtoken $ \s -> TokEmphasis (init (tail s)) }
  [\'\`] $ident+ [\'\`]         { ident }
  \\ .                          { strtoken (TokString . tail) }
  "&#" $digit+ \;               { strtoken $ \s -> TokString [chr (read (init (drop 2 s)))] }
  "&#" [xX] $hexdigit+ \;       { strtoken $ \s -> case readHex (init (drop 3 s)) of [(n,_)] -> TokString [chr n]; _ -> error "hexParser: Can't happen" }
  -- allow special characters through if they don't fit one of the previous
  -- patterns.
  [\/\'\`\<\#\&\\]                      { strtoken TokString }
  [^ $special \/ \< \# \n \'\` \& \\ \]]* \n { strtokenNL TokString `andBegin` line }
  [^ $special \/ \< \# \n \'\` \& \\ \]]+    { strtoken TokString }
}

<def> {
  \]                            { token TokDefEnd `andBegin` string }
}

-- ']' doesn't have any special meaning outside of the [...] at the beginning
-- of a definition paragraph.
<string> {
  \]                            { strtoken TokString }
}

{
data Token
  = TokPara
  | TokNumber
  | TokBullet
  | TokDefStart
  | TokDefEnd
  | TokSpecial Char
  | TokIdent RdrName
  | TokString String
  | TokURL String
  | TokPic String
  | TokEmphasis String
  | TokAName String
  | TokBirdTrack String
  deriving Show

-- -----------------------------------------------------------------------------
-- Alex support stuff

type StartCode = Int
type Action = String -> StartCode -> (StartCode -> Maybe [Token]) -> Maybe [Token]

--TODO: we ought to switch to ByteString input.
type AlexInput = (Char, [Word8], String)

-- | For alex >= 3
--
-- See also alexGetChar
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (c,(b:bs),s) = Just (b,(c,bs,s))
alexGetByte (c,[],[])    = Nothing
alexGetByte (_,[],(c:s)) = case utf8Encode c of
                             (b:bs) -> Just (b, (c, bs, s))

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

-- | For alex < 3
--
-- See also alexGetByte
alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (_, _, [])   = Nothing
alexGetChar (_, _, c:cs) = Just (c, (c,[],cs))

alexInputPrevChar (c,_) = c

tokenise :: String -> Maybe [Token]
tokenise str =
    go ('\n', [], eofHack str) para
  where
    go inp@(_,_,str') sc =
          case alexScan inp sc of
                AlexEOF -> Just []
                AlexError _ -> Nothing
                AlexSkip  inp' _       -> go inp' sc
                AlexToken inp' len act -> act (take len str') sc (\sc' -> go inp' sc')

-- NB. we add a final \n to the string, (see comment in the beginning of line
-- production above).
eofHack str = str++"\n"

andBegin  :: Action -> StartCode -> Action
andBegin act new_sc = \str _ cont -> act str new_sc cont

token :: Token -> Action
token t = \_ sc cont -> liftM (t :) (cont sc)

strtoken, strtokenNL :: (String -> Token) -> Action
strtoken t = \str sc cont -> liftM (t str :) (cont sc)
strtokenNL t = \str sc cont -> liftM (t (filter (/= '\r') str) :) (cont sc)
-- ^ We only want LF line endings in our internal doc string format, so we
-- filter out all CRs.

begin :: StartCode -> Action
begin sc = \_ _ cont -> cont sc

-- -----------------------------------------------------------------------------
-- Lex a string as a Haskell identifier

ident :: Action
ident str sc cont = liftM (TokIdent str :) (cont sc)
}
