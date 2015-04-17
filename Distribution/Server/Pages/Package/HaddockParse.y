{
-- Disable warnings that the generated code causes
{-# OPTIONS_GHC -fno-warn-deprecated-flags
                -fno-warn-missing-signatures
                -fno-warn-unused-binds
                -fno-warn-unused-matches
                -fno-warn-name-shadowing
                -fno-warn-incomplete-patterns
                -fno-warn-tabs #-}
module Distribution.Server.Pages.Package.HaddockParse (parseHaddockParagraphs) where

import Distribution.Server.Pages.Package.HaddockLex
import Distribution.Server.Pages.Package.HaddockHtml
import Distribution.Server.Pages.Package.HaddockTypes
import Data.Char  (isSpace)
}

%expect 0

%tokentype { Token }

%token
        '@'     { TokSpecial '@' }
        '['     { TokDefStart }
        ']'     { TokDefEnd }
        DQUO    { TokSpecial '\"' }
        URL     { TokURL $$ }
        PIC     { TokPic $$ }
        ANAME   { TokAName $$ }
        '/../'  { TokEmphasis $$ }
        '-'     { TokBullet }
        '(n)'   { TokNumber }
        '>..'   { TokBirdTrack $$ }
        IDENT   { TokIdent $$ }
        PARA    { TokPara }
        STRING  { TokString $$ }

%monad { Maybe }

%name parseHaddockParagraphs  doc
%name parseHaddockString seq

%%

doc     :: { Doc RdrName }
        : apara PARA doc        { docAppend $1 $3 }
        | PARA doc              { $2 }
        | apara                 { $1 }
        | {- empty -}           { DocEmpty }

apara   :: { Doc RdrName }
        : ulpara                { DocUnorderedList [$1] }
        | olpara                { DocOrderedList [$1] }
        | defpara               { DocDefList [$1] }
        | para                  { $1 }

ulpara  :: { Doc RdrName }
        : '-' para              { $2 }

olpara  :: { Doc RdrName }
        : '(n)' para            { $2 }

defpara :: { (Doc RdrName, Doc RdrName) }
        : '[' seq ']' seq       { ($2, $4) }

para    :: { Doc RdrName }
        : seq                   { docParagraph $1 }
        | codepara              { DocCodeBlock $1 }

codepara :: { Doc RdrName }
        : '>..' codepara        { docAppend (DocString $1) $2 }
        | '>..'                 { DocString $1 }

seq     :: { Doc RdrName }
        : elem seq              { docAppend $1 $2 }
        | elem                  { $1 }

elem    :: { Doc RdrName }
        : elem1                 { $1 }
        | '@' seq1 '@'          { DocMonospaced $2 }

seq1    :: { Doc RdrName }
        : PARA seq1             { docAppend (DocString "\n") $2 }
        | elem1 seq1            { docAppend $1 $2 }
        | elem1                 { $1 }

elem1   :: { Doc RdrName }
        : STRING                { DocString $1 }
        | '/../'                { DocEmphasis (DocString $1) }
        | URL                   { DocHyperlink (makeHyperlink $1) }
        | PIC                   { DocPic $1 }
        | ANAME                 { DocAName $1 }
        | IDENT                 { DocIdentifier $1 }
        | DQUO strings DQUO     { DocModule $2 }

strings  :: { String }
        : STRING                { $1 }
        | STRING strings        { $1 ++ $2 }

{
happyError :: [Token] -> Maybe a
happyError toks = Nothing

-- | Create a `Hyperlink` from given string.
--
-- A hyperlink consists of a URL and an optional label.  The label is separated
-- from the url by one or more whitespace characters.
makeHyperlink :: String -> Hyperlink
makeHyperlink input = case break isSpace $ strip input of
  (url, "")    -> Hyperlink url Nothing
  (url, label) -> Hyperlink url (Just . dropWhile isSpace $ label)

-- | Remove all leading and trailing whitespace
strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
}
