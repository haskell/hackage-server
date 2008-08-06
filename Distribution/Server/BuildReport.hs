-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Reporting
-- Copyright   :  (c) David Waern 2008
-- License     :  BSD-like
--
-- Maintainer  :  david.waern@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Report data structure
--
-----------------------------------------------------------------------------
module Distribution.Server.BuildReport (
    BuildReport(..),
    InstallOutcome(..),
    Outcome(..),

    -- * parsing and pretty printing
    parseBuildReport,
    readBuildReport,
    parseBuildReports,
    showBuildReport,
  ) where

import Distribution.Package
         ( PackageIdentifier )
import Distribution.PackageDescription
         ( FlagName(..), FlagAssignment )
--import Distribution.Version
--         ( Version )
import Distribution.System
         ( OS, Arch )
import Distribution.Compiler
         ( CompilerId )
import Distribution.Text
         ( Text(disp, parse) )
import Distribution.ParseUtils
         ( FieldDescr(..), Field(F), simpleField, listField
         , ParseResult(..), readFields, ppFields
         , warning, lineNo, locatedErrorMsg )
import qualified Distribution.Compat.ReadP as Parse
         ( ReadP, pfail, munch1, char, option, skipSpaces )
import qualified Text.PrettyPrint.HughesPJ as Disp
         ( Doc, render, char, text )
import Text.PrettyPrint.HughesPJ
         ( (<+>), (<>) )

import Data.List
         ( unfoldr )
import Control.Monad
         ( foldM )
import Data.Char as Char
         ( isAlpha, isAlphaNum )
import qualified Data.Map as Map

data BuildReport
   = BuildReport {
    -- | The package this build report is about
    package         :: PackageIdentifier,

    -- | The OS and Arch the package was built on
    os              :: OS,
    arch            :: Arch,

    -- | The Haskell compiler (and hopefully version) used
    compiler        :: CompilerId,

    -- | The uploading client, ie cabal-install-x.y.z
    client          :: PackageIdentifier,

    -- | Which configurations flags we used
    flagAssignment  :: FlagAssignment,

    -- | Which dependent packages we were using exactly
    dependencies    :: [PackageIdentifier],

    -- | Did installing work ok?
    installOutcome  :: InstallOutcome,

    --   Which version of the Cabal library was used to compile the Setup.hs
--    cabalVersion    :: Version,

    --   Which build tools we were using (with versions)
--    tools      :: [PackageIdentifier],

    -- | Configure outcome, did configure work ok?
    docsOutcome     :: Outcome,

    -- | Configure outcome, did configure work ok?
    testsOutcome    :: Outcome
  }

data InstallOutcome
   = DependencyFailed PackageIdentifier
   | DownloadFailed
   | UnpackFailed
   | SetupFailed
   | ConfigureFailed
   | BuildFailed
   | InstallFailed
   | InstallOk

data Outcome = NotTried | Failed | Ok

-- ------------------------------------------------------------
-- * External format
-- ------------------------------------------------------------

initialBuildReport :: BuildReport
initialBuildReport = BuildReport {
    package         = requiredField "package",
    os              = requiredField "os",
    arch            = requiredField "arch",
    compiler        = requiredField "compiler",
    client          = requiredField "client",
    flagAssignment  = [],
    dependencies    = [],
    installOutcome  = requiredField "install-outcome",
--    cabalVersion  = Nothing,
--    tools         = [],
    docsOutcome     = NotTried,
    testsOutcome    = NotTried
  }
  where
    requiredField fname = error ("required field: " ++ fname)

-- -----------------------------------------------------------------------------
-- Parsing

readBuildReport :: String -> BuildReport
readBuildReport s = case parseBuildReport s of
  Left  err -> error $ "error parsing build report: " ++ err
  Right rpt -> rpt

parseBuildReport :: String -> Either String BuildReport
parseBuildReport str = case parseFields fieldDescrs initialBuildReport str of
  ParseFailed err -> Left  msg where (_, msg) = locatedErrorMsg err
  ParseOk   _ rpt -> Right rpt

--FIXME: this function is now in Cabal as of 1.5, so remove this local copy
parseFields :: [FieldDescr a] -> a -> String -> ParseResult a
parseFields fields initial = \str ->
  readFields str >>= foldM setField initial
  where
    fieldMap = Map.fromList
      [ (name, f) | f@(FieldDescr name _ _) <- fields ]
    setField accum (F line name value) = case Map.lookup name fieldMap of
      Just (FieldDescr _ _ set) -> set line value accum
      Nothing -> do
        warning $ "Unrecognized field " ++ name ++ " on line " ++ show line
        return accum
    setField accum f = do
        warning $ "Unrecognized stanza on line " ++ show (lineNo f)
        return accum

parseBuildReports :: String -> [BuildReport]
parseBuildReports str =
  [ report | Right report <- map parseBuildReport (split str) ]

  where
    split :: String -> [String]
    split = filter (not . null) . unfoldr chunk . lines
    chunk [] = Nothing
    chunk ls = case break null ls of
                 (r, rs) -> Just (unlines r, dropWhile null rs)

-- -----------------------------------------------------------------------------
-- Pretty-printing

showBuildReport :: BuildReport -> String
showBuildReport = showFields fieldDescrs

--FIXME: this function is now in Cabal as of 1.5, so remove this local copy
showFields :: [FieldDescr a] -> a -> String
showFields fields = Disp.render . flip ppFields fields

-- -----------------------------------------------------------------------------
-- Description of the fields, for parsing/printing

fieldDescrs :: [FieldDescr BuildReport]
fieldDescrs =
 [ simpleField "package"         disp           parse
                                 package        (\v r -> r { package = v })
 , simpleField "os"              disp           parse
                                 os             (\v r -> r { os = v })
 , simpleField "arch"            disp           parse
                                 arch           (\v r -> r { arch = v })
 , simpleField "compiler"        disp           parse
                                 compiler       (\v r -> r { compiler = v })
 , simpleField "client"          disp           parse
                                 client         (\v r -> r { client = v })
 , listField   "flags"           dispFlag       parseFlag
                                 flagAssignment (\v r -> r { flagAssignment = v })
 , listField   "dependencies"    disp           parse
                                 dependencies   (\v r -> r { dependencies = v })
 , simpleField "install-outcome" disp           parse
                                 installOutcome (\v r -> r { installOutcome = v })
 , simpleField "docs-outcome"    disp           parse
                                 docsOutcome    (\v r -> r { docsOutcome = v })
 , simpleField "tests-outcome"   disp           parse
                                 testsOutcome   (\v r -> r { testsOutcome = v })
 ]

dispFlag :: (FlagName, Bool) -> Disp.Doc
dispFlag (FlagName name, True)  =                  Disp.text name
dispFlag (FlagName name, False) = Disp.char '-' <> Disp.text name

parseFlag :: Parse.ReadP r (FlagName, Bool)
parseFlag = do
  value <- Parse.option True (Parse.char '-' >> return False)
  name  <- Parse.munch1 (\c -> Char.isAlphaNum c || c == '_' || c == '-')
  return (FlagName name, value)

instance Text InstallOutcome where
  disp (DependencyFailed pkgid) = Disp.text "DependencyFailed" <+> disp pkgid
  disp DownloadFailed  = Disp.text "DownloadFailed"
  disp UnpackFailed    = Disp.text "UnpackFailed"
  disp SetupFailed     = Disp.text "SetupFailed"
  disp ConfigureFailed = Disp.text "ConfigureFailed"
  disp BuildFailed     = Disp.text "BuildFailed"
  disp InstallFailed   = Disp.text "InstallFailed"
  disp InstallOk       = Disp.text "InstallOk"

  parse = do
    name <- Parse.munch1 Char.isAlphaNum
    case name of
      "DependencyFailed" -> do Parse.skipSpaces
                               pkgid <- parse
                               return (DependencyFailed pkgid)
      "DownloadFailed"   -> return DownloadFailed
      "UnpackFailed"     -> return UnpackFailed
      "SetupFailed"      -> return SetupFailed
      "ConfigureFailed"  -> return ConfigureFailed
      "BuildFailed"      -> return BuildFailed
      "InstallFailed"    -> return InstallFailed
      "InstallOk"        -> return InstallOk
      _                  -> Parse.pfail

instance Text Outcome where
  disp NotTried = Disp.text "NotTried"
  disp Failed   = Disp.text "Failed"
  disp Ok       = Disp.text "Ok"
  parse = do
    name <- Parse.munch1 Char.isAlpha
    case name of
      "NotTried" -> return NotTried
      "Failed"   -> return Failed
      "Ok"       -> return Ok
      _          -> Parse.pfail
