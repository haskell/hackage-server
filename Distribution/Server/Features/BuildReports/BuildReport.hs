{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards,
             TemplateHaskell, TypeFamilies #-}
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
-- Anonymous build report data structure, printing and parsing
--
-----------------------------------------------------------------------------
module Distribution.Server.Features.BuildReports.BuildReport (
    BuildReport(..),
    InstallOutcome(..),
    Outcome(..),

    -- * parsing and pretty printing
    read,
    parse,
    parseList,
    show,
    showList,

    BuildReport_v1
  ) where

import Distribution.Package
         ( PackageIdentifier(..) )
import Distribution.PackageDescription
         ( FlagName(..), FlagAssignment )
--import Distribution.Version
--         ( Version )
import Distribution.System
         ( OS, Arch )
import Distribution.Compiler
         ( CompilerId )
import qualified Distribution.Text as Text
         ( Text(disp, parse), display )
import Distribution.ParseUtils
         ( FieldDescr(..), ParseResult(..), Field(..)
         , simpleField, boolField, listField, readFields
         , syntaxError, locatedErrorMsg, showFields )
import Distribution.Simple.Utils
         ( comparing )
import Distribution.Server.Util.Merge
import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint.HughesPJ as Disp
         ( Doc, char, text )
import Text.PrettyPrint.HughesPJ
         ( (<+>), (<>), render )
import Data.SafeCopy
         ( deriveSafeCopy, extension, base, Migrate(..) )
import Test.QuickCheck
         ( Arbitrary(..), elements, oneof )
import Text.StringTemplate ()
import Text.StringTemplate.Classes
         ( SElem(..), ToSElem(..) )

import Data.Foldable
         ( foldMap )
import Data.List
         ( unfoldr, sortBy )
import Data.Char as Char
         ( isAlpha, isAlphaNum )
import qualified Data.Map as Map
import Data.Time
         ( UTCTime )
import Data.Typeable
         ( Typeable )
import Control.Applicative
import Control.Monad

import Prelude hiding (show, read)


data BuildReport
   = BuildReport {
    -- | The package this build report is about
    package         :: PackageIdentifier,

    -- | The time at which the report was uploaded
    time            :: Maybe UTCTime,

    -- | Whether the client was generating documentation for upload
    docBuilder      :: Bool,

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
  deriving (Eq, Typeable, Show)

data InstallOutcome
   = PlanningFailed
   | DependencyFailed PackageIdentifier
   | DownloadFailed
   | UnpackFailed
   | SetupFailed
   | ConfigureFailed
   | BuildFailed
   | InstallFailed
   | InstallOk
   deriving (Eq, Show)

data Outcome = NotTried | Failed | Ok deriving (Eq, Show)

-- ------------------------------------------------------------
-- * External format
-- ------------------------------------------------------------

initialBuildReport :: BuildReport
initialBuildReport = BuildReport {
    package         = requiredField "package",
    time            = Nothing,
    docBuilder      = False,
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

requiredFields :: [String]
requiredFields
    = ["package", "os", "arch", "compiler", "client", "install-outcome"]

-- -----------------------------------------------------------------------------
-- Parsing

read :: String -> BuildReport
read s = case parse s of
  Left  err -> error $ "error parsing build report: " ++ err
  Right rpt -> rpt

parse :: String -> Either String BuildReport
parse s = case parseFields s of
  ParseFailed perror -> Left msg where (_, msg) = locatedErrorMsg perror
  ParseOk   _ report -> Right report

parseFields :: String -> ParseResult BuildReport
parseFields input = do
  fields <- mapM extractField =<< readFields input
  let merged = mergeBy (\desc (_,name,_) -> compare (fieldName desc) name)
                       sortedFieldDescrs
                       (sortBy (comparing (\(_,name,_) -> name)) fields)
  foldM checkMerged initialBuildReport merged

  where
    extractField :: Field -> ParseResult (Int, String, String)
    extractField (F line name value)  = return (line, name, value)
    extractField (Section line _ _ _) = syntaxError line "Unrecognized stanza"
    extractField (IfBlock line _ _ _) = syntaxError line "Unrecognized stanza"

    checkMerged report merged = case merged of
      InBoth fieldDescr (line, _name, value) ->
        fieldSet fieldDescr line value report
      OnlyInRight (line, name, _) ->
        syntaxError line ("Unrecognized field " ++ name)
      OnlyInLeft  fieldDescr
        | fieldName fieldDescr `elem` requiredFields ->
            fail ("Missing field " ++ fieldName fieldDescr)
        | otherwise -> return report

parseList :: String -> [BuildReport]
parseList str =
  [ report | Right report <- map parse (split str) ]

  where
    split :: String -> [String]
    split = filter (not . null) . unfoldr chunk . lines
    chunk [] = Nothing
    chunk ls = case break null ls of
                 (r, rs) -> Just (unlines r, dropWhile null rs)

-- -----------------------------------------------------------------------------
-- Pretty-printing

show :: BuildReport -> String
show = showFields fieldDescrs

-- -----------------------------------------------------------------------------
-- Description of the fields, for parsing/printing

fieldDescrs :: [FieldDescr BuildReport]
fieldDescrs =
 [ simpleField "package"         Text.disp      Text.parse
                                 package        (\v r -> r { package = v })
 , simpleField "time"            dispTime       parseTime
                                 time           (\v r -> r { time = v })
 , boolField   "doc-builder"     docBuilder     (\v r -> r { docBuilder = v })
 , simpleField "os"              Text.disp      Text.parse
                                 os             (\v r -> r { os = v })
 , simpleField "arch"            Text.disp      Text.parse
                                 arch           (\v r -> r { arch = v })
 , simpleField "compiler"        Text.disp      Text.parse
                                 compiler       (\v r -> r { compiler = v })
 , simpleField "client"          Text.disp      Text.parse
                                 client         (\v r -> r { client = v })
 , listField   "flags"           dispFlag       parseFlag
                                 flagAssignment (\v r -> r { flagAssignment = v })
 , listField   "dependencies"    Text.disp      Text.parse
                                 dependencies   (\v r -> r { dependencies = v })
 , simpleField "install-outcome" Text.disp      Text.parse
                                 installOutcome (\v r -> r { installOutcome = v })
 , simpleField "docs-outcome"    Text.disp      Text.parse
                                 docsOutcome    (\v r -> r { docsOutcome = v })
 , simpleField "tests-outcome"   Text.disp      Text.parse
                                 testsOutcome   (\v r -> r { testsOutcome = v })
 ]
  where
    dispTime = foldMap Text.disp
    parseTime = (Just <$> Text.parse) Parse.<++ pure Nothing

sortedFieldDescrs :: [FieldDescr BuildReport]
sortedFieldDescrs = sortBy (comparing fieldName) fieldDescrs

dispFlag :: (FlagName, Bool) -> Disp.Doc
dispFlag (FlagName name, True)  =                  Disp.text name
dispFlag (FlagName name, False) = Disp.char '-' <> Disp.text name

parseFlag :: Parse.ReadP r (FlagName, Bool)
parseFlag = do
  name <- Parse.munch1 (\c -> Char.isAlphaNum c || c == '_' || c == '-')
  case name of
    ('-':flag) -> return (FlagName flag, False)
    flag       -> return (FlagName flag, True)

instance Text.Text InstallOutcome where
  disp PlanningFailed  = Disp.text "PlanningFailed"
  disp (DependencyFailed pkgid) = Disp.text "DependencyFailed" <+> Text.disp pkgid
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
      "PlanningFailed"   -> return PlanningFailed
      "DependencyFailed" -> do Parse.skipSpaces
                               pkgid <- Text.parse
                               return (DependencyFailed pkgid)
      "DownloadFailed"   -> return DownloadFailed
      "UnpackFailed"     -> return UnpackFailed
      "SetupFailed"      -> return SetupFailed
      "ConfigureFailed"  -> return ConfigureFailed
      "BuildFailed"      -> return BuildFailed
      "InstallFailed"    -> return InstallFailed
      "InstallOk"        -> return InstallOk
      _                  -> Parse.pfail

instance Text.Text Outcome where
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

instance MemSize BuildReport where
    memSize (BuildReport a b c d e f g h i j k l) = memSize10 a b c d e f g h i j + 2 + memSize k + memSize l

instance MemSize InstallOutcome where
    memSize (DependencyFailed a) = memSize1 a
    memSize _                    = memSize0

instance MemSize Outcome where
    memSize _ = memSize0

-------------------
-- HStringTemplate instances
--

instance ToSElem BuildReport where
    toSElem BuildReport{..} = SM . Map.fromList $
        [ ("package", display package)
        , ("time", toSElem time)
        , ("docBuilder", toSElem docBuilder)
        , ("os", display os)
        , ("arch", display arch)
        , ("compiler", display compiler)
        , ("client", display client)
        , ("flagAssignment", toSElem $ map (render . dispFlag) flagAssignment)
        , ("dependencies", toSElem $ map Text.display dependencies)
        , ("installOutcome", display installOutcome)
        , ("docsOutcome", display docsOutcome)
        , ("testsOutcome", display testsOutcome)
        ]
      where
        display value = toSElem (Text.display value)

-------------------
-- Arbitrary instances
--

instance Arbitrary BuildReport where
  arbitrary = BuildReport <$> arbitrary <*> arbitrary <*> arbitrary
                          <*> arbitrary <*> arbitrary <*> arbitrary
                          <*> arbitrary <*> arbitrary <*> arbitrary
                          <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary InstallOutcome where
  arbitrary = oneof [ pure PlanningFailed
                    , pure DependencyFailed <*> arbitrary
                    , pure DownloadFailed
                    , pure UnpackFailed
                    , pure SetupFailed
                    , pure ConfigureFailed
                    , pure BuildFailed
                    , pure InstallFailed
                    , pure InstallOk
                    ]

instance Arbitrary Outcome where
  arbitrary = elements [ NotTried, Failed, Ok ]


-------------------
-- SafeCopy instances
--

deriveSafeCopy 0 'base      ''Outcome
deriveSafeCopy 1 'extension ''InstallOutcome
deriveSafeCopy 3 'extension ''BuildReport


-------------------
-- Old SafeCopy versions
--

data BuildReport_v1 = BuildReport_v1 {
    v1_package         :: PackageIdentifier,
    v1_os              :: OS,
    v1_arch            :: Arch,
    v1_compiler        :: CompilerId,
    v1_client          :: PackageIdentifier,
    v1_flagAssignment  :: FlagAssignment,
    v1_dependencies    :: [PackageIdentifier],
    v1_installOutcome  :: InstallOutcome_v1,
    v1_docsOutcome     :: Outcome,
    v1_testsOutcome    :: Outcome
  }

data InstallOutcome_v1
    = V1_DependencyFailed PackageIdentifier
    | V1_DownloadFailed
    | V1_UnpackFailed
    | V1_SetupFailed
    | V1_ConfigureFailed
    | V1_BuildFailed
    | V1_InstallFailed
    | V1_InstallOk

deriveSafeCopy 0 'base ''InstallOutcome_v1
deriveSafeCopy 2 'base ''BuildReport_v1

instance Migrate BuildReport where
    type MigrateFrom BuildReport = BuildReport_v1
    migrate BuildReport_v1{..} = BuildReport {
        package = v1_package
      , time = Nothing
      , docBuilder = True  -- Most reports come from the doc builder anyway
      , os = v1_os
      , arch = v1_arch
      , compiler = v1_compiler
      , client = v1_client
      , flagAssignment = v1_flagAssignment
      , dependencies = v1_dependencies
      , installOutcome = migrate v1_installOutcome
      , docsOutcome = v1_docsOutcome
      , testsOutcome = v1_testsOutcome
      }

instance Migrate InstallOutcome where
    type MigrateFrom InstallOutcome = InstallOutcome_v1
    migrate outcome = case outcome of
        V1_DependencyFailed pkgid -> DependencyFailed pkgid
        V1_DownloadFailed -> DownloadFailed
        V1_UnpackFailed -> UnpackFailed
        V1_SetupFailed -> SetupFailed
        V1_ConfigureFailed -> ConfigureFailed
        V1_BuildFailed -> BuildFailed
        V1_InstallFailed -> InstallFailed
        V1_InstallOk -> InstallOk
