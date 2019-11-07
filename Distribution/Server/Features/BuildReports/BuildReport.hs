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

    affixTimestamp,

    BuildReport_v0,
  ) where

import Distribution.Package
         ( PackageIdentifier(..) )
import Distribution.Types.GenericPackageDescription
         ( FlagName, mkFlagName, unFlagName )
import Distribution.System
         ( OS, Arch )
import Distribution.Compiler
         ( CompilerId )
import qualified Distribution.Text as Text
         ( display )
import Distribution.Pretty (Pretty(..))
import Distribution.Parsec (Parsec(..))
import qualified Distribution.Compat.CharParsing as P
import Distribution.Simple.Utils
         ( comparing )
import Distribution.Server.Util.Merge
import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

import qualified Text.ParserCombinators.ReadP as Parse
import qualified Text.PrettyPrint.HughesPJ as Disp
         ( Doc, char, text, (<>) )
import Text.PrettyPrint.HughesPJ
         ( (<+>), render )
import Data.Serialize as Serialize
         ( Serialize(..) )
import Data.SafeCopy
         ( SafeCopy(..), deriveSafeCopy, extension, base, Migrate(..) )
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
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Time
         ( UTCTime, getCurrentTime )
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
    flagAssignment  :: [(FlagName,Bool)],
    -- TODO: this is the pre-Cabal-2.2 'FlagAssignment' type;
    --       consider changing this to the new opaque 'FlagAssignment' type at some point
    --       (which will have implications for the safecopy serialisation)

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
   deriving (Eq, Ord, Show)

data Outcome = NotTried | Failed | Ok deriving (Eq, Ord, Show)

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
-- Timestamps

-- | If the 'time' field is empty, fill it in with the current time.
affixTimestamp :: BuildReport -> IO BuildReport
affixTimestamp report = case time report of
    Nothing -> (\v -> report { time = Just v }) <$> getCurrentTime
    Just _ -> return report

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
dispFlag (fn, True)  =                       Disp.text (unFlagName fn)
dispFlag (fn, False) = Disp.char '-' Disp.<> Disp.text (unFlagName fn)

parseFlag :: Parse.ReadP r (FlagName, Bool)
parseFlag = do
  name <- Parse.munch1 (\c -> Char.isAlphaNum c || c == '_' || c == '-')
  case name of
    ('-':flag) -> return (mkFlagName flag, False)
    flag       -> return (mkFlagName flag, True)

instance Pretty InstallOutcome where
  pretty PlanningFailed  = Disp.text "PlanningFailed"
  pretty (DependencyFailed pkgid) = Disp.text "DependencyFailed" <+> pretty pkgid
  pretty DownloadFailed  = Disp.text "DownloadFailed"
  pretty UnpackFailed    = Disp.text "UnpackFailed"
  pretty SetupFailed     = Disp.text "SetupFailed"
  pretty ConfigureFailed = Disp.text "ConfigureFailed"
  pretty BuildFailed     = Disp.text "BuildFailed"
  pretty InstallFailed   = Disp.text "InstallFailed"
  pretty InstallOk       = Disp.text "InstallOk"

instance Parsec InstallOutcome where
  parsec = do
    name <- P.munch1 Char.isAlphaNum
    case name of
      "PlanningFailed"   -> return PlanningFailed
      "DependencyFailed" -> do P.spaces
                               pkgid <- parsec
                               return (DependencyFailed pkgid)
      "DownloadFailed"   -> return DownloadFailed
      "UnpackFailed"     -> return UnpackFailed
      "SetupFailed"      -> return SetupFailed
      "ConfigureFailed"  -> return ConfigureFailed
      "BuildFailed"      -> return BuildFailed
      "InstallFailed"    -> return InstallFailed
      "InstallOk"        -> return InstallOk
      _                  -> fail "unknown InstallOutcome"

instance Pretty Outcome where
  pretty NotTried = Disp.text "NotTried"
  pretty Failed   = Disp.text "Failed"
  pretty Ok       = Disp.text "Ok"

instance Parsec Outcome where
  parsec = do
    name <- P.munch1 Char.isAlpha
    case name of
      "NotTried" -> return NotTried
      "Failed"   -> return Failed
      "Ok"       -> return Ok
      _          -> fail "unknown Outcome"

instance MemSize BuildReport where
    memSize (BuildReport a b c d e f g h i j k l) = memSize10 a b c d e f g h i j + memSize k + memSize l

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

-- Note this is kind of backwards from a migration pov, but this is because
-- the oldest one used a textual external rep with a parser, and the only
-- version we have with a parser is the latest, but they're sufficiently
-- compatible that we can get away with it for now.
newtype BuildReport_v0 = BuildReport_v0 BuildReport

instance SafeCopy  BuildReport_v0
instance Serialize BuildReport_v0 where
    put (BuildReport_v0 br) = Serialize.put . BS.pack . show $ br
    get = (BuildReport_v0 . read . BS.unpack) `fmap` Serialize.get

instance Migrate BuildReport_v1 where
    type MigrateFrom BuildReport_v1 = BuildReport_v0
    migrate (BuildReport_v0 BuildReport{..}) = BuildReport_v1 {
        v1_package = package
      , v1_os = os
      , v1_arch = arch
      , v1_compiler = compiler
      , v1_client = client
      , v1_flagAssignment = flagAssignment
      , v1_dependencies = dependencies
      , v1_installOutcome = case installOutcome of
          PlanningFailed         -> error "impossible rev migration"
          DependencyFailed pkgid -> V0_DependencyFailed pkgid
          DownloadFailed         -> V0_DownloadFailed
          UnpackFailed           -> V0_UnpackFailed
          SetupFailed            -> V0_SetupFailed
          ConfigureFailed        -> V0_ConfigureFailed
          BuildFailed            -> V0_BuildFailed
          InstallFailed          -> V0_InstallFailed
          InstallOk              -> V0_InstallOk
      , v1_docsOutcome = docsOutcome
      , v1_testsOutcome = testsOutcome
      }

data BuildReport_v1 = BuildReport_v1 {
    v1_package         :: PackageIdentifier,
    v1_os              :: OS,
    v1_arch            :: Arch,
    v1_compiler        :: CompilerId,
    v1_client          :: PackageIdentifier,
    v1_flagAssignment  :: [(FlagName,Bool)],
    v1_dependencies    :: [PackageIdentifier],
    v1_installOutcome  :: InstallOutcome_v0,
    v1_docsOutcome     :: Outcome,
    v1_testsOutcome    :: Outcome
  }

data InstallOutcome_v0
   = V0_DependencyFailed PackageIdentifier
   | V0_DownloadFailed
   | V0_UnpackFailed
   | V0_SetupFailed
   | V0_ConfigureFailed
   | V0_BuildFailed
   | V0_InstallFailed
   | V0_InstallOk

deriveSafeCopy 0 'base      ''InstallOutcome_v0
deriveSafeCopy 2 'extension ''BuildReport_v1

instance Migrate BuildReport where
    type MigrateFrom BuildReport = BuildReport_v1
    migrate BuildReport_v1{..} = BuildReport {
        package = v1_package
      , time = Nothing
      , docBuilder = True  -- Most old reports come from the doc builder anyway
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
    type MigrateFrom InstallOutcome = InstallOutcome_v0
    migrate outcome = case outcome of
        V0_DependencyFailed pkgid -> DependencyFailed pkgid
        V0_DownloadFailed -> DownloadFailed
        V0_UnpackFailed -> UnpackFailed
        V0_SetupFailed -> SetupFailed
        V0_ConfigureFailed -> ConfigureFailed
        V0_BuildFailed -> BuildFailed
        V0_InstallFailed -> InstallFailed
        V0_InstallOk -> InstallOk
