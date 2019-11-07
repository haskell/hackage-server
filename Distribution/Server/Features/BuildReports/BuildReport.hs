{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards,
             TemplateHaskell, TypeFamilies, FlexibleInstances, MultiParamTypeClasses,
             OverloadedStrings #-}
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

import Distribution.Compat.Newtype
import Distribution.Compat.Lens (Lens')
import Distribution.Package
         ( PackageIdentifier(..) )
import Distribution.Types.GenericPackageDescription
         ( FlagName, unFlagName )
import Distribution.System
         ( OS, Arch )
import Distribution.Compiler
         ( CompilerId )
import Distribution.CabalSpecVersion
         ( CabalSpecVersion(CabalSpecV2_4) )
import qualified Distribution.Pretty as Pretty
import qualified Text.PrettyPrint as Pretty
import Distribution.Parsec.Newtypes
import qualified Distribution.Parsec as Parsec
import qualified Distribution.Compat.CharParsing as Parsec
import Distribution.FieldGrammar
         ( FieldGrammar, parseFieldGrammar, prettyFieldGrammar, partitionFields
         , uniqueField, uniqueFieldAla, booleanFieldDef, monoidalFieldAla )
import Distribution.Fields.Parser
         ( readFields )
import Distribution.Fields.Pretty
         ( showFields )
import Distribution.Fields.ParseResult
         (ParseResult, parseFatalFailure, runParseResult )
import Distribution.Server.Framework.Instances ()
import Distribution.Server.Framework.MemSize

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

import Data.List
         ( unfoldr )
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
import qualified Prelude


data BuildReport
   = BuildReport {
    -- | The package this build report is about
    _package         :: PackageIdentifier,

    -- | The time at which the report was uploaded
    _time            :: Maybe UTCTime,

    -- | Whether the client was generating documentation for upload
    _docBuilder      :: Bool,

    -- | The OS and Arch the package was built on
    _os              :: OS,
    _arch            :: Arch,

    -- | The Haskell compiler (and hopefully version) used
    _compiler        :: CompilerId,

    -- | The uploading client, ie cabal-install-x.y.z
    _client          :: PackageIdentifier,

    -- | Which configurations flags we used
    _flagAssignment  :: [(FlagName,Bool)],
    -- TODO: this is the pre-Cabal-2.2 'FlagAssignment' type;
    --       consider changing this to the new opaque 'FlagAssignment' type at some point
    --       (which will have implications for the safecopy serialisation)

    -- | Which dependent packages we were using exactly
    _dependencies    :: [PackageIdentifier],

    -- | Did installing work ok?
    _installOutcome  :: InstallOutcome,

    --   Which version of the Cabal library was used to compile the Setup.hs
--    _cabalVersion    :: Version,

    --   Which build tools we were using (with versions)
--    _tools      :: [PackageIdentifier],

    -- | Configure outcome, did configure work ok?
    _docsOutcome     :: Outcome,

    -- | Configure outcome, did configure work ok?
    _testsOutcome    :: Outcome
  }
  deriving (Eq, Typeable, Show)

package :: Lens' BuildReport PackageIdentifier
package f s = fmap (\x -> s { _package = x }) (f (_package s))

time :: Lens' BuildReport (Maybe UTCTime)
time f s = fmap (\x -> s { _time = x }) (f (_time s))

docBuilder :: Lens' BuildReport Bool
docBuilder f s = fmap (\x -> s { _docBuilder = x }) (f (_docBuilder s))

os :: Lens' BuildReport OS
os f s = fmap (\x -> s { _os = x }) (f (_os s))

arch :: Lens' BuildReport Arch
arch f s = fmap (\x -> s { _arch = x }) (f (_arch s))

compiler :: Lens' BuildReport CompilerId
compiler f s = fmap (\x -> s { _compiler = x }) (f (_compiler s))

client :: Lens' BuildReport PackageIdentifier
client f s = fmap (\x -> s { _client = x }) (f (_client s))

flagAssignment :: Lens' BuildReport [(FlagName,Bool)]
flagAssignment f s = fmap (\x -> s { _flagAssignment = x }) (f (_flagAssignment s))

dependencies :: Lens' BuildReport [PackageIdentifier]
dependencies f s = fmap (\x -> s { _dependencies = x }) (f (_dependencies s))

installOutcome :: Lens' BuildReport InstallOutcome
installOutcome f s = fmap (\x -> s { _installOutcome = x }) (f (_installOutcome s))

docsOutcome :: Lens' BuildReport Outcome
docsOutcome f s = fmap (\x -> s { _docsOutcome = x }) (f (_docsOutcome s))

testsOutcome :: Lens' BuildReport Outcome
testsOutcome f s = fmap (\x -> s { _testsOutcome = x }) (f (_testsOutcome s))

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

-- -----------------------------------------------------------------------------
-- Timestamps

-- | If the 'time' field is empty, fill it in with the current time.
affixTimestamp :: BuildReport -> IO BuildReport
affixTimestamp report = case _time report of
    Nothing -> (\v -> report { _time = Just v }) <$> getCurrentTime
    Just _ -> return report

-- -----------------------------------------------------------------------------
-- Parsing

read :: BS.ByteString -> BuildReport
read s = case parse s of
  Left  err -> error $ "error parsing build report: " ++ err
  Right rpt -> rpt

parse :: BS.ByteString -> Either String BuildReport
parse s = case snd $ runParseResult $ parseFields s of
  Left (_, perrors) -> Left $ unlines [ err | Parsec.PError _ err <- perrors ]
  Right report -> Right report

parseFields :: BS.ByteString -> ParseResult BuildReport
parseFields input = do
  fields <- either (parseFatalFailure Parsec.zeroPos . Prelude.show) pure $ readFields input
  case partitionFields fields of
    (fields', [])  -> parseFieldGrammar CabalSpecV2_4 fields' fieldDescrs
    _otherwise -> fail "found sections in BuildReport"

parseList :: BS.ByteString -> [BuildReport]
parseList str =
  [ report | Right report <- map parse (split str) ]

  where
    split :: BS.ByteString -> [BS.ByteString]
    split = filter (not . BS.null) . unfoldr chunk . BS.lines
    chunk [] = Nothing
    chunk ls = case break BS.null ls of
                 (r, rs) -> Just (BS.unlines r, dropWhile BS.null rs)

-- -----------------------------------------------------------------------------
-- Pretty-printing

show :: BuildReport -> String
show = showFields (const []) . prettyFieldGrammar CabalSpecV2_4 fieldDescrs

-- -----------------------------------------------------------------------------
-- Description of the fields, for parsing/printing

newtype Time = Time (Maybe UTCTime)

instance Newtype (Maybe UTCTime) Time
instance Pretty.Pretty Time -- TODO
instance Parsec.Parsec Time -- TODO

fieldDescrs :: (Applicative (g BuildReport), FieldGrammar g) => g BuildReport BuildReport
fieldDescrs =
  BuildReport
    <$> uniqueField       "package"            package
    <*> uniqueFieldAla    "time"      (pack' Time)     time
    <*> booleanFieldDef   "doc-builder"        docBuilder  False
    <*> uniqueField       "os"                 os
    <*> uniqueField       "arch"               arch
    <*> uniqueField       "compiler"           compiler
    <*> uniqueField       "client"             client
    <*> undefined --monoidalFieldAla  "flags"              (alaList CommaFSep) flagAssignment TODO
    <*> monoidalFieldAla  "dependencies"       (alaList VCat)   dependencies
    <*> uniqueField       "install-outcome"    installOutcome
    <*> uniqueField       "docs-outcome"       docsOutcome
    <*> uniqueField       "tests-outcome"      testsOutcome

dispFlag :: (FlagName, Bool) -> Pretty.Doc
dispFlag (fn, True)  =                           Pretty.text (unFlagName fn)
dispFlag (fn, False) = Pretty.char '-' Pretty.<> Pretty.text (unFlagName fn)

instance Pretty.Pretty InstallOutcome where
  pretty PlanningFailed  = Pretty.text "PlanningFailed"
  pretty (DependencyFailed pkgid) = Pretty.text "DependencyFailed" <+> Pretty.pretty pkgid
  pretty DownloadFailed  = Pretty.text "DownloadFailed"
  pretty UnpackFailed    = Pretty.text "UnpackFailed"
  pretty SetupFailed     = Pretty.text "SetupFailed"
  pretty ConfigureFailed = Pretty.text "ConfigureFailed"
  pretty BuildFailed     = Pretty.text "BuildFailed"
  pretty InstallFailed   = Pretty.text "InstallFailed"
  pretty InstallOk       = Pretty.text "InstallOk"

instance Parsec.Parsec InstallOutcome where
  parsec = do
    name <- Parsec.munch1 Char.isAlphaNum
    case name of
      "PlanningFailed"   -> return PlanningFailed
      "DependencyFailed" -> do Parsec.spaces
                               pkgid <- Parsec.parsec
                               return (DependencyFailed pkgid)
      "DownloadFailed"   -> return DownloadFailed
      "UnpackFailed"     -> return UnpackFailed
      "SetupFailed"      -> return SetupFailed
      "ConfigureFailed"  -> return ConfigureFailed
      "BuildFailed"      -> return BuildFailed
      "InstallFailed"    -> return InstallFailed
      "InstallOk"        -> return InstallOk
      _                  -> fail "unknown InstallOutcome"

instance Pretty.Pretty Outcome where
  pretty NotTried = Pretty.text "NotTried"
  pretty Failed   = Pretty.text "Failed"
  pretty Ok       = Pretty.text "Ok"

instance Parsec.Parsec Outcome where
  parsec = do
    name <- Parsec.munch1 Char.isAlpha
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
        [ ("package", display _package)
        , ("time", toSElem _time)
        , ("docBuilder", toSElem _docBuilder)
        , ("os", display _os)
        , ("arch", display _arch)
        , ("compiler", display _compiler)
        , ("client", display _client)
        , ("flagAssignment", toSElem $ map (render . dispFlag) _flagAssignment)
        , ("dependencies", toSElem $ map Pretty.prettyShow _dependencies)
        , ("installOutcome", display _installOutcome)
        , ("docsOutcome", display _docsOutcome)
        , ("testsOutcome", display _testsOutcome)
        ]
      where
        display value = toSElem (Pretty.prettyShow value)

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
    get = (BuildReport_v0 . read) `fmap` Serialize.get

instance Migrate BuildReport_v1 where
    type MigrateFrom BuildReport_v1 = BuildReport_v0
    migrate (BuildReport_v0 BuildReport{..}) = BuildReport_v1 {
        v1_package = _package
      , v1_os = _os
      , v1_arch = _arch
      , v1_compiler = _compiler
      , v1_client = _client
      , v1_flagAssignment = _flagAssignment
      , v1_dependencies = _dependencies
      , v1_installOutcome = case _installOutcome of
          PlanningFailed         -> error "impossible rev migration"
          DependencyFailed pkgid -> V0_DependencyFailed pkgid
          DownloadFailed         -> V0_DownloadFailed
          UnpackFailed           -> V0_UnpackFailed
          SetupFailed            -> V0_SetupFailed
          ConfigureFailed        -> V0_ConfigureFailed
          BuildFailed            -> V0_BuildFailed
          InstallFailed          -> V0_InstallFailed
          InstallOk              -> V0_InstallOk
      , v1_docsOutcome = _docsOutcome
      , v1_testsOutcome = _testsOutcome
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
        _package = v1_package
      , _time = Nothing
      , _docBuilder = True  -- Most old reports come from the doc builder anyway
      , _os = v1_os
      , _arch = v1_arch
      , _compiler = v1_compiler
      , _client = v1_client
      , _flagAssignment = v1_flagAssignment
      , _dependencies = v1_dependencies
      , _installOutcome = migrate v1_installOutcome
      , _docsOutcome = v1_docsOutcome
      , _testsOutcome = v1_testsOutcome
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
