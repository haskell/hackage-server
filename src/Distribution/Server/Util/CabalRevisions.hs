{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      :  Distribution.Server.Util.CabalRevisions
-- Copyright   :  Duncan Coutts et al.
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Validation and helpers for Cabal revision handling
module Distribution.Server.Util.CabalRevisions
    ( diffCabalRevisions
    , diffCabalRevisions'
    , Change(..)
    , insertRevisionField
    ) where

-- NB: This module avoids to import any hackage-server modules
import Distribution.CabalSpecVersion (CabalSpecVersion(..), cabalSpecLatest, showCabalSpecVersion)
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.PkgconfigDependency
import Distribution.Types.PkgconfigVersionRange
import Distribution.Types.LegacyExeDependency
import Distribution.Types.UnqualComponentName
import Distribution.Types.CondTree
import Distribution.Types.ForeignLib
import Distribution.Package
import Distribution.Pretty (Pretty (..), prettyShow)
import Distribution.Version
import Distribution.Compiler (CompilerFlavor)
import Distribution.FieldGrammar (prettyFieldGrammar)
import Distribution.Fields.Pretty (PrettyField (..), showFields)
#if MIN_VERSION_Cabal(3,7,0)
import Distribution.Fields.Pretty (pattern NoComment)
#endif
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.PackageDescription.FieldGrammar (sourceRepoFieldGrammar)
import Distribution.PackageDescription.Check
import Distribution.Parsec (showPWarning, showPError, PWarning (..))
import Distribution.Utils.ShortText
import Text.PrettyPrint as Doc
         ((<+>), colon, text, Doc, hsep, punctuate)

import Control.Applicative
import Control.Monad
import Control.Monad.Except  (ExceptT, runExceptT, throwError)
import Control.Monad.Writer (MonadWriter(..), Writer, runWriter)
import Data.Foldable (for_)
import Data.List
         ((\\), deleteBy, intercalate)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Char as Char
import qualified Data.Semigroup as S
import qualified Data.Monoid as M
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(Proxy))

import qualified Control.Monad.Fail as Fail

-- | Entry point to cabal revision validator
--
-- This takes an original and a revised @.cabal@ decoded as Unicode
-- 'String' and performs validations. Returns either a validation
-- error or a list of detected changes.
diffCabalRevisions :: BS.ByteString -> BS.ByteString -> Either String [Change]
diffCabalRevisions = diffCabalRevisions' True

-- | Like 'diffCabalRevisions' but only optionally check @x-revision@ field modifications.
diffCabalRevisions'
    :: Bool                    -- ^ check @x-revision@
    -> BS.ByteString           -- ^ old revision
    -> BS.ByteString           -- ^ new revision
    -> Either String [Change]
diffCabalRevisions' checkXRevision oldVersion newRevision = runCheck $
    checkCabalFileRevision checkXRevision oldVersion newRevision

newtype CheckM a = CheckM { unCheckM :: ExceptT String (Writer [Change]) a }
    deriving (Functor, Applicative)

runCheck :: CheckM () -> Either String [Change]
runCheck c = case runWriter . runExceptT . unCheckM $ c of
               (Left err, _      ) -> Left err
               (Right (), changes)
                 | foldMap changeSeverity changes /= Trivial -> Right changes
                 | otherwise ->
                   Left "Only trivial changes, don't bother making this revision."

changeSeverity :: Change -> Severity
changeSeverity (Change s _ _ _) = s

instance Monad CheckM where
  return         = Control.Applicative.pure
  CheckM m >>= f = CheckM (m >>= unCheckM . f)

#if !MIN_VERSION_base(4,13,0)
  fail = Fail.fail
#endif

instance Fail.MonadFail CheckM where
  fail           = CheckM . throwError

-- | If we have only 'Trivial' changes, then there is no point to make
-- a revision. In other words for changes to be accepted, there should
-- be at least one 'Normal' change.
data Severity
    = Normal
    | Trivial
  deriving (Eq, Ord, Show, Enum, Bounded)

instance S.Semigroup Severity where
    Normal  <> _ = Normal
    Trivial <> x = x

-- | "Max" monoid.
instance M.Monoid Severity where
    mempty = Trivial
    mappend = (S.<>)

data Change = Change Severity String String String -- severity, what, from, to
  deriving Show



logChange :: Change -> CheckM ()
logChange change = CheckM (tell [change])

type Check a = a -> a -> CheckM ()

checkCabalFileRevision :: Bool -> Check BS.ByteString
checkCabalFileRevision checkXRevision old new = do
    (pkg,  warns)  <- parseCabalFile old
    (pkg', warns') <- parseCabalFile new

    let pkgid    = packageId pkg
        filename = prettyShow pkgid ++ ".cabal"

    checkGenericPackageDescription checkXRevision pkg pkg'
    checkParserWarnings filename warns warns'
    checkPackageChecks  pkg   pkg'

  where
    parseCabalFile fileContent =
      case runParseResult $ parseGenericPackageDescription fileContent of
        (warnings,  Right pkg) -> return (pkg, warnings)
        (_warnings, Left (_mver, errs)) -> do
            for_ errs $ \err -> fail (showPError "-" err)
            fail "no better error"

    -- new PWarning isn't Eq
    differenceBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
    differenceBy eq = foldl (flip $ deleteBy eq)

    -- things can move, pos can change
    eqPWarning :: PWarning -> PWarning -> Bool
    eqPWarning (PWarning t _pos s) (PWarning t' _pos' s') =
        t == t' && s == s'

    checkParserWarnings :: FilePath -> Check [PWarning]
    checkParserWarnings filename warns warns' =
      case differenceBy eqPWarning warns' warns of
        []       -> return ()
        newwarns -> fail $ "New parse warning: "
                        ++ unlines (map (showPWarning filename) newwarns)

    checkPackageChecks :: Check GenericPackageDescription
    checkPackageChecks pkg pkg' =
      let checks  = checkPackage pkg  Nothing
          checks' = checkPackage pkg' Nothing
       in case checks' \\ checks of
            []        -> return ()
            newchecks -> fail $ unlines (map ppPackageCheck newchecks)
#if !MIN_VERSION_Cabal(3,9,0)
      where ppPackageCheck = explanation
#endif

checkGenericPackageDescription :: Bool -> Check GenericPackageDescription
checkGenericPackageDescription checkXRevision
    (GenericPackageDescription descrA _versionA flagsA libsA sublibsA flibsA exesA testsA benchsA)
    (GenericPackageDescription descrB _versionB flagsB libsB sublibsB flibsB exesB testsB benchsB) = do

    checkPackageDescriptions checkXRevision descrA descrB

    checkList "Cannot add or remove flags" checkFlag flagsA flagsB

    checkMaybe "Cannot add or remove library sections"
      (checkCondTree checkLibrary)
      (withComponentName' (CLibName LMainLibName) <$> libsA)
      (withComponentName' (CLibName LMainLibName) <$> libsB)

    checkListAssoc "Cannot add or remove sub-library sections"
      (checkCondTree checkLibrary)
      (withComponentName (CLibName . LSubLibName) <$> sublibsA)
      (withComponentName (CLibName . LSubLibName) <$> sublibsB)

    checkListAssoc "Cannot add or remove foreign-library sections"
      (checkCondTree checkForeignLib)
      (withComponentName CFLibName <$> flibsA)
      (withComponentName CFLibName <$> flibsB)

    checkListAssoc "Cannot add or remove executable sections"
      (checkCondTree checkExecutable)
      (withComponentName CExeName <$> exesA)
      (withComponentName CExeName <$> exesB)

    checkListAssoc "Cannot add or remove test-suite sections"
      (checkCondTree checkTestSuite)
      (withComponentName CTestName <$> testsA)
      (withComponentName CTestName <$> testsB)

    checkListAssoc "Cannot add or remove benchmark sections"
      (checkCondTree checkBenchmark)
      (withComponentName CBenchName <$> benchsA)
      (withComponentName CBenchName <$> benchsB)
  where
    withComponentName  f (name, condTree) = (name, (f name, condTree))
    withComponentName' f        condTree  = (f,             condTree)


checkFlag :: Check PackageFlag
checkFlag flagOld flagNew = do
    -- This check is applied via 'checkList' and for simplicity we
    -- disallow renaming/reordering flags (even though reordering
    -- would be fine semantically)
    checkSame "Cannot change ordering of flags"
              (flagName flagOld) (flagName flagNew)

    -- Automatic flags' defaults may be changed as they don't make new
    -- configurations reachable by the solver that weren't before
    --
    -- Moreover, automatic flags may be converted into manual flags
    -- but not the other way round.
    --
    -- NB: We always allow to change the flag description as it has
    --     purely informational value
    when (flagManual flagOld) $ do
        checkSame "Cannot change the default of a manual flag"
                  (flagDefault flagOld) (flagDefault flagNew)

        checkSame "Cannot change a manual flag into an automatic flag"
                  (flagManual flagOld) (flagManual flagNew)

    let fname = unFlagName (flagName flagOld)

    changesOk ("type of flag '" ++ fname ++ "'")
              (\b -> if b then "manual" else "automatic")
              (flagManual flagOld) (flagManual flagNew)

    changesOk ("default of flag '" ++ fname ++ "'") prettyShow
              (flagDefault flagOld) (flagDefault flagNew)

    changesOk ("description of flag '" ++ fname ++ "'") id
              (flagDescription flagOld) (flagDescription flagNew)

checkPackageDescriptions :: Bool -> Check PackageDescription
checkPackageDescriptions checkXRevision
  pdA@(PackageDescription
     { specVersion     = _specVersionA
     , package         = packageIdA
     , licenseRaw      = licenseRawA
     , licenseFiles    = licenseFilesA
     , copyright       = copyrightA
     , maintainer      = maintainerA
     , author          = authorA
     , stability       = stabilityA
     , testedWith      = testedWithA
     , homepage        = homepageA
     , pkgUrl          = pkgUrlA
     , bugReports      = bugReportsA
     , sourceRepos     = sourceReposA
     , synopsis        = synopsisA
     , description     = descriptionA
     , category        = categoryA
     , customFieldsPD  = customFieldsPDA
     , buildTypeRaw    = buildTypeRawA
     , setupBuildInfo  = setupBuildInfoA
     , library         = _libraryA
     , subLibraries    = _subLibrariesA
     , executables     = _executablesA
     , foreignLibs     = _foreignLibsA
     , testSuites      = _testSuitesA
     , benchmarks      = _benchmarksA
     , dataFiles       = dataFilesA
     , dataDir         = dataDirA
     , extraSrcFiles   = extraSrcFilesA
     , extraTmpFiles   = extraTmpFilesA
     , extraDocFiles   = extraDocFilesA
     })
  pdB@(PackageDescription
     { specVersion     = _specVersionB
     , package         = packageIdB
     , licenseRaw      = licenseRawB
     , licenseFiles    = licenseFilesB
     , copyright       = copyrightB
     , maintainer      = maintainerB
     , author          = authorB
     , stability       = stabilityB
     , testedWith      = testedWithB
     , homepage        = homepageB
     , pkgUrl          = pkgUrlB
     , bugReports      = bugReportsB
     , sourceRepos     = sourceReposB
     , synopsis        = synopsisB
     , description     = descriptionB
     , category        = categoryB
     , customFieldsPD  = customFieldsPDB
     , buildTypeRaw    = buildTypeRawB
     , setupBuildInfo  = setupBuildInfoB
     , library         = _libraryB
     , subLibraries    = _subLibrariesB
     , executables     = _executablesB
     , foreignLibs     = _foreignLibsB
     , testSuites      = _testSuitesB
     , benchmarks      = _benchmarksB
     , dataFiles       = dataFilesB
     , dataDir         = dataDirB
     , extraSrcFiles   = extraSrcFilesB
     , extraTmpFiles   = extraTmpFilesB
     , extraDocFiles   = extraDocFilesB
     })
  = do
  checkSame "Don't be silly! You can't change the package name!"
            (packageName packageIdA) (packageName packageIdB)
  checkSame "You can't change the package version!"
            (packageVersion packageIdA) (packageVersion packageIdB)
  checkSame "Cannot change the license"
            (licenseRawA, licenseFilesA) (licenseRawB, licenseFilesB)
  changesOk "copyright"  fromShortText copyrightA copyrightB
  changesOk "maintainer" fromShortText maintainerA maintainerB
  changesOk "author"     fromShortText authorA authorB
  changesOk "stability"  fromShortText stabilityA stabilityB
  changesOk' Trivial "tested-with" (show . ppTestedWith) testedWithA testedWithB
  changesOk "homepage" fromShortText homepageA homepageB
  checkSame "The package-url field is unused, don't bother changing it."
            pkgUrlA pkgUrlB
  changesOk "bug-reports" fromShortText bugReportsA bugReportsB
  changesOkList changesOk "source-repository" (showFields noComment . (:[]) . ppSourceRepo)
            sourceReposA sourceReposB
  changesOk "synopsis"    fromShortText synopsisA synopsisB
  changesOk "description" fromShortText descriptionA descriptionB
  changesOk "category"    fromShortText categoryA categoryB
  checkSame "Cannot change the build-type"
            buildTypeRawA buildTypeRawB
  checkSame "Cannot change the data files"
            (dataFilesA, dataDirA) (dataFilesB, dataDirB)
  checkSame "Changing extra-tmp-files is a bit pointless at this stage"
            extraTmpFilesA extraTmpFilesB
  checkSame "Changing extra-source-files would not make sense!"
            extraSrcFilesA extraSrcFilesB
  checkSame "You can't change the extra-doc-files."
            extraDocFilesA extraDocFilesB

  checkSame "Cannot change custom/extension fields"
            (filter (\(f,_) -> f `notElem` ["x-revision","x-curation"]) customFieldsPDA)
            (filter (\(f,_) -> f `notElem` ["x-revision","x-curation"]) customFieldsPDB)

  checkSpecVersionRaw pdA pdB
  checkSetupBuildInfo setupBuildInfoA setupBuildInfoB

  when checkXRevision $ checkRevision customFieldsPDA customFieldsPDB
  checkCuration customFieldsPDA customFieldsPDB
  where
#if MIN_VERSION_Cabal(3,7,0)
    noComment _ = NoComment
#else
    noComment _ = []
#endif

checkSpecVersionRaw :: Check PackageDescription
checkSpecVersionRaw pdA pdB
  | range110To120 specVersionA
  , range110To120 specVersionB
  = changesOk "cabal-version" showCabalSpecVersion specVersionA specVersionB

  | otherwise
  = checkSame "Cannot change the Cabal spec version"
              specVersionA specVersionB
  where
    specVersionA = specVersion pdA
    specVersionB = specVersion pdB

    -- nothing interesting changed within the  Cabal >=1.10 && <1.21 range
    -- therefore we allow to change the spec version within this interval
    range110To120 v = CabalSpecV1_10 >= v && v <= CabalSpecV1_20

checkRevision :: Check [(String, String)]
checkRevision customFieldsA customFieldsB =
    checkSame ("The new x-revision must be " ++ show expectedRevision)
              newRevision expectedRevision
  where
    oldRevision = getRevision customFieldsA
    newRevision = getRevision customFieldsB
    expectedRevision = oldRevision + 1

    getRevision customFields =
      case lookup "x-revision" customFields of
        Just s  | [(n,"")] <- reads s -> n :: Int
        _                             -> 0

checkCuration :: Check [(String, String)]
checkCuration customFieldsA customFieldsB =
    checkNotPresent "Revised metadata must not contain an x-curation field as revisions necessarily imply curation, and revising an uncurated package adopts it into the curated layer." oldCuration newCuration
  where
    oldCuration = lookup "x-curation" customFieldsA
    newCuration = lookup "x-curation" customFieldsB



checkCondTree :: (ComponentName -> Check a) -> Check (ComponentName, CondTree ConfVar [Dependency] a)
checkCondTree checkElem (componentName, condNodeA)
                        (_            , condNodeB) =
    checkCondNode condNodeA condNodeB
  where
    checkCondNode (CondNode dataA constraintsA componentsA)
                  (CondNode dataB constraintsB componentsB) = do
      checkDependencies componentName constraintsA constraintsB
      checkList "Cannot add or remove 'if' conditionals"
                checkComponent componentsA componentsB
      checkElem componentName dataA dataB

    checkComponent (CondBranch condA ifPartA thenPartA)
                   (CondBranch condB ifPartB thenPartB) = do
      checkSame "Cannot change the 'if' condition expressions"
                condA condB
      checkCondNode ifPartA ifPartB
      checkMaybe "Cannot add or remove the 'else' part in conditionals"
                 checkCondNode thenPartA thenPartB

checkDependencies :: forall d vr. (Pretty d, IsDependency vr d) => ComponentName -> Check [d]
checkDependencies componentName ds1 ds2 = do
    forM_ removed $ \dep -> do
        fail (unwords [ "Cannot remove existing", depKind, "on"
                      , depKeyShow dproxy (depKey dep), "in", cnameStr, " component"])

    forM_ added $ \dep ->
        if depInAddWhitelist dep
           then logChange (Change Normal (unwords ["added the", cnameStr, "component's"
                                                  , depKind, "on"]) "" (prettyShow dep))
           else fail (unwords [ "Cannot add new", depKind, "on"
                              , depKeyShow dproxy (depKey dep)
                              , "in", cnameStr, "component"])

    forM_ changed $ \(depk, (verA, verB)) -> do
        changesOk (unwords ["the", cnameStr, "component's", depKind, "on"
                           , depKeyShow dproxy depk])
                   prettyShow verA verB
  where
    (removed, changed, added) = computeCanonDepChange ds1 ds2

    dproxy :: Proxy d
    dproxy = Proxy

    cnameStr = showComponentName componentName

    depKind = depTypeName dproxy ++ " dependency"

class (Ord (DepKey d), Pretty vr, Eq vr) => IsDependency vr d | d -> vr where
    type DepKey d

    depTypeName    :: Proxy d -> String
    depKey         :: d -> DepKey d
    depKeyShow     :: Proxy d -> DepKey d -> String
    depVerRg       :: d -> vr
    reconstructDep :: DepKey d -> vr -> d

    depInAddWhitelist :: d -> Bool
    depInAddWhitelist _ = False

    intersectVr :: Proxy d -> vr -> vr -> vr

instance IsDependency VersionRange Dependency where
    type DepKey Dependency = PackageName

    depTypeName Proxy             = "library"
    depKey (Dependency pkgname _ _) = pkgname
    depKeyShow Proxy              = prettyShow''
    depVerRg (Dependency _ vr _)  = vr
    reconstructDep                = \n vr -> Dependency n vr mainLibSet

    depInAddWhitelist (Dependency pn _ _) = pn `elem`
    -- Special case: there are some pretty weird broken packages out there, see
    --   https://github.com/haskell/hackage-server/issues/303
    -- which need us to add a new dep on `base`
            [ mkPackageName "base"

    -- See also https://github.com/haskell/hackage-server/issues/472
    --
    -- this is mostly to allow to add dependencies on `base-orphans == 0`
    -- as otherwise we have no way to express when a package is
    -- incompatible with the recently introduced `base-orphans` package
    -- which started adopting orphan instances; in the long-term we need a
    -- more general approach to this, as otherwise we'll end up adding
    -- ad-hoc exceptions like this one. See e.g.
    --   https://github.com/haskell/cabal/issues/3061
    --
            , mkPackageName "base-orphans"
            ]

    intersectVr _ = intersectVersionRanges


instance IsDependency VersionRange ExeDependency where
    type DepKey ExeDependency = (PackageName,UnqualComponentName)

    depTypeName Proxy                   = "tool"
    depKey (ExeDependency pkgname cn _) = (pkgname,cn)
    depKeyShow Proxy (pkgname,cn)       = concat ["'", prettyShow pkgname, ":", prettyShow cn, "'"]
    depVerRg (ExeDependency _ _ vr)     = vr
    reconstructDep (pkgname,cn)         = ExeDependency pkgname cn

    intersectVr _ = intersectVersionRanges

instance IsDependency VersionRange LegacyExeDependency where
    type DepKey LegacyExeDependency = String

    depTypeName Proxy                      = "legacy-tool"
    depKey (LegacyExeDependency tname _)   = tname
    depKeyShow Proxy tname                 = "'" ++ tname ++ "'"
    depVerRg (LegacyExeDependency _ vr)    = vr
    reconstructDep                         = LegacyExeDependency

    intersectVr _ = intersectVersionRanges

    depInAddWhitelist (LegacyExeDependency pn _) = pn `elem`
    -- list of trusted tools cabal supports w/o explicit build-tools
    -- c.f. Distribution.Simple.BuildToolDepends.desugarBuildTool
    -- and 'knownSuffixHandlers' in "Distribution.Client.Init.Heuristics"
            [ "alex"
            , "c2hs"
            , "cpphs"
            , "greencard"
            , "happy"
            , "hsc2hs"
            ]

instance IsDependency PkgconfigVersionRange PkgconfigDependency where
    type DepKey PkgconfigDependency = PkgconfigName

    depTypeName Proxy                      = "pkg-config"
    depKey (PkgconfigDependency pkgname _) = pkgname
    depKeyShow Proxy                       = prettyShow''
    depVerRg (PkgconfigDependency _ vr)    = vr
    reconstructDep                         = PkgconfigDependency

    intersectVr _ = PcIntersectVersionRanges


-- The result tuple represents the 3 canonicalised dependency
-- (removed deps (old ranges), retained deps (old & new ranges), added deps (new ranges))
-- or expressed as set-operations: (A \ B, (A ∩ B), B \ A)
computeCanonDepChange :: forall vr d. IsDependency vr d => [d] -> [d] -> ([d],[(DepKey d,(vr,vr))],[d])
computeCanonDepChange depsA depsB
    = ( mapToDeps (a `Map.difference` b)
      , Map.toList $ Map.intersectionWith (,) a b
      , mapToDeps (b `Map.difference` a)
      )
  where
    a = depsToMapWithCanonVerRange depsA
    b = depsToMapWithCanonVerRange depsB

    depsToMapWithCanonVerRange
        = Map.fromListWith (flip $ intersectVr (Proxy :: Proxy d)) .
          map (\d -> (depKey d, depVerRg d))

    mapToDeps
        = map (\(pkgname, verrange) -> reconstructDep pkgname verrange) . Map.toList


checkSetupBuildInfo :: Check (Maybe SetupBuildInfo)
checkSetupBuildInfo Nothing  Nothing = return ()
checkSetupBuildInfo (Just _) Nothing =
    fail "Cannot remove a 'custom-setup' section"

checkSetupBuildInfo Nothing (Just (SetupBuildInfo setupDependsA _internalA)) =
    logChange $ Change Normal
                       "added a 'custom-setup' section with 'setup-depends'"
                       "[implicit]" (intercalate ", " (map prettyShow setupDependsA))

checkSetupBuildInfo (Just (SetupBuildInfo setupDependsA _internalA))
                    (Just (SetupBuildInfo setupDependsB _internalB)) = do
    forM_ removed $ \dep ->
      logChange $ Change Normal "removed 'custom-setup' dependency on" (prettyShow dep) ""
    forM_ added $ \dep ->
      logChange $ Change Normal "added 'custom-setup' dependency on" "" (prettyShow dep)
    forM_ changed $ \(pkgn, (verA, verB)) ->
        changesOk ("the 'custom-setup' dependency on " ++ prettyShow'' pkgn)
                  prettyShow verA verB
  where
    (removed, changed, added) =
      computeCanonDepChange setupDependsA setupDependsB

checkLibrary :: ComponentName -> Check Library
checkLibrary componentName
             (Library modulesA reexportedA requiredSigsA exposedSigsA
                      exposedA visibilityA buildInfoA)
             (Library modulesB reexportedB requiredSigsB exposedSigsB
                      exposedB visibilityB buildInfoB) = do
  checkSame "Cannot change the exposed modules" modulesA modulesB
  checkSame "Cannot change the re-exported modules" reexportedA reexportedB
  checkSame "Cannot change the required signatures" requiredSigsA requiredSigsB
  checkSame "Cannot change the exposed signatures"  exposedSigsA  exposedSigsB
  checkSame "Cannot change the package exposed status" exposedA exposedB
  checkSame "Cannot change the package visibility" visibilityA visibilityB
  checkBuildInfo componentName buildInfoA buildInfoB

checkForeignLib :: ComponentName -> Check ForeignLib
checkForeignLib componentName
             (ForeignLib nameA typeA optionsA buildInfoA verA verLinuxA modDefA)
             (ForeignLib nameB typeB optionsB buildInfoB verB verLinuxB modDefB) = do
  checkSame "Cannot change the foreign library name" nameA nameB
  checkSame "Cannot change the foreign library type" typeA typeB
  checkSame "Cannot change the foreign library options" optionsA optionsB
  checkSame "Cannot change the foreign library version" verA verB
  checkSame "Cannot change the foreign library version for Linux" verLinuxA verLinuxB
  checkSame "Cannot change the module definition files" modDefA modDefB
  checkBuildInfo componentName buildInfoA buildInfoB

checkExecutable :: ComponentName -> Check Executable
checkExecutable componentName
                (Executable _nameA pathA scopeA buildInfoA)
                (Executable _nameB pathB scopeB buildInfoB) = do
  checkSame "Cannot change build information" pathA pathB
  checkSame "Cannot change executable scope" scopeA scopeB
  checkBuildInfo componentName buildInfoA buildInfoB

checkTestSuite :: ComponentName -> Check TestSuite
checkTestSuite componentName
#if MIN_VERSION_Cabal(3,7,0)
               (TestSuite _nameA interfaceA buildInfoA testGeneratorsA)
               (TestSuite _nameB interfaceB buildInfoB testGeneratorsB)
#else
               (TestSuite _nameA interfaceA buildInfoA)
               (TestSuite _nameB interfaceB buildInfoB)
#endif
  = do
  checkSame "Cannot change test-suite type" interfaceA interfaceB
  checkBuildInfo componentName buildInfoA buildInfoB
#if MIN_VERSION_Cabal(3,7,0)
  -- @test-generators@
  checkSame "Cannot change test-generators" testGeneratorsA testGeneratorsB
#endif

checkBenchmark :: ComponentName -> Check Benchmark
checkBenchmark componentName
               (Benchmark _nameA interfaceA buildInfoA)
               (Benchmark _nameB interfaceB buildInfoB) = do
  checkSame "Cannot change benchmark type" interfaceA interfaceB
  checkBuildInfo componentName buildInfoA buildInfoB

checkBuildInfo :: ComponentName -> Check BuildInfo
checkBuildInfo componentName biA biB = do
    -- @other-extension@
    changesOkSet ("'other-extensions' in " ++ showComponentName componentName ++ " component")
              prettyShow
              (Set.fromList $ otherExtensions biA) (Set.fromList $ otherExtensions biB)

    -- @buildable@
    changesOk ("'buildable' in " ++ showComponentName componentName ++ " component") prettyShow
              (buildable biA) (buildable biB)

    -- @build-tool-depends@
    checkDependencies componentName
        (buildToolDepends biA)
        (buildToolDepends biB)

    -- @build-tools@
    checkDependencies componentName
        (buildTools biA)
        (buildTools biB)

    -- @pkgconfig-depends@
    checkDependencies componentName
        (pkgconfigDepends biA)
        (pkgconfigDepends biB)

    checkSame "Cannot change build information (just the dependency version constraints)"
              (biA { targetBuildDepends = [], otherExtensions = [], buildTools = [], buildToolDepends = [], pkgconfigDepends = [], buildable = True })
              (biB { targetBuildDepends = [], otherExtensions = [], buildTools = [], buildToolDepends = [], pkgconfigDepends = [], buildable = True })

changesOk' :: Eq a => Severity -> String -> (a -> String) -> Check a
changesOk' rel what render a b
  | a == b    = return ()
  | otherwise = logChange (Change rel what (render a) (render b))

changesOk :: Eq a => String -> (a -> String) -> Check a
changesOk = changesOk' Normal

changesOkList :: (String -> (a -> String) -> Check a)
              -> String -> (a -> String) -> Check [a]
changesOkList changesOkElem what render = go
  where
    go []     []     = return ()
    go (a:_)  []     = logChange (Change Normal ("removed " ++ what) (render a) "")
    go []     (b:_)  = logChange (Change Normal ("added "   ++ what) "" (render b))
    go (a:as) (b:bs) = changesOkElem what render a b >> go as bs

changesOkSet :: Ord a => String -> (a -> String) -> Check (Set.Set a)
changesOkSet what render old new = do
    unless (Set.null removed) $
        logChange (Change Normal ("removed " ++ what) (renderSet removed) "")
    unless (Set.null added) $
        logChange (Change Normal ("added " ++ what) "" (renderSet added))
    return ()
  where
    added   = new Set.\\ old
    removed = old Set.\\ new
    renderSet = intercalate ", " . map render . Set.toList


-- | Single-quote-wrapping 'prettyShow'
prettyShow'' :: Pretty a => a -> String
prettyShow'' x = "'" ++ prettyShow x ++ "'"

checkSame :: Eq a => String -> Check a
checkSame msg x y | x == y    = return ()
                  | otherwise = fail msg

checkList :: String -> Check a -> Check [a]
checkList _   _         []     []     = return ()
checkList msg checkElem (x:xs) (y:ys) = checkElem x y
                                     >> checkList msg checkElem xs ys
checkList msg _         _      _      = fail msg

checkListAssoc :: Eq b => String -> Check a -> Check [(b,a)]
checkListAssoc _   _         [] [] = return ()
checkListAssoc msg checkElem ((kx,x):xs) ((ky,y):ys)
                       | kx == ky  = checkElem x y
                                  >> checkListAssoc msg checkElem xs ys
                       | otherwise = fail msg
checkListAssoc msg _         _  _  = fail msg

checkNotPresent :: String -> Check (Maybe String)
checkNotPresent msg _ (Just _) = fail msg
checkNotPresent _ _ Nothing = return ()

checkMaybe :: String -> Check a -> Check (Maybe a)
checkMaybe _   _     Nothing  Nothing  = return ()
checkMaybe _   check (Just x) (Just y) = check x y
checkMaybe msg _     _        _        = fail msg

ppTestedWith :: [(CompilerFlavor, VersionRange)] -> Doc
ppTestedWith = hsep . punctuate colon . map (uncurry ppPair)
  where
    ppPair compiler vr = text (prettyShow compiler) <+> text (prettyShow vr)

ppSourceRepo :: SourceRepo -> PrettyField ()
ppSourceRepo repo = PrettySection () "source-repository" [pretty kind] $
    prettyFieldGrammar cabalSpecLatest (sourceRepoFieldGrammar kind) repo
  where
    kind = repoKind repo

-- TODO: Verify that we don't need to worry about UTF8
-- | Insert or update \"x-revision:\" field
insertRevisionField :: Int -> ByteString -> ByteString
insertRevisionField rev
    | rev == 1  = LBS8.unlines . insertAfterVersion . LBS8.lines
    | otherwise = LBS8.unlines . replaceRevision    . LBS8.lines
  where
    replaceRevision [] = []
    replaceRevision (ln:lns)
      | isField (LBS8.pack "x-revision") ln
      = LBS8.pack ("x-revision: " ++ show rev) : lns

      | otherwise
      = ln : replaceRevision lns

    insertAfterVersion [] = []
    insertAfterVersion (ln:lns)
      | isField (LBS8.pack "version") ln
      = ln : LBS8.pack ("x-revision: " ++ show rev) : lns

      | otherwise
      = ln : insertAfterVersion lns

    isField nm ln
      | LBS8.isPrefixOf nm (LBS8.map Char.toLower ln)
      , let (_, t) = LBS8.span (\c -> c == ' ' || c == '\t')
                             (LBS8.drop (LBS8.length nm) ln)
      , Just (':',_) <- LBS8.uncons t
                  = True
      | otherwise = False
