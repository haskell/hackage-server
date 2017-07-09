{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      :  Distribution.Server.Util.CabalRevisions
-- Copyright   :  Duncan Coutts et al.
-- License     :  BSD3
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Validation and helpers for Cabal revision handling
module Distribution.Server.Util.CabalRevisions
    ( diffCabalRevisions
    , Change(..)
    , insertRevisionField
    ) where

-- NB: This module avoids to import any hackage-server modules
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.PkgconfigDependency
import Distribution.Types.LegacyExeDependency
import Distribution.Types.UnqualComponentName
import Distribution.Types.CondTree
import Distribution.Types.ForeignLib
import Distribution.Package
import Distribution.Text (display)
import Distribution.Version
import Distribution.Compiler (CompilerFlavor)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
         (parseGenericPackageDescription, sourceRepoFieldDescrs)
import Distribution.PackageDescription.Check
import Distribution.ParseUtils
         ( ParseResult(..), locatedErrorMsg, showPWarning )
import Distribution.ParseUtils (FieldDescr(..))
import Distribution.Text (Text(..))
import Distribution.Simple.LocalBuildInfo (ComponentName(..) ,showComponentName)
import Text.PrettyPrint as Doc
         (nest, empty, isEmpty, (<+>), colon, (<>), text, vcat, ($+$), Doc, hsep, punctuate)

import Control.Applicative
import Control.Monad
import Control.Monad.Except  (ExceptT, runExceptT, throwError)
import Control.Monad.Writer (MonadWriter(..), Writer, runWriter)
import Data.List
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Char as Char
import qualified Data.Semigroup as S
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Foldable (foldMap)
import Data.Proxy (Proxy(Proxy))

-- | Entry point to cabal revision validator
--
-- This takes an original and a revised @.cabal@ decoded as Unicode
-- 'String' and performs validations. Returns either a validation
-- error or a list of detected changes.
diffCabalRevisions :: String -> String -> Either String [Change]
diffCabalRevisions oldVersion newRevision = runCheck $
    checkCabalFileRevision oldVersion newRevision

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
instance S.Monoid Severity where
    mempty = Trivial
    mappend = (S.<>)

data Change = Change Severity String String String -- severity, what, from, to
  deriving Show



logChange :: Change -> CheckM ()
logChange change = CheckM (tell [change])

type Check a = a -> a -> CheckM ()

checkCabalFileRevision :: Check String
checkCabalFileRevision old new = do
    (pkg,  warns)  <- parseCabalFile old
    (pkg', warns') <- parseCabalFile new

    let pkgid    = packageId pkg
        filename = display pkgid ++ ".cabal"

    checkGenericPackageDescription pkg pkg'
    checkParserWarnings filename warns warns'
    checkPackageChecks  pkg   pkg'

  where
    parseCabalFile fileContent =
      case parseGenericPackageDescription fileContent of
        ParseFailed      err -> fail (formatErrorMsg (locatedErrorMsg err))
        ParseOk warnings pkg -> return (pkg, warnings)

    formatErrorMsg (Nothing, msg) = msg
    formatErrorMsg (Just n,  msg) = "Line " ++ show n ++ ": " ++ msg

    checkParserWarnings filename warns warns' =
      case warns' \\ warns of
        []       -> return ()
        newwarns -> fail $ "New parse warning: "
                        ++ unlines (map (showPWarning filename) newwarns)

    checkPackageChecks pkg pkg' =
      let checks  = checkPackage pkg  Nothing
          checks' = checkPackage pkg' Nothing
       in case checks' \\ checks of
            []        -> return ()
            newchecks -> fail $ unlines (map explanation newchecks)

checkGenericPackageDescription :: Check GenericPackageDescription
checkGenericPackageDescription
    (GenericPackageDescription descrA flagsA libsA sublibsA flibsA exesA testsA benchsA)
    (GenericPackageDescription descrB flagsB libsB sublibsB flibsB exesB testsB benchsB) = do

    checkPackageDescriptions descrA descrB

    checkList "Cannot add or remove flags" checkFlag flagsA flagsB

    checkMaybe "Cannot add or remove library sections"
      (checkCondTree checkLibrary)
      (withComponentName' CLibName <$> libsA)
      (withComponentName' CLibName <$> libsB)

    checkListAssoc "Cannot add or remove sub-library sections"
      (checkCondTree checkLibrary)
      (withComponentName CSubLibName <$> sublibsA)
      (withComponentName CSubLibName <$> sublibsB)

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


checkFlag :: Check Flag
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

    changesOk ("default of flag '" ++ fname ++ "'") display
              (flagDefault flagOld) (flagDefault flagNew)

    changesOk ("description of flag '" ++ fname ++ "'") id
              (flagDescription flagOld) (flagDescription flagNew)

checkPackageDescriptions :: Check PackageDescription
checkPackageDescriptions
  pdA@(PackageDescription
     packageIdA licenseA licenseFileA
     copyrightA maintainerA authorA stabilityA testedWithA homepageA
     pkgUrlA bugReportsA sourceReposA synopsisA descriptionA
     categoryA customFieldsA _buildDependsA _specVersionRawA buildTypeA
     customSetupA _libraryA _subLibrariesA _executablesA _foreignLibsA
     _testSuitesA _benchmarksA
     dataFilesA dataDirA extraSrcFilesA extraTmpFilesA extraDocFilesA)
  pdB@(PackageDescription
     packageIdB licenseB licenseFileB
     copyrightB maintainerB authorB stabilityB testedWithB homepageB
     pkgUrlB bugReportsB sourceReposB synopsisB descriptionB
     categoryB customFieldsB _buildDependsB _specVersionRawB buildTypeB
     customSetupB _libraryB _subLibrariesB _executablesB _foreignLibsB
     _testSuitesB _benchmarksB
     dataFilesB dataDirB extraSrcFilesB extraTmpFilesB extraDocFilesB)
  = do
  checkSame "Don't be silly! You can't change the package name!"
            (packageName packageIdA) (packageName packageIdB)
  checkSame "You can't change the package version!"
            (packageVersion packageIdA) (packageVersion packageIdB)
  checkSame "Cannot change the license"
            (licenseA, licenseFileA) (licenseB, licenseFileB)
  changesOk "copyright"  id copyrightA copyrightB
  changesOk "maintainer" id maintainerA maintainerB
  changesOk "author"     id authorA authorB
  checkSame "The stability field is unused, don't bother changing it."
            stabilityA stabilityB
  changesOk' Trivial "tested-with" (show . ppTestedWith) testedWithA testedWithB
  changesOk "homepage" id homepageA homepageB
  checkSame "The package-url field is unused, don't bother changing it."
            pkgUrlA pkgUrlB
  changesOk "bug-reports" id bugReportsA bugReportsB
  changesOkList changesOk "source-repository" (show . ppSourceRepo)
            sourceReposA sourceReposB
  changesOk "synopsis"    id synopsisA synopsisB
  changesOk "description" id descriptionA descriptionB
  changesOk "category"    id categoryA categoryB
  checkSame "Cannot change the build-type"
            buildTypeA buildTypeB
  checkSame "Cannot change the data files"
            (dataFilesA, dataDirA) (dataFilesB, dataDirB)
  checkSame "Changing extra-tmp-files is a bit pointless at this stage"
            extraTmpFilesA extraTmpFilesB
  checkSame "Changing extra-source-files would not make sense!"
            extraSrcFilesA extraSrcFilesB
  checkSame "You can't change the extra-doc-files."
            extraDocFilesA extraDocFilesB

  checkSame "Cannot change custom/extension fields"
            (filter (\(f,_) -> f /= "x-revision") customFieldsA)
            (filter (\(f,_) -> f /= "x-revision") customFieldsB)

  checkSpecVersionRaw pdA pdB
  checkSetupBuildInfo customSetupA customSetupB

  checkRevision customFieldsA customFieldsB

checkSpecVersionRaw :: Check PackageDescription
checkSpecVersionRaw pdA pdB
  | specVersionA `withinRange` range110To120
  , specVersionB `withinRange` range110To120
  = changesOk "cabal-version" display specVersionA specVersionB

  | otherwise
  = checkSame "Cannot change the Cabal spec version"
              specVersionA specVersionB
  where
    specVersionA = specVersion pdA
    specVersionB = specVersion pdB

    -- nothing interesting changed within the  Cabal >=1.10 && <1.21 range
    -- therefore we allow to change the spec version within this interval
    range110To120 = (orLaterVersion (mkVersion [1,10])) `intersectVersionRanges`
                    (earlierVersion (mkVersion [1,21]))

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

checkDependencies :: forall d. (Text d, IsDependency d) => ComponentName -> Check [d]
checkDependencies componentName ds1 ds2 = do
    forM_ removed $ \dep -> do
        fail (unwords [ "Cannot remove existing", depKind, "on"
                      , depKeyShow dproxy (depKey dep), "in", cnameStr, " component"])

    forM_ added $ \dep ->
        if depInAddWhitelist dep
           then logChange (Change Normal (unwords ["added the", cnameStr, "component's"
                                                  , depKind, "on"]) "" (display dep))
           else fail (unwords [ "Cannot add new", depKind, "on"
                              , depKeyShow dproxy (depKey dep)
                              , "in", cnameStr, "component"])

    forM_ changed $ \(depk, (verA, verB)) -> do
        changesOk (unwords ["the", cnameStr, "component's", depKind, "on"
                           , depKeyShow dproxy depk])
                   display verA verB
  where
    (removed, changed, added) = computeCanonDepChange ds1 ds2

    dproxy :: Proxy d
    dproxy = Proxy

    cnameStr = showComponentName componentName

    depKind = depTypeName dproxy ++ " dependency"

class (Ord (DepKey d)) => IsDependency d where
    type DepKey d

    depTypeName    :: Proxy d -> String
    depKey         :: d -> DepKey d
    depKeyShow     :: Proxy d -> DepKey d -> String
    depVerRg       :: d -> VersionRange
    reconstructDep :: DepKey d -> VersionRange -> d

    depInAddWhitelist :: d -> Bool
    depInAddWhitelist _ = False

instance IsDependency Dependency where
    type DepKey Dependency = PackageName

    depTypeName Proxy             = "library"
    depKey (Dependency pkgname _) = pkgname
    depKeyShow Proxy              = display''
    depVerRg (Dependency _ vr)    = vr
    reconstructDep                = Dependency

    depInAddWhitelist (Dependency pn _) = pn `elem`
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


instance IsDependency ExeDependency where
    type DepKey ExeDependency = (PackageName,UnqualComponentName)

    depTypeName Proxy                   = "tool"
    depKey (ExeDependency pkgname cn _) = (pkgname,cn)
    depKeyShow Proxy (pkgname,cn)       = concat ["'", display pkgname, ":", display cn, "'"]
    depVerRg (ExeDependency _ _ vr)     = vr
    reconstructDep (pkgname,cn)         = ExeDependency pkgname cn

instance IsDependency LegacyExeDependency where
    type DepKey LegacyExeDependency = String

    depTypeName Proxy                      = "legacy-tool"
    depKey (LegacyExeDependency tname _)   = tname
    depKeyShow Proxy tname                 = "'" ++ tname ++ "'"
    depVerRg (LegacyExeDependency _ vr)    = vr
    reconstructDep                         = LegacyExeDependency

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

instance IsDependency PkgconfigDependency where
    type DepKey PkgconfigDependency = PkgconfigName

    depTypeName Proxy                      = "pkg-config"
    depKey (PkgconfigDependency pkgname _) = pkgname
    depKeyShow Proxy                       = display''
    depVerRg (PkgconfigDependency _ vr)    = vr
    reconstructDep                         = PkgconfigDependency


-- The result tuple represents the 3 canonicalised dependency
-- (removed deps (old ranges), retained deps (old & new ranges), added deps (new ranges))
-- or expressed as set-operations: (A \ B, (A ∩ B), B \ A)
computeCanonDepChange :: IsDependency d => [d] -> [d] -> ([d],[(DepKey d,(VersionRange,VersionRange))],[d])
computeCanonDepChange depsA depsB
    = ( mapToDeps (a `Map.difference` b)
      , Map.toList $ Map.intersectionWith (,) a b
      , mapToDeps (b `Map.difference` a)
      )
  where
    a = depsToMapWithCanonVerRange depsA
    b = depsToMapWithCanonVerRange depsB

    depsToMapWithCanonVerRange
        = Map.fromListWith (flip intersectVersionRanges) .
          map (\d -> (depKey d, depVerRg d))

    mapToDeps
        = map (\(pkgname, verrange) -> reconstructDep pkgname verrange) . Map.toList


checkSetupBuildInfo :: Check (Maybe SetupBuildInfo)
checkSetupBuildInfo Nothing  Nothing = return ()
checkSetupBuildInfo (Just _) Nothing =
    fail "Cannot remove a 'custom-setup' section"

checkSetupBuildInfo Nothing (Just (SetupBuildInfo setupDependsA _internalA)) =
    logChange $ Change Normal
                       ("added a 'custom-setup' section with 'setup-depends'")
                       "[implicit]" (intercalate ", " (map display setupDependsA))

checkSetupBuildInfo (Just (SetupBuildInfo setupDependsA _internalA))
                    (Just (SetupBuildInfo setupDependsB _internalB)) = do
    forM_ removed $ \dep ->
      logChange $ Change Normal ("removed 'custom-setup' dependency on") (display dep) ""
    forM_ added $ \dep ->
      logChange $ Change Normal ("added 'custom-setup' dependency on") "" (display dep)
    forM_ changed $ \(pkgn, (verA, verB)) ->
        changesOk ("the 'custom-setup' dependency on " ++ display'' pkgn)
                  display verA verB
  where
    (removed, changed, added) =
      computeCanonDepChange setupDependsA setupDependsB

checkLibrary :: ComponentName -> Check Library
checkLibrary componentName
             (Library modulesA reexportedA requiredSigsA exposedSigsA
                      exposedA buildInfoA)
             (Library modulesB reexportedB requiredSigsB exposedSigsB
                      exposedB buildInfoB) = do
  checkSame "Cannot change the exposed modules" modulesA modulesB
  checkSame "Cannot change the re-exported modules" reexportedA reexportedB
  checkSame "Cannot change the required signatures" requiredSigsA requiredSigsB
  checkSame "Cannot change the exposed signatures"  exposedSigsA  exposedSigsB
  checkSame "Cannot change the package exposed status" exposedA exposedB
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
               (TestSuite _nameA interfaceA buildInfoA)
               (TestSuite _nameB interfaceB buildInfoB) = do
  checkSame "Cannot change test-suite type" interfaceA interfaceB
  checkBuildInfo componentName buildInfoA buildInfoB

checkBenchmark :: ComponentName -> Check Benchmark
checkBenchmark componentName
               (Benchmark _nameA interfaceA buildInfoA)
               (Benchmark _nameB interfaceB buildInfoB) = do
  checkSame "Cannot change benchmark type" interfaceA interfaceB
  checkBuildInfo componentName buildInfoA buildInfoB

checkBuildInfo :: ComponentName -> Check BuildInfo
checkBuildInfo componentName biA biB = do
    changesOkSet ("'other-extensions' in " ++ showComponentName componentName ++ " component")
              display
              (Set.fromList $ otherExtensions biA) (Set.fromList $ otherExtensions biB)

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

    checkSame "Cannot change build information \
              \(just the dependency version constraints)"
              (biA { targetBuildDepends = [], otherExtensions = [], buildTools = [], buildToolDepends = [], pkgconfigDepends = [] })
              (biB { targetBuildDepends = [], otherExtensions = [], buildTools = [], buildToolDepends = [], pkgconfigDepends = [] })

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


-- | Single-quote-wrapping 'display'
display'' :: Text a => a -> String
display'' x = "'" ++ display x ++ "'"

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

checkMaybe :: String -> Check a -> Check (Maybe a)
checkMaybe _   _     Nothing  Nothing  = return ()
checkMaybe _   check (Just x) (Just y) = check x y
checkMaybe msg _     _        _        = fail msg

ppTestedWith :: [(CompilerFlavor, VersionRange)] -> Doc
ppTestedWith = hsep . punctuate colon . map (uncurry ppPair)
  where
    ppPair compiler vr = text (display compiler) <+> text (display vr)

--TODO: export from Cabal
ppSourceRepo :: SourceRepo -> Doc
ppSourceRepo repo =
    emptyLine $ text "source-repository" <+> disp (repoKind repo) $+$
        (nest 4 (ppFields sourceRepoFieldDescrs' repo))
  where
    sourceRepoFieldDescrs' =
      filter (\fd -> fieldName fd /= "kind") sourceRepoFieldDescrs

    emptyLine :: Doc -> Doc
    emptyLine d = text " " $+$ d

    ppFields :: [FieldDescr a] -> a -> Doc
    ppFields fields x =
        vcat [ ppField name (getter x)
             | FieldDescr name getter _ <- fields]

    ppField :: String -> Doc -> Doc
    ppField name fielddoc | isEmpty fielddoc = Doc.empty
                          | otherwise        = text name <> colon <+> fielddoc


-- TODO: Verify that we don't need to worry about UTF8
-- | Insert or update \"x-revision:\" field
insertRevisionField :: Int -> ByteString -> ByteString
insertRevisionField rev
    | rev == 1  = BS.unlines . insertAfterVersion . BS.lines
    | otherwise = BS.unlines . replaceRevision    . BS.lines
  where
    replaceRevision [] = []
    replaceRevision (ln:lns)
      | isField (BS.pack "x-revision") ln
      = BS.pack ("x-revision: " ++ show rev) : lns

      | otherwise
      = ln : replaceRevision lns

    insertAfterVersion [] = []
    insertAfterVersion (ln:lns)
      | isField (BS.pack "version") ln
      = ln : BS.pack ("x-revision: " ++ show rev) : lns

      | otherwise
      = ln : insertAfterVersion lns

    isField nm ln
      | BS.isPrefixOf nm (BS.map Char.toLower ln)
      , let (_, t) = BS.span (\c -> c == ' ' || c == '\t')
                             (BS.drop (BS.length nm) ln)
      , Just (':',_) <- BS.uncons t
                  = True
      | otherwise = False
