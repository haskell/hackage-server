{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards, BangPatterns,
             StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Distribution.Server.Features.EditCabalFiles (
    initEditCabalFilesFeature
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.Templating

import Distribution.Server.Features.Users
import Distribution.Server.Features.Core
import Distribution.Server.Packages.Types
import Distribution.Server.Features.Upload

import Distribution.Package
import Distribution.Text (display)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
         (parsePackageDescription, sourceRepoFieldDescrs)
import Distribution.PackageDescription.Check
import Distribution.ParseUtils
         ( ParseResult(..), locatedErrorMsg, PWarning(..), showPWarning )
import Distribution.Server.Util.Parse (unpackUTF8)
import Distribution.ParseUtils (FieldDescr(..))
import Distribution.Text (Text(..))
import Text.PrettyPrint as Doc
         (nest, empty, isEmpty, (<+>), colon, (<>), text, vcat, ($+$), Doc)
import Text.StringTemplate (ToSElem(..))

import Data.List
import qualified Data.Char as Char
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import Control.Monad.Error  (ErrorT, runErrorT)
import Control.Monad.Writer (MonadWriter(..), Writer, runWriter)
import Data.Time (getCurrentTime)

import qualified Data.ByteString.Lazy.Char8 as BS -- TODO: Verify that we don't need to worry about UTF8

-- | A feature to allow editing cabal files without uploading new tarballs.
--
initEditCabalFilesFeature :: ServerEnv
                          -> UserFeature -> CoreFeature -> UploadFeature
                          -> IO HackageFeature
initEditCabalFilesFeature env@ServerEnv{serverTemplatesDir, serverTemplatesMode} user core upload = do

  -- Page templates
  templates <- loadTemplates serverTemplatesMode
                 [serverTemplatesDir, serverTemplatesDir </> "EditCabalFile"]
                 ["cabalFileEditPage.html", "cabalFilePublished.html"]

  let feature = editCabalFilesFeature env templates user core upload

  return feature


editCabalFilesFeature :: ServerEnv -> Templates
                      -> UserFeature -> CoreFeature -> UploadFeature
                      -> HackageFeature
editCabalFilesFeature _env templates
                      UserFeature{guardAuthorised}
                      CoreFeature{..}
                      UploadFeature{maintainersGroup, trusteesGroup} =
  (emptyHackageFeature "edit-cabal-files") {
    featureResources =
      [ editCabalFileResource
      ]
  , featureState = []
  }

  where
    CoreResource{..} = coreResource
    editCabalFileResource =
      (resourceAt "/package/:package/:cabal.cabal/edit")  {
        resourceDesc = [(GET,  "Page to edit package metadata")
                       ,(POST, "Modify the package metadata")],
        resourceGet  = [("html", serveEditCabalFileGet)],
        resourcePost = [("html", serveEditCabalFilePost)]
      }

    serveEditCabalFileGet :: DynamicPath -> ServerPartE Response
    serveEditCabalFileGet dpath = do
        template <- getTemplate templates "cabalFileEditPage.html"
        pkg <- packageInPath dpath >>= lookupPackageId
        let pkgname = packageName pkg
            pkgid   = packageId pkg
        -- check that the cabal name matches the package
        guard (lookup "cabal" dpath == Just (display pkgname))
        ok $ toResponse $ template
          [ "pkgid"     $= display pkgid
          , "cabalfile" $= insertRevisionField (1 + length (pkgDataOld pkg))
                             (cabalFileByteString (pkgData pkg))
          ]

    serveEditCabalFilePost :: DynamicPath -> ServerPartE Response
    serveEditCabalFilePost dpath = do
        template <- getTemplate templates "cabalFileEditPage.html"
        pkg <- packageInPath dpath >>= lookupPackageId
        let pkgname = packageName pkg
            pkgid   = packageId pkg
        -- check that the cabal name matches the package
        guard (lookup "cabal" dpath == Just (display pkgname))
        uid <- guardAuthorised [ InGroup (maintainersGroup pkgname)
                               , InGroup trusteesGroup ]
        let oldVersion = cabalFileByteString (pkgData pkg)
        newRevision <- getCabalFile
        shouldPublish <- getPublish
        case runCheck $ checkCabalFileRevision pkgid oldVersion newRevision of
          Left errs ->
            responseTemplate template pkgid newRevision
                             shouldPublish [errs] []

          Right changes
            | shouldPublish && not (null changes) -> do
                template' <- getTemplate templates "cabalFilePublished.html"
                time <- liftIO getCurrentTime
                updateAddPackageRevision pkgid (CabalFileText newRevision)
                                               (time, uid)
                ok $ toResponse $ template'
                  [ "pkgid"     $= display pkgid
                  , "cabalfile" $= newRevision
                  , "changes"   $= changes
                  ]
            | otherwise ->
                responseTemplate template pkgid newRevision
                                 shouldPublish [] changes

       where
         getCabalFile = body (lookBS "cabalfile")
         getPublish   = body $ (look "review" >> return False) `mplus`
                               (look "publish" >> return True)

         responseTemplate :: ([TemplateAttr] -> Template) -> PackageId
                          -> ByteString -> Bool -> [String] -> [Change]
                          -> ServerPartE Response
         responseTemplate template pkgid cabalFile publish errors changes =
           ok $ toResponse $ template
             [ "pkgid"     $= display pkgid
             , "cabalfile" $= cabalFile
             , "publish"   $= publish
             , "errors"    $= errors
             , "changes"   $= changes
             ]

instance ToSElem Change where
  toSElem (Change change from to) =
    toSElem (Map.fromList [("what", change)
                          ,("from", from)
                          ,("to", to)])

newtype CheckM a = CheckM { unCheckM :: ErrorT String (Writer [Change]) a }

runCheck :: CheckM () -> Either String [Change]
runCheck c = case runWriter . runErrorT . unCheckM $ c of
               (Left err, _      ) -> Left err
               (Right (), changes) -> Right changes

instance Monad CheckM where
  return         = CheckM . return
  CheckM m >>= f = CheckM (m >>= unCheckM . f)
  fail           = CheckM . throwError

data Change = Change String String String -- what, from, to
  deriving Show

logChange :: Change -> CheckM ()
logChange change = CheckM (tell [change])

type Check a = a -> a -> CheckM ()

checkCabalFileRevision :: PackageId -> Check ByteString
checkCabalFileRevision pkgid old new = do
    (pkg,  warns)  <- parseCabalFile old
    (pkg', warns') <- parseCabalFile new

    checkGenericPackageDescription pkg pkg'
    checkParserWarnings warns warns'
    checkPackageChecks  pkg   pkg'

  where
    filename = display pkgid ++ ".cabal"

    parseCabalFile fileContent =
      case parsePackageDescription . unpackUTF8 $ fileContent of
        ParseFailed      err -> fail (formatErrorMsg (locatedErrorMsg err))
        ParseOk warnings pkg -> return (pkg, warnings)

    formatErrorMsg (Nothing, msg) = msg
    formatErrorMsg (Just n,  msg) = "Line " ++ show n ++ ": " ++ msg

    checkParserWarnings warns warns' =
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
    (GenericPackageDescription descrA flagsA libsA exesA testsA benchsA)
    (GenericPackageDescription descrB flagsB libsB exesB testsB benchsB) = do

    checkPackageDescriptions descrA descrB

    checkSame "Sorry, cannot edit the package flags"
      flagsA flagsB

    checkMaybe "Cannot add or remove library sections"
      (checkCondTree checkLibrary) libsA libsB

    checkListAssoc "Cannot add or remove executable sections"
      (checkCondTree checkExecutable) exesA exesB

    checkListAssoc "Cannot add or remove test-suite sections"
      (checkCondTree checkTestSuite) testsA testsB

    checkListAssoc "Cannot add or remove benchmark sections"
      (checkCondTree checkBenchmark) benchsA benchsB


checkPackageDescriptions :: Check PackageDescription
checkPackageDescriptions
  (PackageDescription
     packageIdA licenseA licenseFileA
     copyrightA maintainerA authorA stabilityA testedWithA homepageA
     pkgUrlA bugReportsA sourceReposA synopsisA descriptionA
     categoryA customFieldsA _buildDependsA specVersionA buildTypeA
     _libraryA _executablesA _testSuitesA _benchmarksA dataFilesA dataDirA
     extraSrcFilesA extraTmpFilesA {-_extraHtmlFilesA-})
  (PackageDescription
     packageIdB licenseB licenseFileB
     copyrightB maintainerB authorB stabilityB testedWithB homepageB
     pkgUrlB bugReportsB sourceReposB synopsisB descriptionB
     categoryB customFieldsB _buildDependsB specVersionB buildTypeB
     _libraryB _executablesB _testSuitesB _benchmarksB dataFilesB dataDirB
     extraSrcFilesB extraTmpFilesB {-_extraHtmlFilesB-})
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
  checkSame "The tested-with field is unused, don't bother changing it."
            testedWithA testedWithB
  changesOk "homepage" id homepageA homepageB
  checkSame "The package-url field is unused, don't bother changing it."
            pkgUrlA pkgUrlB
  changesOk "bug-reports" id bugReportsA bugReportsB
  changesOkList changesOk "source-repository" (show . ppSourceRepo)
            sourceReposA sourceReposB
  changesOk "synopsis"    id synopsisA synopsisB
  changesOk "description" id descriptionA descriptionB
  changesOk "category"    id categoryA categoryB
  checkSame "Cannot change the Cabal spec version"
            specVersionA specVersionB
  checkSame "Cannot change the build-type"
            buildTypeA buildTypeB
  checkSame "Cannot change the data files"
            (dataFilesA, dataDirA) (dataFilesB, dataDirB)
  checkSame "Changing extra-tmp-files is a bit pointless at this stage"
            extraTmpFilesA extraTmpFilesB
  checkSame "Changing extra-source-files would not make sense!"
            extraSrcFilesA extraSrcFilesB

  checkSame "Cannot change custom/extension fields"
            (filter (\(f,_) -> f /= "x-revision") customFieldsA)
            (filter (\(f,_) -> f /= "x-revision") customFieldsB)
  checkRevision customFieldsA customFieldsB


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


checkCondTree :: Check a -> Check (CondTree ConfVar [Dependency] a)
checkCondTree checkElem
  (CondNode dataA constraintsA componentsA)
  (CondNode dataB constraintsB componentsB) = do
    checkDependencies constraintsA constraintsB
    checkList "Cannot add or remove 'if' conditionals"
              checkComponent componentsA componentsB
    checkElem dataA dataB
  where
    checkComponent (condA, ifPartA, thenPartA)
                   (condB, ifPartB, thenPartB) = do
      checkSame "Cannot change the 'if' condition expressions"
                condA condB
      checkCondTree checkElem ifPartA ifPartB
      checkMaybe "Cannot add or remove the 'else' part in conditionals"
                 (checkCondTree checkElem) thenPartA thenPartB

checkDependencies :: Check [Dependency]
checkDependencies =
  checkList "Cannot don't add or remove dependencies, \
            \just change the version constraints"
            checkDependency

checkDependency :: Check Dependency
checkDependency (Dependency pkgA verA) (Dependency pkgB verB)
  | pkgA == pkgB = changesOk ("dependency on " ++ display pkgA) display
                             verA verB
  | otherwise    = fail "Cannot change which packages are dependencies, \
                        \just their version constraints."

checkLibrary :: Check Library
checkLibrary (Library modulesA exposedA buildInfoA)
             (Library modulesB exposedB buildInfoB) = do
  checkSame "Cannot change the exposed modules" modulesA modulesB
  checkSame "Cannot change the package exposed status" exposedA exposedB
  checkBuildInfo buildInfoA buildInfoB

checkExecutable :: Check Executable
checkExecutable (Executable _nameA pathA buildInfoA)
                (Executable _nameB pathB buildInfoB) = do
  checkSame "Cannot change build information" pathA pathB
  checkBuildInfo buildInfoA buildInfoB

checkTestSuite :: Check TestSuite
checkTestSuite (TestSuite _nameA interfaceA buildInfoA _enabledA)
               (TestSuite _nameB interfaceB buildInfoB _enabledB) = do
  checkSame "Cannot change test-suite type" interfaceA interfaceB
  checkBuildInfo buildInfoA buildInfoB

checkBenchmark :: Check Benchmark
checkBenchmark (Benchmark _nameA interfaceA buildInfoA _enabledA)
               (Benchmark _nameB interfaceB buildInfoB _enabledB) = do
  checkSame "Cannot change benchmark type" interfaceA interfaceB
  checkBuildInfo buildInfoA buildInfoB

checkBuildInfo :: Check BuildInfo
checkBuildInfo =
  checkSame "Cannot change build information \
            \(just the dependency version constraints)"

changesOk :: Eq a => String -> (a -> String) -> Check a
changesOk what render a b
  | a == b    = return ()
  | otherwise = logChange (Change what (render a) (render b))

changesOkList :: (String -> (a -> String) -> Check a)
              -> String -> (a -> String) -> Check [a]
changesOkList changesOkElem what render = go
  where
    go []     []     = return ()
    go (a:_)  []     = logChange (Change ("added "   ++ what) (render a) "")
    go []     (b:_)  = logChange (Change ("removed " ++ what) "" (render b))
    go (a:as) (b:bs) = changesOkElem what render a b >> go as bs

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

