{-# LANGUAGE PatternGuards #-}
module Main where

import Network.Browser
import Network.URI (URI(..), URIAuth(..))

import Distribution.Client
import Distribution.Server.Util.Merge
import Distribution.Package
import Distribution.Version
import Distribution.Text
import Distribution.Verbosity
import Distribution.Simple.Utils

import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS

import System.Environment
import System.Exit
import System.FilePath
import System.Directory
import System.Console.GetOpt


data MirrorOpts = MirrorOpts {
                    srcURI       :: URI,
                    dstURI       :: URI,
                    stateDir     :: FilePath,
                    selectedPkgs :: [PackageId]
                  }

main :: IO ()
main = topHandler $ do
  args <- getArgs
  (verbosity, opts) <- validateOpts args

  mirrorOnce verbosity opts


mirrorOnce :: Verbosity -> MirrorOpts -> IO ()
mirrorOnce verbosity opts = do

    let srcCacheFile = stateDir opts </> mkCacheFileName (srcURI opts)
        dstCacheFile = stateDir opts </> mkCacheFileName (dstURI opts)
    when (srcCacheFile == dstCacheFile) $
      die "source and destination cache files clash"

    createDirectoryIfMissing False (stateDir opts)

    httpSession verbosity $ do

      srcIndex <- downloadIndex (srcURI opts) srcCacheFile
      dstIndex <- downloadIndex (dstURI opts) dstCacheFile

      let pkgsMissingFromDest = diffIndex srcIndex dstIndex
          pkgsToMirror
            | null (selectedPkgs opts) = pkgsMissingFromDest
            | otherwise                = subsetIndex (selectedPkgs opts)
                                                     pkgsMissingFromDest

      ioAction $ notice verbosity $ show (length pkgsToMirror) ++ " packages to mirror."

      mirrorPackages verbosity opts pkgsToMirror

  where
    mkCacheFileName :: URI -> FilePath
    mkCacheFileName URI { uriAuthority = Just auth }
                        = makeValid (uriRegName auth ++ uriPort auth)
    mkCacheFileName uri = error $ "unexpected URI " ++ show uri


diffIndex :: [PkgIndexInfo] -> [PkgIndexInfo] -> [PkgIndexInfo]
diffIndex as bs =
    [ pkg | OnlyInLeft pkg <- mergeBy (comparing mirrorPkgId)
                                       (sortBy (comparing mirrorPkgId) as)
                                       (sortBy (comparing mirrorPkgId) bs) ]
  where
    mirrorPkgId (PkgIndexInfo pkgid _ _ _) = pkgid

subsetIndex :: [PackageId] -> [PkgIndexInfo] -> [PkgIndexInfo]
subsetIndex pkgids =
    filter (\(PkgIndexInfo pkgid' _ _ _) -> anyMatchPackage pkgids pkgid')
  where
    anyMatchPackage :: [PackageId] -> PackageId -> Bool
    anyMatchPackage pkgids pkgid' =
      any (\pkgid -> matchPackage pkgid pkgid') pkgids

    matchPackage :: PackageId -> PackageId -> Bool
    matchPackage (PackageIdentifier name  (Version [] _))
                 (PackageIdentifier name' _             ) = name == name'
    matchPackage pkgid pkgid' = pkgid == pkgid'


mirrorPackages :: Verbosity -> MirrorOpts -> [PkgIndexInfo] -> HttpSession ()
mirrorPackages verbosity opts pkgsToMirror = do

    let credentials = extractURICredentials (dstURI opts)
    setAuthorityGen (provideAuthInfo (dstURI opts) credentials)

    sequence_
      [ do let srcBasedir
                 | isOldHackageURI (srcURI opts) = "packages" </> "archive"
                 | otherwise                     = "packages"
               srcPkgFile = display (packageName pkgid) </>
                            display (packageVersion pkgid) </>
                            display pkgid <.> "tar.gz"
               src     = srcURI   opts <//> srcBasedir </> srcPkgFile
               dst     = dstURI   opts <//> "package" </> display pkgid
               pkgfile = stateDir opts </>  display pkgid <.> "tar.gz"

           ioAction $ notice verbosity $ "\nmirroring " ++ display pkgid
           ok <-     downloadFile' src         pkgfile
           when ok $ putPackage    dst pkginfo pkgfile

      | pkginfo@(PkgIndexInfo pkgid _ _ _) <- pkgsToMirror ]

putPackage :: URI -> PkgIndexInfo -> FilePath -> HttpSession ()
putPackage baseURI (PkgIndexInfo pkgid _mtime _muname _muid) pkgFile = do
    putPackageTarball

{-  case mtime of
      Nothing   -> return ()
      Just time -> putPackageUploadTime -}
  where
    putPackageTarball = do
      pkgContent <- ioAction $ BS.readFile pkgFile
      let pkgURI = baseURI <//> display pkgid <.> "tar.gz"
      requestPUT pkgURI "application/x-gzip" pkgContent
{-
    putPackageUploadTime time = do
      (_, rsp) <- request (requestPUT pkgURI "text/plain" timeStr)
      where
        timeStr = formatTime defaultTimeLocale "%c" time
    putPackageUploadUser = do
      where
        nameStr = display uname
-}

-------------------------
-- Command line handling
-------------------------

data MirrorFlags = MirrorFlags {
    flagCacheDir  :: Maybe FilePath,
    flagVerbosity :: Verbosity,
    flagHelp      :: Bool
}

emptyMirrorFlags :: MirrorFlags
emptyMirrorFlags = MirrorFlags Nothing normal False

mirrorFlagDescrs :: [OptDescr (MirrorFlags -> MirrorFlags)]
mirrorFlagDescrs =
  [ Option ['h'] ["help"]
      (NoArg (\opts -> opts { flagHelp = True }))
      "Show this help text"

  , Option ['v'] []
      (NoArg (\opts -> opts { flagVerbosity = moreVerbose (flagVerbosity opts) }))
      "Verbose mode (can be listed multiple times e.g. -vv)"

  , Option [] ["cache-dir"]
      (ReqArg (\dir opts -> opts { flagCacheDir = Just dir }) "DIR")
      "Where to put files during mirroring"
  ]

validateOpts :: [String] -> IO (Verbosity, MirrorOpts)
validateOpts args = do
    let (flags0, args', errs) = getOpt Permute mirrorFlagDescrs args
        flags = accum flags0 emptyMirrorFlags

    when (flagHelp flags) printUsage
    when (not (null errs)) (printErrors errs)

    case args' of
      (from:to:pkgstrs) -> case (validateHackageURI from, validateHackageURI to) of
        (Left err, _) -> die err
        (_, Left err) -> die err
        (Right fromURI, Right toURI) -> do
          pkgs <- case validatePackageIds pkgstrs of
            Left err   -> die err
            Right pkgs -> return pkgs

          return $ (,) (flagVerbosity flags) MirrorOpts {
            srcURI       = fromURI,
            dstURI       = toURI,
            stateDir     = fromMaybe "mirror-cache" (flagCacheDir flags),
            selectedPkgs = pkgs
          }

      _ -> do putStrLn $ "Expected two URLs, a source and destination.\n"
                      ++ "See hackage-mirror --help for details and an example."
              exitFailure

  where
    printUsage = do
      putStrLn $ usageInfo usageHeader mirrorFlagDescrs ++ helpExampleStr
      exitSuccess
    usageHeader = helpDescrStr
               ++ "Usage: hackage-mirror fromURL toURL [packages] [options]\n"
               ++ "Options:"
    printErrors errs = do
      putStrLn $ concat errs ++ "Try --help."
      exitFailure

    accum flags = foldr (flip (.)) id flags

helpDescrStr :: String
helpDescrStr = unlines
  [ "The hackage-mirror client copies packages from one hackage server to another."
  , "By default it copies over all packages that exist on the source but not on"
  , "the destination server. You can also select just specific packages to mirror."
  , "It is also possible to run the mirror in a continuous mode, giving you"
  , "nearly-live mirroring.\n"
  ]

helpExampleStr :: String
helpExampleStr = unlines
  [ "\nExample:"
  , "  Suppose we have:"
  , "  - source server: hackage.haskell.org"
  , "  - dest server:   localhost:8080"
  , "  Uploading packages almost always requires authentication, so suppose we have"
  , "  a user account for our mirror client with username 'foo' and password 'bar'."
  , "  We include the authentication details into the destination URL:"
  , "    http://foo:bar@localhost:8080/"
  , "  To test that it is working without actually syncing a Gb of data from"
  , "  hackage.haskell.org, we will specify to mirror only the 'zlib' package."
  , "  So overall we run:"
  , "    hackage-mirror http://hackage.haskell.org/ \\"
  , "                   http://foo:bar@localhost:8080/  zlib"
  , "  This will synchronise all versions of the 'zlib' package and then exit."
  ]
