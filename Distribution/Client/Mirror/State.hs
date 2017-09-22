{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.Mirror.State (
    MirrorState(..)
  , MirrorEnv(..)
  , mirrorInit
  , savePackagesState
  , toErrorState
  , fromErrorState
  ) where

-- stdlib
import Control.Exception
import Control.Monad
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Network.URI
import System.Directory
import System.FilePath
import qualified Data.Set as Set

-- Cabal
import Distribution.Package
import Distribution.Simple.Utils hiding (warn)
import Distribution.Text
import Distribution.Verbosity

-- hackage
import Distribution.Client.Mirror.CmdLine
import Distribution.Client.Mirror.Config
import Distribution.Client.Mirror.Session

{-------------------------------------------------------------------------------
  Mirror state
-------------------------------------------------------------------------------}

data MirrorState = MirrorState {
                     ms_missingPkgs          :: Set PackageId,
                     ms_unmirrorablePkgs     :: Set PackageId
                   }
  deriving (Eq, Show)

data MirrorEnv = MirrorEnv {
                   me_srcCacheDir :: FilePath,
                   me_dstCacheDir :: FilePath,
                   me_missingPkgsFile      :: FilePath,
                   me_unmirrorablePkgsFile :: FilePath
                 }
  deriving Show

{-------------------------------------------------------------------------------
  Initialization and termination
-------------------------------------------------------------------------------}

mirrorInit :: Verbosity -> MirrorOpts -> IO (MirrorEnv, MirrorState)
mirrorInit verbosity opts = do

    let srcName     = preRepoName $ mirrorSource (mirrorConfig opts)
        dstName     = preRepoName $ mirrorTarget (mirrorConfig opts)
        srcCacheDir = stateDir opts </> srcName
        dstCacheDir = stateDir opts </> dstName

    when (srcCacheDir == dstCacheDir) $
      dieNoVerbosity "source and destination cache files clash"

    when (continuous opts == Just 0) $
      warn verbosity "A sync interval of zero is a seriously bad idea!"

    when (isCentralHackage (mirrorSource (mirrorConfig opts))
       && maybe False (<5) (continuous opts)) $
       dieNoVerbosity $ "Please don't hit the central hackage.haskell.org "
          ++ "more frequently than every 5 minutes."

    createDirectoryIfMissing False (stateDir opts)
    createDirectoryIfMissing False srcCacheDir
    createDirectoryIfMissing False dstCacheDir

    let missingPkgsFile      = stateDir opts
                           </> "missing-" ++ srcName
        -- being unmirrorable is a function of the combination of the source and
        -- destination, so the cache file uses both names.
        unmirrorablePkgsFile = stateDir opts
                           </> "unmirrorable-" ++ srcName ++ "-" ++ dstName
    missingPkgs      <- readPkgProblemFile missingPkgsFile
    unmirrorablePkgs <- readPkgProblemFile unmirrorablePkgsFile

    return ( MirrorEnv {
                 me_srcCacheDir          = srcCacheDir
               , me_dstCacheDir          = dstCacheDir
               , me_missingPkgsFile      = missingPkgsFile
               , me_unmirrorablePkgsFile = unmirrorablePkgsFile
               }
           , MirrorState {
                 ms_missingPkgs          = missingPkgs
               , ms_unmirrorablePkgs     = unmirrorablePkgs
               }
           )

isCentralHackage :: PreRepo -> Bool
isCentralHackage PreRepo{..} =
    let regName = fmap uriRegName . uriAuthority =<< preRepoURI
    in regName == Just "hackage.haskell.org"

savePackagesState :: MirrorEnv  -> MirrorState -> IO ()
savePackagesState (MirrorEnv _ _ missingPkgsFile unmirrorablePkgsFile)
                  (MirrorState missingPkgs unmirrorablePkgs) = do
  writePkgProblemFile missingPkgsFile      missingPkgs
  writePkgProblemFile unmirrorablePkgsFile unmirrorablePkgs

{-------------------------------------------------------------------------------
  Package problem file
-------------------------------------------------------------------------------}

readPkgProblemFile :: FilePath -> IO (Set PackageId)
readPkgProblemFile file = do
  exists <- doesFileExist file
  if exists
    then evaluate . Set.fromList
                  . catMaybes . map simpleParse . lines
                =<< readFile file
    else return Set.empty

writePkgProblemFile :: FilePath -> Set PackageId -> IO ()
writePkgProblemFile file =
  writeFile file . unlines . map display . Set.toList

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

toErrorState :: MirrorState -> ErrorState
toErrorState (MirrorState missing unmirrorable) =
  ErrorState missing unmirrorable

fromErrorState :: ErrorState -> MirrorState
fromErrorState (ErrorState missing unmirrorable) =
  MirrorState missing unmirrorable
