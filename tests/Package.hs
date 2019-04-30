
module Package (mkPackage) where

import Codec.Archive.Tar
import Codec.Archive.Tar.Entry
import Codec.Compression.GZip
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char
import System.FilePath

mkPackage :: String -> (FilePath,   -- Tar filename
                        String,     -- Tar file content
                        FilePath,   -- Cabal filename in index
                        String,     -- Cabal file content
                        FilePath,   -- Haskell filename in source tree
                        String)     -- Haskell file content
mkPackage name = (name ++ "-1.0.0.0.tar.gz",               BS.unpack targz,
                  name ++ "/1.0.0.0/" ++ name ++ ".cabal", cabalFile,
                  modName <.> "hs",                        modFile)
    where targz = compress tar
          tar = write entries
          entries = [directoryEntry (mkTarPath dir),
                     cabalEntry,
                     modEntry]
          dir = name ++ "-1.0.0.0"
          modName = headToUpper name
          cabalEntry = fileEntry (mkTarPath (dir </> name <.> "cabal"))
                                 (BS.pack cabalFile)
          modEntry = fileEntry (mkTarPath (dir </> modName <.> "hs"))
                               (BS.pack modFile)
          cabalFile = unlines [
                          "name:          " ++ name,
                          "version:       1.0.0.0",
                          "synopsis:      test package " ++ name,
                          "cabal-version: 2.0",
                          "build-type:    Simple",
                          "license:       BSD3",
                          "category:      MyCategory",
                          "",
                          "Library {",
                          "    default-language: Haskell2010",
                          "    exposed-modules: " ++ modName,
                          "}"]
          modFile = unlines [
                        "module " ++ modName ++ " where",
                        "f" ++ name ++ " :: () -> ()",
                        "f" ++ name ++ " () = ()"]

mkTarPath :: FilePath -> TarPath
mkTarPath fp = case toTarPath False fp of
               Left err -> error err
               Right tp -> tp

headToUpper :: String -> String
headToUpper [] = []
headToUpper (x : xs) = toUpper x : xs

