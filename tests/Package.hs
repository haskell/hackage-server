
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
                        String)     -- Cabal file content
mkPackage name
    = let (entries, cabalFile) = mkEntries name
          tar                  = write entries
          targz                = compress tar
      in (name ++ "-1.0.0.0.tar.gz", BS.unpack targz,
          name ++ "/1.0.0.0/" ++ name ++ ".cabal", cabalFile)

mkTarPath :: FilePath -> TarPath
mkTarPath fp = case toTarPath False fp of
               Left err -> error err
               Right tp -> tp

mkEntries :: String -> ([Entry], String)
mkEntries name = (entries, cabalFile)
    where entries = [directoryEntry (mkTarPath dir),
                     cabalEntry,
                     modEntry]
          dir = name ++ "-1.0.0.0"
          modName = headToUpper name
          cabalEntry = fileEntry (mkTarPath (dir </> name <.> "cabal"))
                                 (BS.pack cabalFile)
          modEntry = fileEntry (mkTarPath (dir </> modName <.> "cabal"))
                               (BS.pack modFile)
          cabalFile = unlines [
                          "name:          " ++ name,
                          "version:       1.0.0.0",
                          "synopsis:      test package " ++ name,
                          "cabal-version: >= 1.2",
                          "build-type:    Simple",
                          "license:       BSD3",
                          "category:      MyCategory",
                          "",
                          "Library {",
                          "    exposed-modules: " ++ modName,
                          "}"]
          modFile = unlines [
                        "module " ++ modName ++ " where",
                        "f" ++ name ++ " :: () -> ()",
                        "f" ++ name ++ " () = ()"]

headToUpper :: String -> String
headToUpper [] = []
headToUpper (x : xs) = toUpper x : xs

