{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

module Distribution.Server.Features.Documentation.State where

import Distribution.Package
import Distribution.Server.Framework.BlobStorage (BlobId)
import Data.TarIndex () -- For SafeCopy instances
import Distribution.Server.Framework.MemSize

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State

import qualified Data.Map as Map

---------------------------------- Documentation
data Documentation = Documentation {
     documentation :: !(Map.Map PackageIdentifier BlobId)
   } deriving (Typeable, Show, Eq)

deriveSafeCopy 0 'base ''Documentation

instance MemSize Documentation where
    memSize (Documentation a) = memSize1 a

initialDocumentation :: Documentation
initialDocumentation = Documentation Map.empty

lookupDocumentation :: PackageIdentifier -> Query Documentation (Maybe BlobId)
lookupDocumentation pkgId
    = do m <- asks documentation
         return $ Map.lookup pkgId m

hasDocumentation :: PackageIdentifier -> Query Documentation Bool
hasDocumentation pkgId
    = lookupDocumentation pkgId >>= \x -> case x of
         Just{} -> return True
         _      -> return False

insertDocumentation :: PackageIdentifier -> BlobId -> Update Documentation ()
insertDocumentation pkgId blob
    = State.modify $ \doc -> doc {documentation = Map.insert pkgId blob (documentation doc)}

removeDocumentation :: PackageIdentifier -> Update Documentation ()
removeDocumentation pkgId
    = State.modify $ \doc -> doc {documentation = Map.delete pkgId (documentation doc)}

getDocumentation :: Query Documentation Documentation
getDocumentation = ask

-- |Replace all existing documentation
replaceDocumentation :: Documentation -> Update Documentation ()
replaceDocumentation = State.put

makeAcidic ''Documentation ['insertDocumentation
                           ,'removeDocumentation
                           ,'lookupDocumentation
                           ,'hasDocumentation
                           ,'getDocumentation
                           ,'replaceDocumentation
                           ]

