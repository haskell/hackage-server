{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}

module Distribution.Server.Features.Documentation.State where

import Distribution.Package
import Distribution.Server.Framework.BlobStorage (BlobId)
import Data.TarIndex (TarIndex)

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable
import Control.Monad.Reader
import qualified Control.Monad.State as State

import qualified Data.Map as Map

---------------------------------- Documentation
data Documentation = Documentation {
     documentation :: Map.Map PackageIdentifier (BlobId, TarIndex)
   } deriving (Typeable, Show)

deriveSafeCopy 0 'base ''Documentation

initialDocumentation :: Documentation
initialDocumentation = Documentation Map.empty

lookupDocumentation :: PackageIdentifier -> Query Documentation (Maybe (BlobId, TarIndex))
lookupDocumentation pkgId
    = do m <- asks documentation
         return $ Map.lookup pkgId m

hasDocumentation :: PackageIdentifier -> Query Documentation Bool
hasDocumentation pkgId
    = lookupDocumentation pkgId >>= \x -> case x of
         Just{} -> return True
         _      -> return False

insertDocumentation :: PackageIdentifier -> BlobId -> TarIndex -> Update Documentation ()
insertDocumentation pkgId blob index
    = State.modify $ \doc -> doc {documentation = Map.insert pkgId (blob, index) (documentation doc)}

getDocumentation :: Query Documentation Documentation
getDocumentation = ask

-- |Replace all existing documentation
replaceDocumentation :: Documentation -> Update Documentation ()
replaceDocumentation = State.put

makeAcidic ''Documentation ['insertDocumentation
                           ,'lookupDocumentation
                           ,'hasDocumentation
                           ,'getDocumentation
                           ,'replaceDocumentation
                           ]

