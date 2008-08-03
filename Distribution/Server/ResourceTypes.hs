{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Server.ResourceTypes
-- Copyright   :  (c) David Himmelstrup 2008
--                    Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Types for various kinds of resources we serve, xml, package tarballs etc.
-----------------------------------------------------------------------------
module Distribution.Server.ResourceTypes where

import HAppS.Server (ToMessage(..))

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Lazy
import qualified Text.RSS as RSS
import Text.RSS
         ( RSS )
import qualified Text.RSS as RSS
         ( rssToXML, showXML )

newtype Tarball = Tarball BS.Lazy.ByteString

instance ToMessage Tarball where
    toContentType _ = BS.pack "application/gzip"
    toMessage (Tarball bs) = bs


newtype CabalFile = CabalFile BS.Lazy.ByteString

instance ToMessage CabalFile where
    toContentType _ = BS.pack "text/plain"
    toMessage (CabalFile bs) = bs

instance ToMessage RSS where
    toContentType _ = BS.pack "application/rss+xml"
    toMessage = BS.Lazy.pack . RSS.showXML . RSS.rssToXML
