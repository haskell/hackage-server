{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards, BangPatterns #-}
module Distribution.Server.Features.EditCabalFiles (
    initEditCabalFilesFeature

  , diffCabalRevisionsByteString
  , Change(..)
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.Templating

import Distribution.Server.Features.Users
import Distribution.Server.Features.Core
import Distribution.Server.Packages.Types
import Distribution.Server.Features.Upload

import Distribution.Package
import Distribution.Text (display)
import Distribution.Parsec ( showPError )
import Distribution.Server.Util.ParseSpecVer
import Distribution.Server.Util.CabalRevisions
         (Change(..), diffCabalRevisions, insertRevisionField)
import Text.StringTemplate.Classes (SElem(SM))

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS.L
import qualified Data.Map as Map
import Data.Time (getCurrentTime)

-- | A feature to allow editing cabal files without uploading new tarballs.
--
initEditCabalFilesFeature :: ServerEnv
                          -> IO (UserFeature
                              -> CoreFeature
                              -> UploadFeature
                              -> IO HackageFeature)
initEditCabalFilesFeature env@ServerEnv{ serverTemplatesDir,
                                         serverTemplatesMode } = do
    -- Page templates
    templates <- loadTemplates serverTemplatesMode
                   [serverTemplatesDir, serverTemplatesDir </> "EditCabalFile"]
                   ["cabalFileEditPage.html", "cabalFilePublished.html"]

    return $ \user core upload -> do
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
  , featureReloadFiles = reloadTemplates templates
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
          [ "pkgid"     $= pkgid
          , "cabalfile" $= insertRevisionField (pkgNumRevisions pkg)
                             (cabalFileByteString (pkgLatestCabalFileText pkg))
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
        let oldVersion = cabalFileByteString (pkgLatestCabalFileText pkg)
        newRevision <- getCabalFile
        shouldPublish <- getPublish
        case diffCabalRevisionsByteString oldVersion newRevision of
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
                  [ "pkgid"     $= pkgid
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
             [ "pkgid"     $= pkgid
             , "cabalfile" $= cabalFile
             , "publish"   $= publish
             , "errors"    $= errors
             , "changes"   $= changes
             ]


-- | Wrapper around 'diffCabalRevisions' which operates on
-- 'ByteString' decoded with lenient UTF8 and with any leading BOM
-- stripped.
diffCabalRevisionsByteString :: ByteString -> ByteString -> Either String [Change]
diffCabalRevisionsByteString oldRevision newRevision =
    maybe (diffCabalRevisions (BS.L.toStrict oldRevision) (BS.L.toStrict newRevision))
          Left
          parseSpecVerCheck
  where
    -- HACK-Alert
    --
    -- make sure the parseSpecVer heuristic agrees with the full parser.
    -- Note that diffCabalRevisions parses the newRevision a second time.
    parseSpecVerCheck = case parseGenericPackageDescriptionChecked newRevision of
       (True, _, Right _)      -> Nothing -- parsing successful
       (_, _, Left (_, err:_)) -> Just $ showPError "" err -- TODO: show all errors
       (_, _, Left (_, []))    -> Just "Parsing failed"
       (False, _, Right _)     -> Just "The 'cabal-version' field could not be properly parsed"

-- orphan
instance ToSElem Change where
  toSElem (Change severity what0 from to) = SM . Map.fromList $
        [ ("what",     toSElem what)
        , ("severity", toSElem (show severity))
        ] ++
        [ ("from",     toSElem from)   | not (null from) ] ++
        [ ("to",       toSElem to)     | not (null to) ]
    where
      -- TODO/FIXME: stringly hack
      what = case what0 of
        ('a':'d':'d':'e':'d':_)         -> 'A' : tail what0
        ('r':'e':'m':'o':'v':'e':'d':_) -> 'R' : tail what0
        _                               -> "Changed " ++ what0
