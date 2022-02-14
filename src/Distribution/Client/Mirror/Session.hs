-- TODO: There is quite a bit of duplication between the various .Mirror.*
-- modules and Distribution.Client. This should be removed where possible.
-- (One symptom of this is the frequent need to use explicit import or hiding
-- lists for imports of Distribution.Client in the Mirror.* modules.)
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.Client.Mirror.Session (
    -- * MirrorSession
    MirrorSession -- Opaque
  , runMirrorSession
  , mirrorError
  , mirrorAskHttpLib
  , liftCont
  , Unlift(..)
  , askUnlift
  , mirrorFinally
    -- * Errors
  , MirrorError(..)
  , Entity(..)
  , ErrorState(..)
  , GetError(..)
  , ErrorResponse(..)
    -- ** Utility
  , mkErrorResponse
  , formatMirrorError
  , formatEntity
  , formatIOError
  , formatGetError
  , formatErrorResponse
    -- * Events
  , MirrorEvent(..)
  , notifyResponse
    -- * HTTP session
  , HttpSession
    -- ** Specific HttpSession actions
  , downloadFile
  , downloadFile'
  , requestGET
  , requestPUT
    -- ** Embedding HTTP session into mirror session
  , browserAction
  , browserActions
    -- * Logging
  , warn
    -- * Re-exports
  , throwError
  , liftIO
  ) where

-- stdlib
import Distribution.Server.Prelude

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import Data.IORef
import Data.List (isPrefixOf)
import Data.Set (Set)
import Data.Typeable (cast)
import Network.Browser
import Network.HTTP
import Network.URI (URI)
import System.Directory
import System.FilePath
import System.IO.Error
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set             as Set

-- Cabal
import Distribution.Package
import Distribution.Text
import Distribution.Verbosity
import Distribution.Simple.Utils (wrapText)

-- hackage
import Distribution.Server.Util.Parse (unpackUTF8)

-- hackage-security
import qualified Hackage.Security.Client                         as Sec
import qualified Hackage.Security.Client.Repository.HttpLib      as Sec
import qualified Hackage.Security.Client.Repository.HttpLib.HTTP as Sec.HTTP
import qualified Hackage.Security.Util.Pretty                    as Sec

{-------------------------------------------------------------------------------
  MirrorSession monad

  ERROR HANDLING STRATEGY

  There's two general classes of errors we have to consider:

   - systematic errors that apply to all packages
   - errors that only apply to individual packages

  For the first class we want to fail and not continue mirroring.

  For the second class we want to keep going and try mirroring other packages.
  In addition we want to remember the packages involved persistently so that
  we don't keep trying again and again to mirror packages that simply cannot
  be mirrored.

  We want the mirroring to be robust. So we don't want to have temporary or
  individual package errors make us fail overall.

  To add further complication, it can be hard to distinguish a systematic
  error from an individual per-package error. Our strategy may have to be
  complex and based on counting various events to make best guesses.

  Since we expect our error handling stategy we try to separate it from the
  main mirroring logic.

  We make a new monad to hold the plumbing for our strategy. We make it to
  be a state monad so that we can accumulating info about non-fatal errors in
  individual packages. We make it an error monad to deal with fatal errors.

  We layer state and error on top of the HTTP Browser monad. The layering
  order of the state vs error is such that errors do not roll back the changed
  state. This is because we want (at least the option) to keep the info about
  individual packages when we encounter a systematic error.

  So that's the plumbing. The actual policy/strategy is implemented by an
  action in the monad where we inform it of the interesting events, which is
  basically all the gets and puts, the packages and their http response codes.
  This policy action behaves like a state machine action, updating the monad
  state and if it decides it has to fail hard, by making use of error monad.
-------------------------------------------------------------------------------}

data MirrorInternalEnv = MirrorInternalEnv {
    mirrorVerbosity :: Verbosity
  , mirrorKeepGoing :: Bool
  , mirrorErrorRef  :: IORef ErrorState
  , mirrorBrowser   :: Sec.HTTP.Browser
  , mirrorHttpLib   :: Sec.HttpLib
  }

newtype MirrorSession a = MirrorSession {
    unMirror :: ReaderT MirrorInternalEnv IO a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           )

instance MonadReader (Verbosity, Bool) MirrorSession where
  ask       = MirrorSession $ do
                MirrorInternalEnv{..} <- ask
                return (mirrorVerbosity, mirrorKeepGoing)
  local f m = MirrorSession (local f' (unMirror m))
    where
      f' :: MirrorInternalEnv -> MirrorInternalEnv
      f' MirrorInternalEnv{..} = MirrorInternalEnv{
            mirrorVerbosity = verbosity'
          , mirrorKeepGoing = keepGoing'
          , ..
          }
        where
          (verbosity', keepGoing') = f (mirrorVerbosity, mirrorKeepGoing)

instance MonadState ErrorState MirrorSession where
  get   = MirrorSession $ do
            MirrorInternalEnv{..} <- ask
            liftIO $ readIORef mirrorErrorRef
  put x = MirrorSession $ do
            MirrorInternalEnv{..} <- ask
            liftIO $ writeIORef mirrorErrorRef x

runMirrorSession :: Verbosity -> Bool -> ErrorState -> MirrorSession a
                 -> IO (Either MirrorError a, ErrorState)
runMirrorSession verbosity keepGoing st (MirrorSession m) = do
    stRef <- newIORef st
    Sec.HTTP.withClient $ \browser httpLib -> do
      let internalEnv = MirrorInternalEnv {
              mirrorVerbosity = verbosity
            , mirrorKeepGoing = keepGoing
            , mirrorErrorRef  = stRef
            , mirrorBrowser   = browser
            , mirrorHttpLib   = httpLib
            }
      result <- catches (Right <$> runReaderT m internalEnv) [
          Handler $ \(ex :: MirrorError)    -> return $ Left ex
        , Handler $ \(ex :: IOError)        -> return $ Left (MirrorIOError ex)
        , Handler $ \(ex :: AsyncException) ->
            case ex of
              UserInterrupt -> return $ Left Interrupted
              _otherwise    -> throwIO ex
        ]
      st' <- readIORef stRef
      return (result, st')

mirrorError :: MirrorError -> MirrorSession a
mirrorError = liftIO . throwIO

newtype Unlift = Unlift { unlift :: forall a. MirrorSession a -> IO a }

-- | Unlifting from MirrorSession to IO (@monad-unlift@ style)
askUnlift :: MirrorSession Unlift
askUnlift = MirrorSession $ ReaderT $ \env ->
    return $ Unlift $ \act -> runReaderT (unMirror act) env

-- | Lift a continuation in IO to a continuation in MirrorSession
--
-- This is just a convenience wrapper around 'askRun'.
--
-- Another way to think about this type is
--
-- > liftCont :: ContT r IO a -> ContT r MirrorSession a
liftCont :: ((a -> IO            b) -> IO            b)
         -> ((a -> MirrorSession b) -> MirrorSession b)
liftCont f g = do
    run <- askUnlift
    liftIO $ f $ \a -> unlift run (g a)

mirrorFinally :: MirrorSession a -> MirrorSession b -> MirrorSession a
mirrorFinally a b = do
    run <- askUnlift
    liftIO $ unlift run a `finally` unlift run b

mirrorAskHttpLib :: MirrorSession Sec.HttpLib
mirrorAskHttpLib = MirrorSession $ do
    MirrorInternalEnv{..} <- ask
    return mirrorHttpLib

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data MirrorError = MirrorIOError IOError
                 | GetEntityError Entity GetError
                 | ParseEntityError Entity String String
                 | PutPackageError PackageId ErrorResponse
                 | Interrupted
                 | InvalidOption String
  deriving (Typeable,Show)

instance Exception MirrorError

data Entity = EntityIndex | EntityLog | EntityPackage PackageId
  deriving Show

data ErrorState = ErrorState {
                    es_missing      :: Set PackageId,
                    es_unmirrorable :: Set PackageId
                  }
  deriving Show

data GetError =
    GetError ErrorResponse
  | GetVerificationError Sec.VerificationError
  | GetRemoteError Sec.SomeRemoteError
  | GetInvalidPackage Sec.InvalidPackageException
  deriving Show

data ErrorResponse = ErrorResponse URI ResponseCode String (Maybe String)
  deriving Show

mkErrorResponse :: URI -> Response ByteString -> ErrorResponse
mkErrorResponse uri rsp =
    ErrorResponse uri (rspCode rsp) (rspReason rsp) mBody
  where
    mBody = case lookupHeader HdrContentType (rspHeaders rsp) of
      Just mimetype | "text/plain" `isPrefixOf` mimetype
                   -> Just (unpackUTF8 (rspBody rsp))
      _            -> Nothing

formatMirrorError :: MirrorError -> String
formatMirrorError (MirrorIOError      ioe)  = formatIOError ioe
formatMirrorError (GetEntityError entity rsp) =
    "Failed to download " ++ formatEntity entity
 ++ ",\n  " ++ formatGetError rsp
formatMirrorError (ParseEntityError entity uri theError) =
    "Error parsing " ++ formatEntity entity
 ++ " at " ++ uri ++ ": " ++ theError
formatMirrorError (PutPackageError pkgid rsp) =
    "Failed to upload package "  ++ display pkgid
 ++ ",\n  " ++ formatErrorResponse rsp
formatMirrorError Interrupted = error "formatMirrorError: Interrupted"
formatMirrorError (InvalidOption theError) =
    "Invalid option: " ++ theError

formatEntity :: Entity -> String
formatEntity EntityIndex           = "the package index"
formatEntity EntityLog             = "the package upload log"
formatEntity (EntityPackage pkgid) = "package " ++ display pkgid

formatIOError :: IOError -> String
formatIOError ioe
  | isUserError ioe = file ++ location ++ detail
  | otherwise       = show ioe
  where
    file         = case ioeGetFileName ioe of
                     Nothing   -> ""
                     Just path -> path ++ ": "
    location     = case ioeGetLocation ioe of
                    ""  -> ""
                    loc -> loc ++ ": "
    detail       = ioeGetErrorString ioe

formatGetError :: GetError -> String
formatGetError (GetError             theError) = formatErrorResponse theError
formatGetError (GetVerificationError theError) = Sec.pretty theError
formatGetError (GetRemoteError       theError) = Sec.pretty theError
formatGetError (GetInvalidPackage    theError) = Sec.pretty theError

formatErrorResponse :: ErrorResponse -> String
formatErrorResponse (ErrorResponse uri (a,b,c) reason mBody) =
    "HTTP error code " ++ show a ++ show b ++ show c
 ++ ", " ++ reason ++ "\n  " ++  show uri
 ++ maybe "" (('\n':) . unlines . map ("  "++) . lines . wrapText) mBody

{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

data MirrorEvent =
    GetIndexOk
  | GetPackageOk
  | GetPackageFailed GetError PackageId
  | PutPackageOk
  | PutPackageFailed ErrorResponse PackageId
  | PackageSkipped

notifyResponse :: MirrorEvent -> MirrorSession ()
notifyResponse e = do
    st <- get
    (verbosity, keepGoing) <- ask
    st' <- handleEvent verbosity keepGoing st
    put st'
  where
    handleEvent _ False st = case e of
      GetIndexOk     -> return st
      GetPackageOk   -> return st
      PutPackageOk   -> return st
      PackageSkipped -> return st
      GetPackageFailed rsp pkgid ->
        mirrorError (GetEntityError (EntityPackage pkgid) rsp)
      PutPackageFailed rsp pkgid ->
        mirrorError (PutPackageError pkgid rsp)

    handleEvent verbosity True st = case e of
      GetIndexOk     -> return st
      GetPackageOk   -> return st
      PutPackageOk   -> return st
      PackageSkipped -> return st
      GetPackageFailed rsp pkgid ->
        if getFailedPermanent rsp
          then do
            liftIO $ warn verbosity $
              formatMirrorError (GetEntityError (EntityPackage pkgid) rsp)
            return st {
                es_missing = Set.insert pkgid (es_missing st)
              }
          else
            mirrorError (GetEntityError (EntityPackage pkgid) rsp)
      PutPackageFailed rsp pkgid ->
        if putFailedPermanent rsp
          then do
            liftIO $ warn verbosity $
              formatMirrorError (PutPackageError pkgid rsp)
            return st {
                es_unmirrorable = Set.insert pkgid (es_unmirrorable st)
              }
          else
            mirrorError (PutPackageError pkgid rsp)

    -- Was this error when downloading a package permanent, or might it have
    -- been some kind of transient error and should we try again?
    --
    -- NOTE: Verification errors can be transient (for instance, if a timestamp
    -- was updated too late).
    getFailedPermanent :: GetError -> Bool
    getFailedPermanent (GetError resp) =
       case resp of
         ErrorResponse _ (4,0,4)    _ _ -> True
         ErrorResponse _ (4,1,0)    _ _ -> True
         ErrorResponse _ (4,5,1)    _ _ -> True
         ErrorResponse _ _otherCode _ _ -> False
    getFailedPermanent (GetRemoteError (Sec.SomeRemoteError theError)) =
       case cast theError of
         Just (Sec.HTTP.UnexpectedResponse _ (4,0,4)) -> True
         Just (Sec.HTTP.UnexpectedResponse _ (4,1,0)) -> True
         Just (Sec.HTTP.UnexpectedResponse _ (4,5,1)) -> True
         _otherwise                                   -> False
    getFailedPermanent (GetVerificationError _) = False
    getFailedPermanent (GetInvalidPackage _)    = True

    putFailedPermanent :: ErrorResponse -> Bool
    putFailedPermanent (ErrorResponse _ (4,0,0)    _ _) = True
    putFailedPermanent (ErrorResponse _ (4,0,4)    _ _) = True
    putFailedPermanent (ErrorResponse _ _otherCode _ _) = False

{-------------------------------------------------------------------------------
  HttpSession and actions
-------------------------------------------------------------------------------}

type HttpSession a = BrowserAction (HandleStream ByteString) a

downloadFile :: URI -> FilePath -> HttpSession (Maybe GetError)
downloadFile uri file = do
  out $ "downloading " ++ show uri ++ " to " ++ file
  metag <- liftIO $ getETag file
  case metag of
    Just etag -> do
      let headers = [mkHeader HdrIfNoneMatch (quote etag)]
      (_, rsp) <- request (Request uri GET headers BS.empty)
      case rspCode rsp of
        (3,0,4) -> do out $ file ++ " unchanged with ETag " ++ etag
                      return Nothing
        (2,0,0) -> do liftIO $ writeDowloadedFileAndEtag rsp
                      return Nothing
        _       -> return (Just (GetError (mkErrorResponse uri rsp)))

    Nothing -> do
      (_, rsp) <- request (Request uri GET [] BS.empty)
      case rspCode rsp of
        (2,0,0) -> do liftIO $ writeDowloadedFileAndEtag rsp
                      return Nothing
        _       -> return (Just (GetError (mkErrorResponse uri rsp)))

  where
    writeDowloadedFileAndEtag rsp = do
      BS.writeFile file (rspBody rsp)
      setETag file (unquote <$> findHeader HdrETag rsp)

downloadFile' :: URI -> FilePath -> HttpSession (Maybe GetError)
downloadFile' uri file = do
  out $ "downloading " ++ show uri ++ " to " ++ file
  rsp <- requestGET uri
  case rsp of
    Left  theError -> return (Just (GetError theError))
    Right content  -> do liftIO $ BS.writeFile file content
                         --TODO: check we wrote the expected length.
                         return Nothing

requestGET :: URI -> HttpSession (Either ErrorResponse ByteString)
requestGET uri = do
    (_, rsp) <- request (Request uri GET headers BS.empty)
    case rspCode rsp of
      (2,0,0) -> return (Right (rspBody rsp))
      _       -> return (Left  (mkErrorResponse uri rsp))
  where
    headers = []

requestPUT :: URI -> String -> ByteString -> HttpSession (Maybe ErrorResponse)
requestPUT uri mimetype body = do
    (_, rsp) <- request (Request uri PUT headers body)
    case rspCode rsp of
      (2,_,_) -> return Nothing
      _       -> return (Just (mkErrorResponse uri rsp))
  where
    headers = [ Header HdrContentLength (show (BS.length body))
              , Header HdrContentType mimetype ]

{-------------------------------------------------------------------------------
  Auxiliary functions used by HttpSession actions
-------------------------------------------------------------------------------}

getETag :: FilePath -> IO (Maybe String)
getETag file =
    catchJustDoesNotExistError
      (Just <$> readFile (file <.> "etag"))
      (\_ -> return Nothing)

setETag :: FilePath -> Maybe String -> IO ()
setETag file Nothing     = catchJustDoesNotExistError
                             (removeFile (file <.> "etag"))
                             (\_ -> return ())
setETag file (Just etag) = writeFile  (file <.> "etag") etag

catchJustDoesNotExistError :: IO a -> (IOError -> IO a) -> IO a
catchJustDoesNotExistError =
  catchJust (\e -> if isDoesNotExistError e then Just e else Nothing)

quote :: String -> String
quote s = '"' : s ++ ['"']

unquote :: String -> String
unquote ('"':s) = go s
  where
    go []       = []
    go ('"':[]) = []
    go (c:cs)   = c : go cs
unquote     s   = s

{-------------------------------------------------------------------------------
  Embedding HTTP session into mirror session
-------------------------------------------------------------------------------}

browserAction :: HttpSession a -> MirrorSession a
browserAction act = MirrorSession $ do
    MirrorInternalEnv{..} <- ask
    liftIO $ Sec.HTTP.withBrowser mirrorBrowser act

browserActions :: [HttpSession (Maybe err)] -> MirrorSession (Maybe err)
browserActions = foldr1 maybeThen . map browserAction
  where
    -- Bind for the strange not-quite-monad where errors are returned as
    -- (Just err) and success is returned as Nothing
    maybeThen :: Monad m => m (Maybe err) -> m (Maybe err) -> m (Maybe err)
    maybeThen p q = do
      res <- p
      case res of
        Just theError -> return (Just theError)
        Nothing       -> q

{-------------------------------------------------------------------------------
  Logging
-------------------------------------------------------------------------------}

warn :: Verbosity -> String -> IO ()
warn verbosity msg =
  when (verbosity >= normal) $
    putStrLn ("Warning: " ++ msg)
