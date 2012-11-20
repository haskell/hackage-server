
{-|

Functions and combinators to expose functioanlity buiding
on happstack bit is not really specific to any one area
of Hackage.

-}

module Distribution.Server.Util.Happstack (
    rqRealMethod,
    methodOverrideHack,

    remainingPath,
    remainingPathString,
    mime,
    consumeRequestBody
  ) where

import Happstack.Server
import qualified Happstack.Server.SURI as SURI
import Happstack.Server.Internal.Monads
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans (MonadIO(..))
import System.FilePath.Posix (takeExtension, (</>))
import Control.Monad (liftM)

import System.IO.Unsafe (unsafePerformIO)


-- | Allows a hidden '_method' field on a form to override the apparent
-- method of a request. Useful until we can standardise on HTML 5.
methodOverrideHack :: MonadIO m => ServerPartT m a -> ServerPartT m a
methodOverrideHack rest
  = withDataFn (look "_method") $ \mthdStr ->
      let mthd = read mthdStr
      in localRq (\req -> req { rqMethod = mthd }) rest

-- | For use with 'methodOverrideHack': tries to report the original method
-- of a request before the hack was applied.
rqRealMethod :: Request -> Method
rqRealMethod rq = fromMaybe (rqMethod rq) $ unsafePerformIO $ runServerPartT_hack rq $
    withDataFn (liftM (not . null) $ lookInputs "_method") $ \mthd_exists ->
      return $ if mthd_exists then POST else rqMethod rq

runServerPartT_hack :: Monad m => Request -> ServerPartT m a -> m (Maybe a)
runServerPartT_hack rq mx
  = liftM (\res -> case res of
                     Nothing           -> Nothing
                     Just (Left _,  _) -> Nothing
                     Just (Right x, _) -> Just x)
          (ununWebT (runReaderT (unServerPartT mx) rq))


-- |Passes a list of remaining path segments in the URL. Does not
-- include the query string. This call only fails if the passed in
-- handler fails.
remainingPath :: Monad m => ([String] -> ServerPartT m a) -> ServerPartT m a
remainingPath handle = do
    rq <- askRq
    localRq (\newRq -> newRq{rqPaths=[]}) $ handle (rqPaths rq)

-- | Gets the string without altering the request.
remainingPathString :: Monad m => ServerPartT m String
remainingPathString = do
    strs <- liftM rqPaths askRq
    return $ if null strs then "" else foldr1 (</>) . map SURI.escape $ strs

-- |Returns a mime-type string based on the extension of the passed in
-- file.
mime :: FilePath -> String
mime x  = Map.findWithDefault "text/plain" (drop 1 (takeExtension x)) mimeTypes


-- | Get the raw body of a PUT or POST request.
--
-- Note that for performance reasons, this consumes the data and it cannot be
-- called twice.
--
consumeRequestBody :: Happstack m => m RqBody
consumeRequestBody = do
    mRq <- takeRequestBody =<< askRq
    case mRq of
      Nothing -> escape $ internalServerError $ toResponse
                   "consumeRequestBody cannot be called more than once."
      Just rq -> return rq
