module Main where

import Control.Monad (unless)
import Distribution.Server.Features.Browse (StartIndex(..), NumElems(..), paginate, PaginationConfig(..))
import System.Exit (die)

main :: IO ()
main = do
  let res = paginate $ PaginationConfig 10 0
  unless (res == Just (StartIndex 0, NumElems 10)) $
    die $ "Mismatch 1 " ++ show res

  -- We don't want to claim that the page 0 is ever out of bounds,
  -- since it is normal to request page 0 of a listing with 0 results.
  let res = paginate $ PaginationConfig 0 0
  unless (res == Just (StartIndex 0, NumElems 0)) $
    die $ "Mismatch 2 " ++ show res

  let res = paginate $ PaginationConfig 10 1
  unless (res == Nothing) $
    die $ "Mismatch 3 " ++ show res

  let res = paginate $ PaginationConfig 11 1
  unless (res == Just (StartIndex 10, NumElems 1)) $
    die $ "Mismatch 4 " ++ show res

  let res = paginate $ PaginationConfig 9 0
  unless (res == Just (StartIndex 0, NumElems 9)) $
    die $ "Mismatch 5 " ++ show res

  let res = paginate $ PaginationConfig 20 0
  unless (res == Just (StartIndex 0, NumElems 10)) $
    die $ "Mismatch 6 " ++ show res

  let res = paginate $ PaginationConfig 20 1
  unless (res == Just (StartIndex 10, NumElems 10)) $
    die $ "Mismatch 7 " ++ show res
