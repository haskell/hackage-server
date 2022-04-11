{-# LANGUAGE NamedFieldPuns #-}
module Distribution.Server.Util.Paging where
import Text.XHtml (URL)

-- This could be designed better, perhaps turning PaginatedConf into a function that returns the paging info 
--  and the paged data
data PaginatedConf = PaginatedConf
  { currPage :: Int,
    pageSize :: Int,
    totalAmount :: Int
  }

totalPages :: PaginatedConf -> Int
totalPages PaginatedConf {pageSize, totalAmount} = 
    case totalAmount `quotRem` pageSize of
      (x,r)
        | r == 0 -> x
        | otherwise -> succ x

createConf :: Int -> Int -> [a] -> PaginatedConf
createConf page pageSize xs = PaginatedConf page pageSize (length xs)

paginate :: PaginatedConf -> [a] -> [a]
paginate PaginatedConf {currPage, pageSize} = take pageSize . drop toDrop
  where
    toDrop = pageSize * pred currPage

hasNext,hasPrev :: PaginatedConf -> Bool
hasNext pc@PaginatedConf{currPage} = currPage < totalPages pc
hasPrev PaginatedConf {currPage} = currPage > 1

-- | Returns the index positions that the current PaginatedConfiguration would show (Starts at 1)
pageIndexRange :: PaginatedConf -> (Int, Int)
pageIndexRange conf@PaginatedConf{currPage, pageSize, totalAmount} = (start, end)
  where start = succ $ (currPage * pageSize) - pageSize
        end = if currPage == totalPages conf then totalAmount else currPage * pageSize

pageIndexStart, pageIndexEnd ::  PaginatedConf -> Int
pageIndexStart = fst . pageIndexRange
pageIndexEnd = snd . pageIndexRange


allPagedURLS :: URL -> PaginatedConf -> [URL]
allPagedURLS base pc = (\p -> base ++ "?page=" ++ show p ++ "&pageSize=" ++ (show . pageSize) pc) 
    <$> [1..totalPages pc]