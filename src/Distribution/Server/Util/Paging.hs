{-# LANGUAGE NamedFieldPuns #-}
module Distribution.Server.Util.Paging where
import Text.XHtml (URL)

-- Could design this way better
-- Need to check that paging math comes out right
data PaginatedConf = PaginatedConf
  { currPage :: Int,
    pageSize :: Int,
    totalAmount :: Int
  }

totalPages :: PaginatedConf -> Int
totalPages PaginatedConf {pageSize, totalAmount} = totalAmount `div` pageSize

createConf :: Int -> Int -> [a] -> PaginatedConf
createConf page pageSize xs = PaginatedConf page pageSize (length xs)

paginate :: PaginatedConf -> [a] -> [a]
paginate PaginatedConf {currPage, pageSize} = take pageSize . drop toDrop
  where
    toDrop = pageSize * pred currPage

hasNext,hasPrev :: PaginatedConf -> Bool
hasNext pc@PaginatedConf{currPage} = currPage < totalPages pc
hasPrev PaginatedConf {currPage} = currPage >= 1


allPagedURLS :: URL -> PaginatedConf -> [URL]
allPagedURLS base pc = (\p -> base ++ "?page=" ++ show p ++ "&pageSize=" ++ (show . pageSize) pc) 
    <$> [1..totalPages pc]