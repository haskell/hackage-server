{-# LANGUAGE NamedFieldPuns #-}

module Distribution.Server.Util.Paging 
(
  totalPages,
  createConf,
  hasNext,
  hasPrev,
  paginate,
  pageIndexStart,
  pageIndexRange,
  pageIndexEnd,
  allPagedURLs,
  nextURL,
  prevURL,
  toURL,
  pagingInfo,
  PaginatedConfiguration(..),
)
where
import Text.XHtml (URL)
import Data.List (genericTake, genericDrop, genericLength)

-- This could be better designed, perhaps turning PaginatedConfiguration into a 
-- function that returns the paging info and the paged data
data PaginatedConfiguration = PaginatedConfiguration
  { currPage :: Int,
    pageSize :: Int,
    totalAmount :: Int
  }

-- Assumes pageSize isn't 0, not the best design
totalPages :: PaginatedConfiguration -> Int
totalPages PaginatedConfiguration {pageSize, totalAmount} = 
    case totalAmount `quotRem` pageSize of
      (x,r)
        | r == 0 -> x
        | otherwise -> succ x

createConf :: Int -> Int -> [a] -> PaginatedConfiguration
createConf page pageSize xs = PaginatedConfiguration page pageSize (genericLength xs)

paginate :: PaginatedConfiguration -> [a] -> [a]
paginate PaginatedConfiguration {currPage, pageSize} = genericTake pageSize . genericDrop  toDrop
  where
    toDrop =  pageSize * pred currPage

hasNext,hasPrev :: PaginatedConfiguration -> Bool
hasNext pc@PaginatedConfiguration{currPage} =  currPage < totalPages pc
hasPrev PaginatedConfiguration {currPage} =  currPage > 1

-- | Returns the index positions that the current PaginatedConfiguration would show (Starts at 1)
pageIndexRange :: PaginatedConfiguration -> (Int, Int)
pageIndexRange conf@PaginatedConfiguration{currPage, pageSize, totalAmount} = (start, end)
  where start = succ $  currPage *  pageSize -  pageSize
        end = if  currPage == totalPages conf then totalAmount else  currPage *  pageSize

pageIndexStart, pageIndexEnd ::  PaginatedConfiguration -> Int
pageIndexStart = fst . pageIndexRange
pageIndexEnd = snd . pageIndexRange


allPagedURLs :: URL -> PaginatedConfiguration -> [URL]
allPagedURLs base pc = toURL base . (\x -> pc{currPage=x}) <$> [1..totalPages pc]


-- | Converts the PaginatedConfiguration to a URL, Assumes no query params in url
toURL :: URL -> PaginatedConfiguration -> URL
toURL base PaginatedConfiguration{currPage, pageSize} = base ++ "?page=" ++ show currPage ++ "&pageSize=" ++ show pageSize

nextURL :: URL -> PaginatedConfiguration -> Maybe URL
nextURL base conf@PaginatedConfiguration {currPage}
  |  page > totalPages conf = Nothing
  | otherwise = Just $ toURL base conf{currPage = page}
  where page = succ currPage 

prevURL :: URL -> PaginatedConfiguration -> Maybe URL
prevURL base conf@PaginatedConfiguration {currPage}
  | page < 1 = Nothing
  | otherwise = Just $ toURL base conf{currPage=page}
  where page = pred currPage 


pagingInfo :: PaginatedConfiguration -> String
pagingInfo pc@PaginatedConfiguration{totalAmount} = "Showing " ++ show start ++ " to " 
  ++ show end ++ " of " ++ show totalAmount ++ endingText
  where (start, end) = pageIndexRange pc
        endingText = if pageAmount > 0 then " entries" else " entry"
        pageAmount = end - start -- Starts Indexing at 1
