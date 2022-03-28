{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BlockArguments #-}
module Main where

import Prelude hiding (Ordering(..))

import Control.Monad.State.Lazy
import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Time (fromGregorian, nominalDay)
import System.Exit (die)

import Distribution.Server.Features.Browse.Parsers

assertEqual :: forall a b. (Eq a, Eq b, Show a, Show b) => Either a b -> b -> StateT Int IO ()
assertEqual actual onlyRight = do
  let reference :: Either a b
      reference = Right onlyRight
  if actual /= reference
     then do
       lift do
         putStrLn "Expected"
         print reference
         putStrLn "But got"
         print actual
       gotten <- get
       lift . die $ "Failed test " <> show gotten <> " (zero-indexed)"
     else modify (+1)

assertParses :: Text -> [Condition] -> StateT Int IO ()
assertParses searchString = assertEqual (parseOnly conditions searchString)

main :: IO ()
main = do
  let inp = "   dsa( downloads < 100 )( rating > 5.2) test john (lastUpload /= 2000-02-29)"
      ref =
        [ SearchTermCond "dsa"
        , FilterCond (DownloadsFilter (LT, 100))
        , FilterCond (RatingFilter (GT, 5.2))
        , SearchTermCond "test"
        , SearchTermCond "john"
        , FilterCond (LastUploadFilter (NEQ, fromGregorian 2000 2 29))
        ]
   in flip evalStateT 0 do
        assertEqual (parseOnly searchTerms "test donkey") [ SearchTermCond "test", SearchTermCond "donkey" ]
        assertEqual (parseOnly filterOrSearchTerms "(test donkey)") [ SearchTermCond "test", SearchTermCond "donkey" ]
        assertParses "(test donkey)" [ SearchTermCond "test", SearchTermCond "donkey" ]
        assertParses "test donkey" [ SearchTermCond "test", SearchTermCond "donkey" ]
        assertParses "test  donkey" [ SearchTermCond "test", SearchTermCond "donkey" ]
        assertParses "test () donkey" [ SearchTermCond "test", SearchTermCond "donkey" ]
        assertParses "test (donkey)" [ SearchTermCond "test", SearchTermCond "donkey" ]
        assertParses "test1 (donkey1)" [ SearchTermCond "test1", SearchTermCond "donkey1" ]
        assertParses "(test  donkey)" [ SearchTermCond "test", SearchTermCond "donkey" ]
        assertParses "(downloads<=10)" [ FilterCond (DownloadsFilter (LTE, 10)) ]
        assertParses "(dl<=10)" [ SearchTermCond "dl<=10" ]
        assertParses "(lastUpload!=9999-12-31)" [FilterCond (LastUploadFilter (NEQ, fromGregorian 9999 12 31))]
        assertParses "(maintainer:EdwardKmett)" [FilterCond (MaintainerFilter "EdwardKmett")]
        assertParses "(maintainer:23skidoo)" [FilterCond (MaintainerFilter "23skidoo")]
        assertParses "(tag:network)" [FilterCond (TagFilter "network")]
        assertParses "(ageOfLastUpload<5y)" [FilterCond (AgeLastULFilter (LT, nominalDay * 365.25 * 5))]
        assertParses "(ageOfLastUpload<0.00001d)" [FilterCond (AgeLastULFilter (LT, nominalDay * 0.00001))]
        assertParses "(rating<=NaN)" [ SearchTermCond "rating<=NaN" ]
        assertParses "(rating<=-1)" [ SearchTermCond "rating<=-1" ]
        assertParses "(rating<=-0)" [ FilterCond (RatingFilter (LTE, 0)) ]
        assertParses "(downloads<-1)" [ SearchTermCond "downloads<-1" ]
        assertParses "(not maintainer:EdwardKmett)" [ FilterCond (Not (MaintainerFilter "EdwardKmett")) ]
        assertParses "(not not maintainer:EdwardKmett)" [ SearchTermCond "not", SearchTermCond "not", SearchTermCond "maintainer:EdwardKmett" ]
        assertParses "(deprecated:true)" [ FilterCond (DeprecatedFilter OnlyDeprecated) ]
        assertParses "(deprecated:yes)" [ FilterCond (DeprecatedFilter OnlyDeprecated) ]
        assertParses "(deprecated:false)" [ FilterCond (DeprecatedFilter ExcludeDeprecated) ]
        assertParses "(deprecated:no)" [ FilterCond (DeprecatedFilter ExcludeDeprecated) ]
        assertParses "(deprecated:any)" [ FilterCond (DeprecatedFilter Don'tCareAboutDeprecated) ]
        assertParses "" []
        assertParses inp ref
