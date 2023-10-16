module Main where

import Data.Time (UTCTime(UTCTime), fromGregorian)

import Distribution.Server.Features.Vouch (VouchError(..), VouchSuccess(..), judgeVouch)
import Distribution.Server.Users.UserIdSet (fromList)
import Distribution.Server.Users.Types (UserId(UserId))

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

allTests :: TestTree
allTests = testGroup "VouchTest"
  [ testCase "happy path, vouch added, but more vouches needed" $ do
      let ref = Right (AddVouchIncomplete 1)
          voucher = UserId 1
          vouchee = UserId 2
      assertEqual "must match" ref $
        judgeVouch
          (fromList [voucher]) -- uploaders. Can't vouch if user is not a voucher
          (UTCTime (fromGregorian 2020 1 1) 0)
          vouchee
          [] -- vouchers for voucher. If this short enough, voucher is assumed to be old enough to vouch themselves.
          [] -- no existing vouchers
          voucher
  , testCase "happy path, vouch added, no more vouches needed" $ do
      let ref = Right AddVouchComplete
          voucher = UserId 1
          vouchee = UserId 2
          otherVoucherForVouchee = UserId 4
      assertEqual "must match" ref $
        judgeVouch
          (fromList [voucher])
          (UTCTime (fromGregorian 2020 1 1) 0)
          vouchee
          []
          [(otherVoucherForVouchee, UTCTime (fromGregorian 2020 1 1) 0)]
          voucher
  , testCase "non-uploader tried to vouch" $ do
      let ref = Left NotAnUploader
          voucher = UserId 1
          vouchee = UserId 2
      assertEqual "must match" ref $
        judgeVouch
          (fromList []) -- empty. Should contain voucher for operation to proceed.
          (UTCTime (fromGregorian 2020 1 1) 0)
          vouchee
          []
          []
          voucher
  , testCase "voucher too new" $ do
      let ref = Left You'reTooNew
          voucher = UserId 1
          vouchee = UserId 2
          fstVoucherForVoucher = UserId 3
          sndVoucherForVoucher = UserId 4
          now = UTCTime (fromGregorian 2020 1 1) 0
      assertEqual "must match" ref $
        judgeVouch
          (fromList [voucher])
          now
          vouchee
          [ (fstVoucherForVoucher, now) -- These two timestamps are too new
          , (sndVoucherForVoucher, now)
          ]
          []
          voucher
  , testCase "vouchee already uploader" $ do
      let ref = Left VoucheeAlreadyUploader
          voucher = UserId 1
          vouchee = UserId 2
          now = UTCTime (fromGregorian 2020 1 1) 0
      assertEqual "must match" ref $
        judgeVouch
          (fromList [voucher, vouchee]) -- vouchee is here. So they're already an uploader.
          now
          vouchee
          []
          []
          voucher
  , testCase "already vouched" $ do
      let ref = Left YouAlreadyVouched
          voucher = UserId 1
          vouchee = UserId 2
      assertEqual "must match" ref $
        judgeVouch
          (fromList [voucher])
          (UTCTime (fromGregorian 2020 1 1) 0)
          vouchee
          []
          [(voucher, UTCTime (fromGregorian 2020 1 1) 0)] -- voucher is here. So they already vouched
          voucher
  ]

main :: IO ()
main = defaultMain allTests
