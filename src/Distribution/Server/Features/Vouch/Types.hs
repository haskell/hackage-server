{-# LANGUAGE TemplateHaskell #-}
module Distribution.Server.Features.Vouch.Types where

import Data.SafeCopy (base, deriveSafeCopy)
import Data.Time (UTCTime, addUTCTime, nominalDay)

import Distribution.Server.Users.Types (UserId)

data VouchError
  = NotAnUploader
  | You'reTooNew
  | VoucheeAlreadyUploader
  | AlreadySufficientlyVouched
  | YouAlreadyVouched
  deriving (Show, Eq)

data VouchSuccess = AddVouchComplete | AddVouchIncomplete Int
  deriving (Show, Eq)

deriveSafeCopy 0 'base ''VouchError
deriveSafeCopy 0 'base ''VouchSuccess

requiredCountOfVouches :: Int
requiredCountOfVouches = 2

isWithinLastMonth :: UTCTime -> (UserId, UTCTime) -> Bool
isWithinLastMonth now (_, vouchTime) =
  addUTCTime (30 * nominalDay) vouchTime >= now

-- | Whether a vouch is allowed, given whether the voucher and the vouchee are
-- uploaders, the time, the vouches the voucher received, the vouches the
-- vouchee already has, and the voucher.
judgeVouchWith
  :: Bool
  -> Bool
  -> UTCTime
  -> [(UserId, UTCTime)]
  -> [(UserId, UTCTime)]
  -> UserId
  -> Either VouchError VouchSuccess
judgeVouchWith voucherIsUploader voucheeIsUploader now vouchersForVoucher existingVouchers voucher
  | not voucherIsUploader
  = Left NotAnUploader
  -- You can only vouch for non-uploaders, so if this list has items, the user is uploader because of these vouches.
  -- Make sure none of them are too recent.
  | length vouchersForVoucher >= requiredCountOfVouches && any (isWithinLastMonth now) vouchersForVoucher
  = Left You'reTooNew
  | voucheeIsUploader
  = Left VoucheeAlreadyUploader
  | length existingVouchers >= requiredCountOfVouches
  = Left AlreadySufficientlyVouched
  | voucher `elem` map fst existingVouchers
  = Left YouAlreadyVouched
  | length existingVouchers == requiredCountOfVouches - 1
  = Right AddVouchComplete
  | otherwise
  = Right (AddVouchIncomplete (requiredCountOfVouches - length existingVouchers - 1))
