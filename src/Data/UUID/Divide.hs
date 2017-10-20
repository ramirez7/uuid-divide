{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Mutually-exclusive, collectively-exhaustive sets of UUID ranges.
--
-- Useful for evenly dividing operations over uniformly-distributed UUIDs.
module Data.UUID.Divide (
    UUIDRange(..)
  , uuidRangeContains
  , uuidDivide
  , nthRange
  , NthRangeError(..)
  ) where

import           Control.Exception (Exception)
import           Control.Monad     (unless)
import           Data.Bits         (shiftR, (.&.))
import           Data.Bits.Extras  (log2)
import           Data.Typeable     (Typeable)
import           Data.UUID.Types   (UUID, fromWords, nil, toWords)
import           Data.Word         (Word32)

-- | Inclusive on both ends
data UUIDRange =
    UUIDRange { uuidLower :: !UUID, uuidUpper :: !UUID } deriving (Eq, Show)

-- | Test if a UUID is in a UUIDRange
uuidRangeContains :: UUIDRange -> UUID -> Bool
uuidRangeContains UUIDRange{..} uuid = uuid >= uuidLower && uuid <= uuidUpper

-- | Divide all possible UUIDs into @2^n@ equally-sized ranges:
uuidDivide :: Int -- ^ n : 'uuidDivide' will create 2^n groups.
           -> [UUIDRange]
uuidDivide po2 = take (2 ^ po2) ranges
  where

    ranges  :: [UUIDRange]
    ranges = iterate nextRange $ UUIDRange nil (maxPrefix step)

    nextRange :: UUIDRange -> UUIDRange
    nextRange UUIDRange{..} = let prevUpperPrefix = prefix uuidUpper in
      UUIDRange
        { uuidLower = minPrefix $ prevUpperPrefix + 1
        , uuidUpper = maxPrefix $ prevUpperPrefix + 1 + step
        }

    step = mkStep po2

mkStep :: Int -> Word32
mkStep po2 = maxBound `shiftR` po2

minPrefix :: Word32 -> UUID
minPrefix x = fromWords x minBound minBound minBound

maxPrefix :: Word32 -> UUID
maxPrefix x = fromWords x maxBound maxBound maxBound

prefix :: UUID -> Word32
prefix uuid = let (res, _, _, _) = toWords uuid in res

-- | Validation errors for the inputs to 'nthRange'
data NthRangeError =
    NthNotPowerOfTwo
  | NthOutOfBounds
  deriving (Eq, Show, Typeable)

instance Exception NthRangeError

-- | Get the `nth` (starting at 0) range for a total division (must be a power of 2)
--
-- @
-- 'nthRange' (2 ^ po2) n `shouldBe` Right ('uuidDivide' po2 !! (fromIntegral n))
-- @
nthRange :: Int -- ^ Power of 2 that we're dividing by
         -> Word32 -- ^ nth (starts at 0)
         -> Either NthRangeError UUIDRange
nthRange total n = do
  let total32 = fromIntegral total :: Word32
  unless (total32 > 0 && total32 .&. (total32 - 1) == 0) $ Left NthNotPowerOfTwo
  unless (n >= 0 && n < total32) $ Left NthOutOfBounds
  let po2 = log2 (fromIntegral total32)
  let step = mkStep po2
  Right $ UUIDRange
    { uuidLower = minPrefix $ step * n + n
    , uuidUpper = maxPrefix $ step * (n + 1) + n
    }

-- TODO: Utility for non-po2 ranges?
-- TODO: error when we exceed our po2 limit
