{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- | TODO: Doc
module Data.UUID.Divide where

import           Data.Bits (shiftR)
import           Data.UUID (UUID, fromWords, toWords, nil)
import           Data.Word (Word32)

-- | Inclusive on both ends
data UUIDRange =
    UUIDRange { uuidLower :: !UUID, uuidUpper :: !UUID } deriving (Eq, Show)

-- | TODO: Doc
uuidRangeContains :: UUIDRange -> UUID -> Bool
uuidRangeContains UUIDRange{..} uuid = uuid >= uuidLower && uuid <= uuidUpper

-- | TODO: Doc
uuidDivide :: Int -- ^ Power of 2. # of groups.
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

data NthRangeError =
    NegativePowerOfTwo
  | NthOutOfBounds
  deriving (Eq, Show)

-- TODO: Docs
-- This function does validation for you. Right now it's always Right
-- TODO: Maybe & partial versions
nthRange :: Int -- ^ Power of 2 that we're dividing by
         -> Word32 -- ^ nth (starts at 0)
         -> Either NthRangeError UUIDRange
nthRange po2 n = Right $ UUIDRange
  { uuidLower = minPrefix $ step * n + n
  , uuidUpper = maxPrefix $ step * (n + 1) + n
  }
  where
    step = mkStep po2

-- TODO: Utility for non-po2 ranges?
-- TODO: error when we exceed our po2 limit
