{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DivideSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Data.Foldable             (for_)
import           Data.UUID.Divide
import           Data.UUID.Types           (UUID, fromWords)
import           Data.Word                 (Word32)

--import           Data.UUID.NewModule

spec :: Spec
spec = do
{-  describe "newFunction" $ do
    it "should work" $ do
    let x = newFunction () ()
    x `shouldBe` ()-}
  describe "uuidDivide" $ do
    it "should always create UUIDRanges s.t. lower < upper" $
      testAll $ \UUIDRange{..} -> uuidLower < uuidUpper
    it "should always create UUIDRanges s.t. upper_i < lower_(i+1)" $
      testAdjacent $ \UUIDRange {uuidUpper=u1} UUIDRange{uuidLower=l2} -> u1 < l2
    for_ testData $ \urs ->
      -- NOTE: I think we need to target our uuid generate a little bit more here to have a
      -- hope of catching errors with this
      prop "Divided UUIDRanges should be mutually-exclusive & collectively exhaustive" $
        flip fmap uuidGen $ \uuid -> exactlyOne (flip uuidRangeContains $ uuid) urs
  describe "nthRange" $ do
    it "should be consistent with uuidDivide" $ do
      for_ [0..8] $ \(po2 :: Int) ->
        for_ [0..fromIntegral $ ((2 :: Word32) ^ po2) - 1] $ \(n :: Word32) ->
          nthRange (2 ^ po2) n `shouldBe` Right (uuidDivide po2 !! (fromIntegral n))
    it "should fail for non-po2 totals" $ do
      nthRange 15 14 `shouldBe` Left NthNotPowerOfTwo
    it "should fail when n is out of bounds" $ do
      nthRange 16 17 `shouldBe` Left NthOutOfBounds

-- TODO: 'uuidRangeContains' tests

testData :: [[UUIDRange]]
testData = fmap uuidDivide [0..16]

testAll :: (UUIDRange -> Bool) -> IO ()
testAll p =
  for_ testData $ \urs ->
    for_ urs $ \ur -> ur `shouldSatisfy` p

testAdjacent :: (UUIDRange -> UUIDRange -> Bool) -> IO ()
testAdjacent p =
  for_ testData $ \urs ->
    for_ (urs `zip` tail urs) $ \pair -> pair `shouldSatisfy` (uncurry p)

uuidGen :: Gen UUID
uuidGen = fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

exactlyOne :: (a -> Bool) -> [a] -> Bool
exactlyOne p xs = length (filter p xs) == 1
