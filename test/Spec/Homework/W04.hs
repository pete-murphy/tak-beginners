{-# LANGUAGE TemplateHaskell #-}

module Spec.Homework.W04 where

import Control.Monad (void)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Homework.W04
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (hspec)

main :: IO ()
main = do
  hspec specs
  void (checkParallel $$(discover))

specs :: Spec
specs = do
  describe "first" $ do
    it "should return None for empty list" $
      first ([] :: [Int]) `shouldBe` None
    it "should return Some 1 for [1]" $
      first [1] `shouldBe` Some 1
    it "should return Some 1 for [1, 2]" $
      first [1, 2] `shouldBe` Some 1
    it "should return Some 1 for [1..99]" $
      first [1 .. 99] `shouldBe` Some 1
  describe "final" $ do
    it "should return None for empty list" $
      final ([] :: [Int]) `shouldBe` None
    it "should return Some 1 for [1]" $
      final [1] `shouldBe` Some 1
    it "should return Some 20 for [1..20]" $
      final [1 .. 20] `shouldBe` Some 20
  describe "average" $ do
    it "should return None for empty list" $
      average ([] :: [Double]) `shouldBe` None
    it "should return Some 1 for [1]" $
      average [1] `shouldBe` Some 1
    it "should return Some 20 for [1..20]" $
      average [0 .. 20] `shouldBe` Some 10
    it "should return Some 2 for replicate 20 2" $
      average (replicate 20 2) `shouldBe` Some 2
    it "should return Some 0 for replicate 20 0" $
      average (replicate 20 0) `shouldBe` Some 0

prop_first :: Property
prop_first =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 1 100) Gen.alpha
    first xs === Some (head xs)

prop_final :: Property
prop_final =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 1 100) Gen.alpha
    final xs === Some (last xs)

prop_average :: Property
prop_average =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 1 100) (Gen.double (Range.linearFrac (-200) 200))
    case average xs of
      Some avg ->
        diff
          (abs $ avg - (sum xs / fromIntegral (length xs)))
          (<)
          0.000001 -- 🤷‍♀️

prop_semigroupStrangePairAssociativity :: Property
prop_semigroupStrangePairAssociativity =
  property $ do
    x <- forAll Gen.alphaNum
    x' <- forAll Gen.alphaNum
    y <- forAll Gen.alphaNum
    y' <- forAll Gen.alphaNum
    z <- forAll Gen.alphaNum 
    z' <- forAll Gen.alphaNum 
    let a = StrangePair (Pair x x') 
        b = StrangePair (Pair y y')
        c = StrangePair (Pair z z')
    (a <> b) <> c === a <> (b <> c)

prop_semigroupPairAssociativity :: Property
prop_semigroupPairAssociativity =
  property $ do
    x <- forAll $ Gen.string (Range.linear 0 10) Gen.alphaNum
    x' <- forAll $ Gen.string (Range.linear 0 10) Gen.alphaNum
    y <- forAll $ Gen.string (Range.linear 0 10) Gen.alphaNum
    y' <- forAll $ Gen.string (Range.linear 0 10) Gen.alphaNum
    z <- forAll $ Gen.string (Range.linear 0 10) Gen.alphaNum
    z' <- forAll $ Gen.string (Range.linear 0 10) Gen.alphaNum
    let a = Pair x x' 
        b = Pair y y'
        c = Pair z z'
    (a <> b) <> c === a <> (b <> c)