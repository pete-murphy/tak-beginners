{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Homework.W05 where

import Control.Monad (void)
import Data.List (sort)
import Hedgehog
  ( (===),
    Property,
    checkParallel,
    diff,
    discover,
    forAll,
    property,
  )
import qualified Hedgehog.Gen as Gen
import Hedgehog.Main (defaultMain)
import qualified Hedgehog.Range as Range
import Homework.W05
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (hspec)

main :: IO ()
main = do
  hspec spec
  void do checkParallel $$(discover)

spec :: Spec
spec = do
  describe "find" do
    it "should return Nothing for failure" do
      find (> 3) [1, 2, 3] `shouldBe` Nothing
    it "should return Just for success" do
      find (> 3) [1 .. 4] `shouldBe` Just 4
    it "should work on Foldables other than List" do
      find (> 3) (Just 4) `shouldBe` Just 4
  describe "filter" do
    it "should work the same as List.filter" do
      filter' (> 3) [1 .. 20] `shouldBe` filter (> 3) [1 .. 20]
  describe "count" do
    it "should be the same as length after filter" do
      count (> 3) [1 .. 20] `shouldBe` (length . filter (> 3)) [1 .. 20]
  describe "member" do
    it "should work the same as List.elem" do
      member 3 [1 .. 20] `shouldBe` elem 3 [1 .. 20]
  describe "isEmpty" do
    it "should return True for []" do
      isEmpty [] `shouldBe` True
    it "should return True for Nothing" do
      isEmpty Nothing `shouldBe` True
    it "should return False for [1]" do
      isEmpty [1] `shouldBe` False
  describe "Foldable Tree" do
    it "should do a pre-order traversal" do
      {-
      Per Wikipedia (https://en.wikipedia.org/wiki/Tree_traversal#Pre-order_(NLR)),
      the pre-order traversal of:
      
                F
               ╱ ╲
              ╱   ╲
             B     G
            ╱ ╲     ╲
           A   D     I
              ╱ ╲   ╱
             C   E H
      
      would be:
        F, B, A, D, C, E, G, I, H
      -}

      let tree =
            Branch
              'f'
              (Branch 'b' (l 'a') (Branch 'd' (l 'c') (l 'e')))
              (Branch 'g' Leaf (Branch 'i' (l 'h') Leaf))
          l x = Branch x Leaf Leaf
      toList tree `shouldBe` "fbadcegih"
  describe "OrderedList" do
    it "toList . fromList should be same as List.sort" do
      let xs = [1, 9, 20, 0, 5]
      (toList . fromList) xs `shouldBe` sort xs
  describe "intersection" do
    it "should return intersection of two ordered lists" do
      let xs = fromList [1, 2]
          ys = fromList [1, 9, 20, 0, 5]
      toList (intersection xs ys) `shouldBe` [1]
    it "should return intersection of two ordered lists" do
      let xs = fromList [1, 2]
          ys = fromList [1, 2, 9, 20, 0, 5]
      toList (intersection xs ys) `shouldBe` [1, 2]

prop_orderedList :: Property
prop_orderedList =
  property do
    xs <- forAll do Gen.list (Range.linear 0 100) Gen.alpha
    (toList . fromList) xs === sort xs

prop_deleteAndInsertAndDelete :: Property
prop_deleteAndInsertAndDelete =
  property do
    x <- forAll Gen.alphaNum
    xs <- forAll do Gen.list (Range.linear 0 100) Gen.alphaNum
    (toList . delete x . insert x . fromList) xs
      === (toList . delete x . fromList) xs

prop_sizeAfterDelete :: Property
prop_sizeAfterDelete =
  property do
    x <- forAll Gen.alphaNum
    xs <- forAll do Gen.list (Range.linear 0 100) Gen.alphaNum
    (size . delete x . fromList) xs
      === size (fromList xs) - count (x ==) (fromList xs)

prop_memberAfterDelete :: Property
prop_memberAfterDelete =
  property do
    x <- forAll Gen.alphaNum
    xs <- forAll do Gen.list (Range.linear 0 100) Gen.alphaNum
    (member x . delete x . fromList) xs === False

prop_memberAfterInsert :: Property
prop_memberAfterInsert =
  property do
    x <- forAll Gen.alphaNum
    xs <- forAll do Gen.list (Range.linear 0 100) Gen.alphaNum
    (member x . insert x . fromList) xs === True

prop_emptyAfterInsert :: Property
prop_emptyAfterInsert =
  property do
    x <- forAll Gen.alphaNum
    xs <- forAll do Gen.list (Range.linear 0 100) Gen.alphaNum
    (isEmpty . insert x . fromList) xs === False

prop_lengthAfterUnion :: Property
prop_lengthAfterUnion =
  property do
    xs <- forAll do fromList <$> Gen.list (Range.linear 0 100) Gen.alphaNum
    ys <- forAll do fromList <$> Gen.list (Range.linear 0 100) Gen.alphaNum
    size xs + size ys === size (xs `union` ys)

prop_intersectionCommutative :: Property
prop_intersectionCommutative =
  property do
    xs <- forAll do fromList <$> Gen.list (Range.linear 0 100) Gen.alphaNum
    ys <- forAll do fromList <$> Gen.list (Range.linear 0 100) Gen.alphaNum
    toList (xs `intersection` ys) === toList (ys `intersection` xs)

prop_intersectionWithEmptyList :: Property
prop_intersectionWithEmptyList =
  property do
    xs <- forAll do fromList <$> Gen.list (Range.linear 0 100) Gen.alphaNum
    toList (xs `intersection` emptyOL) === []

prop_sizeAfterDelete' :: Property
prop_sizeAfterDelete' =
  property do
    x <- forAll Gen.alphaNum
    xs <- forAll do fromList <$> Gen.list (Range.linear 0 100) Gen.alphaNum
    let diffSize = size xs - size (delete' x xs)
    diff diffSize (<=) 1

prop_intersection'Commutative :: Property
prop_intersection'Commutative =
  property do
    xs <- forAll do fromList <$> Gen.list (Range.linear 0 100) Gen.alphaNum
    ys <- forAll do fromList <$> Gen.list (Range.linear 0 100) Gen.alphaNum
    toList (xs `intersection'` ys) === toList (ys `intersection'` xs)

prop_intersection'WithEmptyList :: Property
prop_intersection'WithEmptyList =
  property do
    xs <- forAll do fromList <$> Gen.list (Range.linear 0 100) Gen.alphaNum
    toList (xs `intersection'` emptyOL) === []

prop_intersectionAndIntersection' :: Property
prop_intersectionAndIntersection' =
  property do
    xs <- forAll do fromList <$> Gen.list (Range.linear 0 100) Gen.alphaNum
    ys <- forAll do fromList <$> Gen.list (Range.linear 0 100) Gen.alphaNum
    toList (xs `intersection` ys) === toList (xs `intersection'` ys)
