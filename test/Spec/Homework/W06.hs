{-# LANGUAGE BlockArguments #-}

module Spec.Homework.W06 where

import Hedgehog (Gen)
import Hedgehog.Classes
import Hedgehog.Function
import qualified Hedgehog.Gen as Gen
import Hedgehog.Main
import qualified Hedgehog.Range as Range
import Homework.W05 (Power (..), Tree (..))
import Homework.W06
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (hspec)

main :: IO ()
main = do
  hspec spec
  defaultMain
    [lawsCheckMany lawsTests]

spec :: Spec
spec =
  describe "stupid" do
    it "should return Just False for non-even length of value" do
      stupid "thingy" [("thingy", "wingy"), ("stuff", "things")]
        `shouldBe` Just False
    it "should return Just True for even length of value" do
      stupid "stuff" [("thingy", "wingy"), ("stuff", "things")]
        `shouldBe` Just True
    it "should return Nothing for missing key" do
      stupid "missing" [("thingy", "wingy"), ("stuff", "things")]
        `shouldBe` Nothing

genOption :: Gen a -> Gen (Option a)
genOption g =
  Gen.choice
    [ Gen.constant None,
      Some <$> g
    ]

genList :: Gen a -> Gen (List a)
genList g =
  foldr Cons Nil
    <$> Gen.list (Range.linear 0 20) g

genTree :: Gen a -> Gen (Tree a)
genTree g =
  foldr treeInsert Leaf
    <$> Gen.list (Range.linear 0 20) g
  where
    treeInsert :: a -> Tree a -> Tree a
    treeInsert x Leaf = Branch x Leaf Leaf
    treeInsert x (Branch y l r)
      | length l > length r = Branch y l (treeInsert x r)
      | otherwise = Branch y (treeInsert x l) r

genPairInt :: Gen a -> Gen (Pair Int a)
genPairInt g = do
  n <- Gen.int Range.linearBounded
  Pair n <$> g

genPower :: Gen a -> Gen (Power a)
genPower g = do
  n <- Gen.int Range.linearBounded
  Power n <$> g

lawsTests :: [(String, [Laws])]
lawsTests =
  [ ( "Option",
      [functorLaws genOption, applicativeLaws genOption, monadLaws genOption]
    ),
    ( "List",
      [functorLaws genList, applicativeLaws genList, monadLaws genList]
    ),
    ( "Tree",
      [functorLaws genTree]
    ),
    ( "Pair",
      [functorLaws genPairInt]
    ),
    ( "Power",
      [functorLaws genPower]
    )
  ]
