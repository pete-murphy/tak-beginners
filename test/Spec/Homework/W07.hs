{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Homework.W07 where

import Hedgehog
  ( (===),
    Gen,
    Property,
    checkParallel,
    diff,
    discover,
    forAll,
    property,
  )
import Hedgehog.Classes
import Hedgehog.Function
import qualified Hedgehog.Gen as Gen
import Hedgehog.Main
import qualified Hedgehog.Range as Range
import Homework.W07
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (hspec)

main :: IO ()
main = do
  hspec spec
  defaultMain
    [checkParallel $$(discover)]

spec :: Spec
spec = do
  describe "foreach" do
    it "should return Just [..] for successful lookups" do
      foreach (`lookup` [(1, "foo"), (2, "bar")]) [1, 2]
        `shouldBe` Just ["foo", "bar"]
    it "should return Nothing for unsuccessful lookups" do
      foreach (`lookup` [(1, "foo"), (2, "bar")]) [1, 2, 3]
        `shouldBe` Nothing
  describe "Count" do
    it "should count the number of times pure has been used" do
      runCount ((+) <$> pure 5 <*> pure 8)
        `shouldBe` (2, 13)
    it "should count the number of times pure has been used" do
      runCount (foreach pure "foobar")
        `shouldBe` (7, "foobar")
  describe "Conf" do
    it "should make orate (with Loud) work as expected" do
      runConf (orate ["Greetings", "Fellow", "Citizens"]) Loud
        `shouldBe` "GREETINGS\nFELLOW\nCITIZENS\n"
    it "should make orate (with Quiet) work as expected" do
      runConf (orate ["Greetings", "Fellow", "Citizens"]) Quiet
        `shouldBe` "greetings fellow citizens"
  describe "wc" do
    it "should return character, word, & line count for a given string" do
      runConf wc "Whan that Aprille\nwith his shoores soote\nthe drought of march\nhath perced to the roote"
        `shouldBe` (86, 16, 4)

prop_weirdFilter :: Property
prop_weirdFilter =
  property do
    xs <- forAll do Gen.list (Range.linear 0 100) (Gen.int Range.linearBounded)
    p <- forAllFn do fn Gen.bool
    weirdFilter p xs === filter p xs

prop_weirdFactorial :: Property
prop_weirdFactorial =
  property do
    n <- forAll do Gen.int (Range.linear 0 1000)
    weirdFactorial n === product [1 .. n]

prop_weirderFactorial :: Property
prop_weirderFactorial =
  property do
    n <- forAll do Gen.int (Range.linear 0 1000)
    weirderFactorial n === product [1 .. n]
