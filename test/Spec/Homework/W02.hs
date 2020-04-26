module Spec.Homework.W02 where

import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (hspec)

import Homework.W02

main :: IO ()
main = hspec specs

specs :: Spec
specs = do
  describe "stackStack" $ do
    it "stackStack (Flat Black Top) (Flat White Top)" $
      stackStack (Flat Black Top) (Flat White Top)
      `shouldBe` Flat Black (Flat White Top)

    it "stackStack (Stand Black) (Cap White)" $
      stackStack (Stand Black) (Cap White)
      `shouldBe` Flat Black (Cap White)

    it "stackStack (Stand Black) (Cap Black)" $
      stackStack (Stand Black) (Cap Black)
      `shouldBe` Flat Black (Cap Black)

  describe "takeStack" $ do
    it "takeStack 3 (Flat White (Flat Black (Cap Black)))" $
      takeStack 3 (Flat White (Flat Black (Cap Black)))
      `shouldBe` Flat White (Flat Black (Cap Black))

    it "takeStack 2 (Flat White (Flat Black (Cap Black)))" $
      takeStack 2 (Flat White (Flat Black (Cap Black)))
      `shouldBe` Flat Black (Cap Black)

    it "takeStack 1 (Flat White (Flat Black (Cap Black)))" $
      takeStack 1 (Flat White (Flat Black (Cap Black)))
      `shouldBe` Cap Black

    it "takeStack 3 (Flat White (Flat Black (Stand Black)))" $
      takeStack 3 (Flat White (Flat Black (Stand Black)))
      `shouldBe` Flat White (Flat Black (Stand Black))

    it "takeStack 2 (Flat White (Flat Black (Stand Black)))" $
      takeStack 2 (Flat White (Flat Black (Stand Black)))
      `shouldBe` Flat Black (Stand Black)

    it "takeStack 1 (Flat White (Flat Black (Stand Black)))" $
      takeStack 1 (Flat White (Flat Black (Stand Black)))
      `shouldBe` Stand Black

    it "takeStack 2 (Flat White (Flat Black Top))" $
      takeStack 2 (Flat White (Flat Black Top))
      `shouldBe` Flat White (Flat Black Top)

    it "takeStack 1 (Flat White (Flat Black Top))" $
      takeStack 1 (Flat White (Flat Black Top))
      `shouldBe` Flat Black Top

    it "takeStack 1 (Flat Black Top)" $
      takeStack 1 (Flat Black Top)
      `shouldBe` Flat Black Top

    it "takeStack 1 (Stand Black)" $
      takeStack 1 (Stand Black)
      `shouldBe` Stand Black

    it "takeStack 1 (Cap Black)" $
      takeStack 1 (Cap Black)
      `shouldBe` Cap Black

    it "takeStack 1 (takeStack 1 (Flat Black Top))" $
      takeStack 1 (takeStack 1 (Flat Black Top))
      `shouldBe` Flat Black Top

  describe "dropStack" $ do
    it "dropStack 2 ((Flat White (Flat Black (Cap Black))))" $
      dropStack 2 ((Flat White (Flat Black (Cap Black))))
      `shouldBe` Flat White Top

    it "dropStack 1 (Flat White (Flat Black (Cap Black)))" $
      dropStack 1 (Flat White (Flat Black (Cap Black)))
      `shouldBe` Flat White (Flat Black Top)

    it "dropStack 1 (Flat White (Flat Black (Stand Black)))" $
      dropStack 1 (Flat White (Flat Black (Stand Black)))
      `shouldBe` Flat White (Flat Black Top)
