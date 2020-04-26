{-# LANGUAGE TemplateHaskell #-}

module Spec.Homework.W03 where

import           Data.Maybe        (isJust)
import           Hedgehog
import qualified Hedgehog.Gen      as Gen
import qualified Hedgehog.Range    as Range
import           Test.Hspec        (Spec, describe, it, shouldBe, shouldNotBe)
import           Test.Hspec.Runner (hspec)
import           Control.Monad     (join)

import Homework.W03

main :: IO ()
main = do
  hspec specs
  propertyTests

specs :: Spec
specs = do
  describe "emptyBoard" $ do
    it "should fail for dimension that's too large" $
      isJust (emptyBoard 9) `shouldBe` False

    it "should fail for dimension that's too small" $
      isJust (emptyBoard 2) `shouldBe` False

    it "should succeed for dimension that's just right" $
      isJust (emptyBoard 3) `shouldBe` True

  describe "getPos" $ do
    it "should work for top-left position on a valid empty board" $
      (getPos (1, 'a') =<< emptyBoard 3)
      `shouldBe` Just Top

    it "should work for bottom-right position on a valid empty board" $
      (getPos (3, 'c') =<< emptyBoard 3)
      `shouldBe` Just Top

    it "should not work for out-of-bound position on a valid empty board" $
      (getPos (4, 'c') =<< emptyBoard 3)
      `shouldBe` Nothing
    
    -- Maybe could be a property test
    it "placing piece A at position P on empty board and then getting at P should return a stack with A" $
      (getPos (4, 'a') =<< placePiece White (Placement FlatStone (4, 'a')) =<< emptyBoard 5)
      `shouldBe` Just (Flat White Top)

    it "placing piece A at invalid position P on empty board and then getting at P should return Nothing" $
      (getPos (9, 'a') =<< placePiece White (Placement FlatStone (9, 'a')) =<< emptyBoard 5)
      `shouldBe` Nothing

    -- Should probably be a property test
    it "updating position P on empty board with stack S, and then getting at P should return a stack with A" $
      (getPos (4, 'a') =<< updatePos (4, 'a') (Flat White Top) =<< emptyBoard 5)
      `shouldBe` Just (Flat White Top)

    it "updating invalid position P on empty board with stack S, and then getting at P should return Nothing" $
      (getPos (9, 'a') =<< updatePos (9, 'a') (Flat White Top) =<< emptyBoard 5)
      `shouldBe` Nothing


prop_updateAndGet :: Property
prop_updateAndGet =
  property $ do
    (pos, st1, st2) <- forAll $ do 
      p <- genPos (Range.linear 0 10)
      maybeBoard' <- genMaybeBoard (Range.linear 0 10)
      pure (p 
           -- Board is a function, so no `Eq` or `Show` instances for `Maybe
           -- Board` but we can apply that `Maybe Board` to some `Pos` and get a
           -- `Maybe Stack`
           , do
             b <- maybeBoard'
             b p 
           , do
             b <- maybeBoard'
             b' <- updatePos p (Flat White Top) b
             b' p
           )
    st2 === join (stackStack <$> st1 <*> st2)

{-
Started writing this property based on a misunderstanding of placePiece

-- | Placing two stacks at same position, and then getting from that position,
-- should be same as stacking the stacks, updating the board with them at that
-- position and then getting from that position :)
prop_stackGetAndStackStackUpdateGet :: Property
prop_stackGetAndStackStackUpdateGet =
  property $ do
    (st1, st2) <- forAll $ do 
      p <- genPos (Range.linear 0 10)
      b <- genMaybeBoard (Range.linear 0 10)
      piece1 <- genPiece
      piece2 <- genPiece
      player1 <- genPlayer
      player2 <- genPlayer
      let b1 = placePiece player1 piece1 p =<< b
          b2 = placePiece player2 piece2 p =<< b
          st = stackStack (buildStack player1 piece1) (buildStack player2 piece2)
      pure ( 
           , placePiece 
           )

    st1 === st2
-}

intToChar :: Int -> Char
intToChar n = case lookup n (zip [0..] ['a'..'z']) of
  Just c -> c
  _      -> error "Whoops partial `intToChar`"

genPos :: MonadGen m => Range Int -> m Pos
genPos r = do
  n <- Gen.int r
  m <- Gen.int r
  pure (n, intToChar m)

genMaybeBoard :: MonadGen m => Range Int -> m (Maybe Board)
genMaybeBoard r = do
  n <- Gen.int r
  pure (emptyBoard n)

genPiece :: MonadGen m => m Piece
genPiece = Gen.enumBounded

genPlayer :: MonadGen m => m Player
genPlayer = Gen.enumBounded

genFlatStack :: MonadGen m => m Stack
genFlatStack = buildStack <$> genPlayer <*> genPiece

genStack :: MonadGen m => m Stack
genStack = do
  stacks <- Gen.list (Range.linear 0 6) genFlatStack
  pure (go Top stacks)
  where
    go s [] = s
    go s (stack:stacks') = 
      case stackStack s stack of
        Just newStack -> go newStack stacks'
        Nothing       -> s



propertyTests :: IO ()
propertyTests = 
  checkParallel $$(discover) >>
  pure ()