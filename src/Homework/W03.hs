{-# LANGUAGE BlockArguments #-}

module Homework.W03 where

import Data.List (elemIndex)

-- Warmups

listSum :: Num a => [a] -> a
-- listSum []     = 0
-- listSum (x:xs) = x + listSum xs
listSum = foldr (+) 0

listProduct :: [Int] -> Int
-- listProduct []     = 1
-- listProduct (x:xs) = x * listProduct xs
listProduct = foldr (*) 1

index :: Eq a => a -> [a] -> Maybe Int
index = go 0
  where
    go _ _ []     = Nothing
    go n y (z:zs) = if y == z 
                      then Just n 
                      else go (n + 1) y zs

lookupAlist :: Eq k => k -> [(k, v)] -> Maybe v
lookupAlist _   []          = Nothing
lookupAlist key ((k, v):xs) = if k == key 
                                then Just v 
                                else lookupAlist key xs

index' :: Eq a => a -> [a] -> Maybe Int
index' x xs = lookupAlist x (zip xs [1..])

average :: (Eq a, Fractional a) => [a] -> Maybe a
average xs = case go xs of
               (_, 0)       -> Nothing
               (sum, count) -> Just (sum / count)
  where go :: Fractional a => [a] -> (a, a)
        go xs = (listSum xs, fromIntegral (length xs))

-- Part 2, Maybe-ify last week's code

data Player = White | Black
  deriving (Eq, Ord, Show, Enum, Bounded)

data Stack
  = Top
  | Cap   Player
  | Stand Player
  | Flat  Player Stack
    deriving (Eq, Ord, Show)

data Piece
  = FlatStone
  | StandingStone
  | CapStone
    deriving (Eq, Ord, Show, Enum, Bounded)

buildStack :: Player -> Piece -> Stack
buildStack player CapStone      = Cap player
buildStack player StandingStone = Stand player
buildStack player FlatStone     = Flat player Top

stackStack :: Stack -> Stack -> Maybe Stack
stackStack Top s              = Just s
stackStack (Flat p s) s'      = case stackStack s s' of
                                  Just st -> Just (Flat p st)
                                  _       -> Nothing
stackStack (Stand p) (Cap p') = Just (Flat p (Cap p'))
stackStack _ _                = Nothing

stackHeight :: Stack -> Int
stackHeight Top         = 0
stackHeight (Cap _)     = 1
stackHeight (Stand _)   = 1
stackHeight (Flat _ st) = 1 + stackHeight st

takeStack :: Int -> Stack -> Maybe Stack
takeStack m stack = go (stackHeight stack - m) stack
  where go 0 st = Just st
        go n (Flat _ st) = go (n - 1) st
        go _ _ = Nothing

dropStack :: Int -> Stack -> Maybe Stack
dropStack m stack = go (stackHeight stack - m) stack
  where go 0 _           = Just Top
        go 1 (Cap p)     = Just (Cap p)
        go 1 (Stand p)   = Just (Stand p)
        go n (Flat p st) = case go (n - 1) st of
                             Just st' -> Just (Flat p st')
                             Nothing  -> Nothing
        go _ _           = Nothing

-- Part 3, A Simple Tak Board

type Row    = Int
type Column = Char

type Pos = (Row, Column)

getPos :: Pos -> Board -> Maybe Stack
getPos p b = b p

updatePos :: Pos -> Stack -> Board -> Maybe Board
updatePos p s b = 
  case b p of
    -- In this case, we know p is a valid position
    Just s' -> 
      -- so return the original board wrapped in Just
      Just \p' ->
        if p /= p'
          then b p'
          -- but update the return value at p
          else Just s
    Nothing -> Nothing

emptyBoard :: Int -> Maybe Board
emptyBoard n = 
  if validSize n 
    then Just \(row, col) -> 
      if validRow row && validCol col 
        then Just Top 
        else Nothing
    else Nothing
      where
        validSize = inRange 3 8
        validRow = inRange 0 n
        validCol c = all (inRange 0 n) (elemIndex c ['a'..'z'])
        inRange :: Ord a => a -> a -> a -> Bool
        inRange l h x = x >= l && x <= h

data Placement = Placement Piece Pos
  deriving (Eq, Ord, Show)

placePiece :: Player -> Placement -> Board -> Maybe Board
placePiece player (Placement piece pos) board = 
  case board pos of
    Just Top -> 
      Just \pos' -> 
        if pos == pos' 
          then Just (buildStack player piece)
          else board pos
    -- In the non-`Top` case, its occupied
    Just s -> Nothing
    Nothing -> Nothing
    
type Board = Pos -> Maybe Stack