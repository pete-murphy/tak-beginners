module Homework.W02 where

import qualified Data.Function as Function

-- Using the types below, implement the functions below according to their descriptions.  Be
-- forewarned that the functions don't actually have the correct types: we'll fix that in the
-- future.  For now, just make a note of where you had a problem and in that (specific) place,
-- write: @error "some helpful error message"@.

data Player = White | Black
  deriving (Eq, Ord, Show)

data Stack
  = Top
  | Cap   Player
  | Stand Player
  | Flat  Player Stack
    deriving (Eq, Ord, Show)

data Piece
  = CapStone
  | StandingStone
  | FlatStone
    deriving (Eq, Ord, Show)

-- | Create a 'Stack' containing a single 'Piece' owned by the given 'Player'.
buildStack :: Player -> Piece -> Stack
buildStack p CapStone      = Cap p
buildStack p StandingStone = Stand p
buildStack p FlatStone     = Flat p Top

-- | Given two stacks, place the second one atop the first:
--
-- >>> stackStack (Flat Black Top) (Flat White Top)
-- Flat Black (Flat White Top)
stackStack :: Stack -> Stack -> Stack
stackStack Top s              = s
stackStack (Flat p s) s'      = Flat p (stackStack s s')
stackStack (Stand p) (Cap p') = Flat p (Cap p')

-- | Create the 'Stack' with the given number of stones taken from the top of the given stack:
--
-- >>> takeStack 2 (Flat White (Flat Black (Cap Black)))
-- Flat Black (Cap Black)
takeStack :: Int -> Stack -> Stack
takeStack n stack =
  Function.fix
    (\pop m s@ ~(Flat _ s') ->
       if m == 0
         then s
         else pop (m - 1) s')
    (stackHeight stack - n)
    stack

stackHeight :: Stack -> Int
stackHeight Top        = 0
stackHeight (Cap _)    = 1
stackHeight (Stand _)  = 1
stackHeight (Flat _ s) = 1 + stackHeight s

-- | Remove the given number of stones from the given 'Stack':
--
-- >>> dropStack 2 (Flat White (Flat Black (Cap Black)))
-- Flat White Top
dropStack :: Int -> Stack -> Stack
dropStack n stack =
  Function.fix
    (\pop m s@ ~(Flat p s') ->
       if m == 0
         then Top
         else Flat p (pop (m - 1) s'))
    (stackHeight stack - n)
    stack
