module Homework.W03 where


-- This week's homework has three parts.  The first part will be a few warmups to get used to
-- working with lists and tuples.  The second will be to adapt the tak 'Stack' manipulation
-- functions from last week to use 'Maybe' to indicate failure (rather than exploding with @error@).
-- Third, we'll start looking at how we might implement the board for our tak game.

-- Warmups

-- Here's the classic implementation of summing a list:
listSum :: Num a => [a] -> a
listSum []     = 0
listSum (x:xs) = x + listSum xs

-- Just to get started, implement the product of a list:
listProduct :: [Int] -> Int
listProduct []     = 0
listProduct (x:xs) = x * listSum xs

-- Let's find the index of an element in a list (i.e., the position of the first occurence)---the
-- element might not be there:
--
-- >>> index 4 [3,1,4,5]
-- Just 2
--
-- >>> index 9 [3,1,4,5]
-- Nothing
--
-- NB: Remember that @Eq a@ means that you can use @(==)@ to compare values of type @a@.
index :: Eq a => a -> [a] -> Maybe Int
index = go 0
  where
    go _ _ []     = Nothing
    go n y (z:zs) = if y == z then Just n else go (n + 1) y zs


-- An "association list" or "alist" is a list of @(key, value)@ pairs, providing a basic means of
-- associating a value with a given key.  As with 'index' above, a given key might not be present in
-- the alist, and so looking it up might fail.
--
-- >>> lookupAlist "foo" [("bar", 3), ("baz", 1), ("foo", 4)]
-- Just 4
--
-- >>> lookupAlist "quux" [("bar", 3), ("baz", 1), ("foo", 4)]
-- Nothing
--
-- NB: Notice how we're relying on as /little/ information as required to implement the function.
-- We have to be able to compare keys, so we need @Eq k@, but we don't actually care what the type
-- of @k@ or @v@ is.  We know that we may fail to find a @v@, so we are compelled to give back a
-- @Maybe v@, just in case there's no matching key in the alist.  If you meditate on the type for a
-- bit, you can pretty much figure out exactly what 'lookupAlist' is going to do even without the
-- description above.
lookupAlist :: Eq k => k -> [(k, v)] -> Maybe v
lookupAlist _   []          = Nothing
lookupAlist key ((k, v):xs) = if k == key then Just v else lookupAlist key xs

-- You may be familiar with a function named 'zip'.  Here's its type:
--
-- zip :: [a] -> [b] -> [(a, b)]
--
-- That type should be enough to tell you what 'zip' does.
--
-- Try writing 'index' again using 'zip' and 'lookupAlist'.
index' :: Eq a => a -> [a] -> Maybe Int
index' x xs = lookupAlist x (zip xs [1..])

-- Now we're going to implement taking the average (arithmetic mean) in a slightly stylized way.
-- We'll take a list of 'Double's, Simultaneously compute its sum /and/ its length, and then do the
-- final division only if it's safe:
average xs = case go xs of
               (_, 0)       -> Nothing
               (sum, count) -> Just (sum / count)
  where go :: [Double] -> (Double, Double)
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

-- | Create a 'Stack' containing a single 'Piece' owned by the given 'Player'.
buildStack :: Player -> Piece -> Stack
buildStack player CapStone      = Cap player
buildStack player StandingStone = Stand player
buildStack player FlatStone     = Flat player Top

-- | Given two stacks, place the second one atop the first:
--
-- >>> stackStack (Flat Black Top) (Flat White Top)
-- Just (Flat Black (Flat White Top))
--
-- >>> stackStack (Cap Black) (Flat White Top)
-- Nothing
stackStack :: Stack -> Stack -> Maybe Stack
stackStack Top s              = Just s
stackStack (Flat p s) s'      = case stackStack s s' of
                                  Just st -> Just (Flat p st)
                                  _       -> Nothing
stackStack (Stand p) (Cap p') = Just $ Flat p (Cap p')
stackStack _ _                = Nothing
-- stackStack Top st              = st
-- stackStack (Flat p st1) st2    = Flat p (stackStack st1 st2)
-- stackStack (Stand p1) (Cap p2) = Flat p1 (Cap p2)
-- stackStack (Cap _) _           = error "can't stack on a capstone"
-- stackStack (Stand _) _         = error "can't stack on a standing stone"

stackHeight :: Stack -> Int
stackHeight Top         = 0
stackHeight (Cap _)     = 1
stackHeight (Stand _)   = 1
stackHeight (Flat _ st) = 1 + stackHeight st

-- | Create the 'Stack' with the given number of stones taken from the top of the given stack:
--
-- >>> takeStack 2 (Flat White (Flat Black (Cap Black)))
-- Just (Flat Black (Cap Black))
--
-- >>> takeStack 2 (Cap Black)
-- Nothing
takeStack :: Int -> Stack -> Maybe Stack
takeStack m stack = go (stackHeight stack - m) stack
  where go 0 st = Just st
        go n (Flat _ st) = go (n - 1) st
        go _ _ = Nothing



-- | Remove the given number of stones from the given 'Stack':
--
-- >>> dropStack 2 (Flat White (Flat Black (Cap Black)))
-- Just (Flat White Top)
--
-- >>> dropStack 2 (Cap Black)
-- Nothing
dropStack :: Int -> Stack -> Maybe Stack
dropStack m stack = go (stackHeight stack - m) stack
  where go 0 _           = Just Top
        go 1 (Cap p)     = Just $ Cap p
        go 1 (Stand p)   = Just $ Stand p
        go n (Flat p st) = case go (n - 1) st of
                             Just st' -> Just $ Flat p st'
                             Nothing  -> Nothing
        go _ _           = Nothing

-- Part 3, A Simple Tak Board

-- We've seen the syntax for creating new data types (@data@).  There is also syntax for creating
-- type aliases:

type Row    = Int
type Column = Char

-- These mean that I can write @Row@ in a type signature, and it would be /the same/ as if I had
-- written @Int@.  This is a useful feature because it allows us to use functions that work on data
-- we already have but wish to interpret in more specific way.  You've already seen this:
--
-- type String = [Char]

-- We can use these types immediately to describe a position on the Tak board:

type Pos = (Row, Column)

-- RANT:
-- 'Pos' is mildly annoying as, so far as I can tell, people don't like thinking about positions
-- on a grid-like board as @(Column, Row)@, even though that's how chess notation ("knight takes
-- b2") and x-y coordinates work.  So if you prefer @type Pos = (Column, Row)@, we can switch, but I
-- feel like it's counter-intuitive for one reason or another either way, so I flipped a coin.
-- END RANT
--
-- Anyway, let's think of a couple operations we'd like to do on our Tak board---I'm going to refer
-- to a type 'Board' that we'll describe below, but we can talk about the operations first.  Read
-- through the following operations we're going to implement first, and then choose which
-- representation of a 'Board' you'd prefer.

-- | Retrieve the 'Stack' at the given position on the 'Board', failing if the position is out of
-- bounds.
getPos :: Pos -> Board -> Maybe Stack
getPos p b = b p

-- | Unconditionally update the 'Board' to hold the given 'Stack' at the given 'Pos', failing if the
-- position is out of bounds.
--
-- NB: "updating" here means producing a new Board that's just like the old one, except with the
-- stack in the appropriate position.
updatePos :: Pos -> Stack -> Board -> Maybe Board
updatePos p s b = 
  case b p of
    -- In this case, we know p is a valid position
    Just s' -> 
      -- so return the original board wrapped in Just
      Just $ \p' ->
        if p /= p'
          then b p'
          -- but update the return value at p
          else Just s
    Nothing -> Nothing

-- | Create an empty 'Board' of the given size, i.e., one for which 'getPos' gives back 'Just Top'
-- for all in-bounds positions.  Fail when the given dimension doesn't correspond to a legal tak
-- board.  The legal sizes are 3 through 8.
emptyBoard :: Int -> Maybe Board
emptyBoard n = 
  if validSize n 
    then Just $ \(row, col) -> 
      if validRow row && validCol col 
        then Just Top 
        else Nothing
    else Nothing
      where
        validSize x = x >= 3 && x <= 8
        validRow r = r >= 0 && r <= n
        validCol c = all (\m -> m >= 0 && m <= n) (lookupAlist c (zip ['a'..'z'] [0..]))


-- | A 'Placement' represents placing a stone on an unoccupied position on the board.  (As opposed
-- to a @Movement@, which we'll work on next week).
data Placement = Placement Piece Pos
  deriving (Eq, Ord, Show)

-- | Place a piece on the 'Board', failing if the position is already occupied /or/ is out of
-- bounds.
placePiece :: Player -> Placement -> Board -> Maybe Board
placePiece player (Placement piece pos) board = 
  case board pos of
    Just Top -> 
      Just $ \pos' -> 
        if pos == pos' 
          then Just $ buildStack player piece
          else board pos
    -- In the non-Top case, its occupied
    Just s -> Nothing
    Nothing -> Nothing
    
-- >>> lookupAlist 'c' (zip ['a'..'z'] [0..])

-- $> getPos (2, 'a') =<< emptyBoard 5

-- $> getPos (2, 'd') =<< emptyBoard 3

-- $> getPos (2, 'd') =<< updatePos (2, 'a') (Flat White Top) =<< emptyBoard 3

-- $> getPos (2, 'o') =<< updatePos (2, 'a') (Flat White Top) =<< emptyBoard 5

-- $> getPos (2, 'o') =<< updatePos (2, 'a') (Flat White Top) =<< emptyBoard 5

-- Now, about that 'Board' type.  I can think of several representations that might suffice.  We
-- could use our alist representation to map coordinates to 'Stack's:

-- type Board = [(Pos, Stack)]

-- Another choice would be to simply represent a board as a function that takes a 'Pos' and gives
-- back a 'Maybe Stack':

type Board = Pos -> Maybe Stack

-- I'm going to give a clearly untenable representation of a 'Board' below to make sure this file
-- compiles, but you'll need to pick one of the above choices to implement the functions above.

-- type Board = ()
