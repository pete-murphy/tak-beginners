{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}

module Homework.W05 where

import Data.Semigroup -- Feel free to use any of the newtype wrappers from Data.Semigroup if you
  -- like.  They're the ones we implemented for homework last week.

-- | Remember that 'Foldable' is characterized by its method 'foldMap':
--
-- @
--   foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- @
--
-- which we interpret as saying "If @m@ is a monoid, and you can turn any @a@ into an @m@, then I
-- can tell you what happens when you turn every @a@ in this @t@ into an @m@ and smash them all
-- together gives you".  That's a bit of a mouthful---honestly, the type signature is probably
-- easier to understand than my prose there.
--
-- Let's start by implementing a few functions that take advantage of 'Foldable'.  Several of these
-- functions are available in the 'Data.Foldable' module, but you should implement them on your own.
-- For some of these functions, I'm only going to provide the types.  Try to figure out what the
-- function ought to do based on the name and the type.  If you're not sure, feel free to ask me.

-- | An example from class to refresh your memory:
length' :: Foldable t => t a -> Int
length' = getSum . foldMap (const $ Sum 1)

-- | Now your turn:
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

-- | We'll find it convenient to have 'mapMaybe' (like last week's @mapOption@, but for the real
-- 'Maybe').  Next week, we'll discuss 'Functor', which generalizes 'map' and 'mapMaybe'.  For now,
-- feel free to use 'mapMaybe', or 'fmap', which does the same thing.
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe = fmap

-- >>> find (> 3) [1..20]
-- Just 4
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find pred = mapMaybe getFirst . foldMap (guard <$> pred <*> Just . First)

-- find pred = mapMaybe getFirst . foldMap (\x -> if pred x then Just (First x) else Nothing)

filter' :: Foldable t => (a -> Bool) -> t a -> [a]
filter' pred = foldMap (guard <$> pred <*> (: []))

guard :: Monoid m => Bool -> m -> m
guard b m = if b then m else mempty

-- >>> count (> 3) [1..20]
-- 17
count :: Foldable t => (a -> Bool) -> t a -> Int
count pred = getSum . foldMap (guard <$> pred <*> const (Sum 1))

-- | This appears in the library named 'elem'.
--
-- >>> member 8 [1..20]
-- True
member :: (Foldable t, Eq a) => a -> t a -> Bool
member a = getAny . foldMap (Any . (==) a)

-- | This appears in the library named 'null'.
--
-- >>> isEmpty Nothing
-- True
isEmpty :: Foldable t => t a -> Bool
isEmpty = getAll . foldMap (All . const False)

-- | This appears in the library named 'concat'
--
-- >>> flatten Nothing
-- []
--
-- >>> flatten [[1,2,3], [4,5,6]]
-- [1,2,3,4,5,6]
flatten :: Foldable t => t [a] -> [a]
flatten = fold

-- | Don't solve every problem using this function :)
toList :: Foldable t => t a -> [a]
toList = foldMap (: [])

-- | Let's instantiate 'Foldable' for a few conventional types.  If you know what a "pre-order
-- traversal" is, do that one.  If not, anything is fine.
data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving (Show)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Branch x t t') =
    f x <> foldMap f t <> foldMap f t'

-- | In class, we briefly discussed that there's a 'Foldable' instance for pairs.  This instance is
-- canonical, but somewhat controversial.  However, because of the resistance to that instance,
-- there isn't an analogous one for any n-tuples with @n>2@.  Let's make one for triples:
instance Foldable ((,,) a b) where
  foldMap f (_, _, c) = f c

-- | Let's implement 'Foldable' for some funny types.  'Power' is a type that represents a
-- collection of @n@ copies of a value of type @a@.  You can think of @Power n x@ as having the same
-- semantics as @replicate n x@, but more efficient.  This type is a good example of one where you
-- can do much better than the default implementations by specializing some of the methods.  Feel
-- free to give that a try here if you like, but only 'foldMap' is required.  (Note that the methods
-- 'maximum' and 'minimum' are, sadly, partial due to compatibility issues with past mistakes.)
--
-- >>> toList (Power 5 'c')
-- "ccccc"
data Power a = Power Int a
  deriving (Eq, Show)

instance Foldable Power where
  foldMap :: Monoid m => (a -> m) -> Power a -> m
  foldMap _ (Power 0 _) = mempty
  foldMap f (Power n a) = f a <> foldMap f (Power (n - 1) a)

  -- perhaps some more methods if you like
  length (Power n _) = n

-- | Run-length encoding (https://en.wikipedia.org/wiki/Run-length_encoding) is a technique for
-- compressing sequences that is very effective when there are long subsequences of repeated
-- elements.  For instance, "foooooobaaaaaaar" could be RLE-encoded (intuitively) as "1f6o1b6a1r".
-- (The space savings would be more obvious for a longer string, presumably).  We can use 'Power' as
-- a building block for a run-length encoded representation of a list.
--
-- >>> toList $ RLE [Power 20 'a', Power 20 'b']
-- "aaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbb"
newtype RLE a = RLE {getRLE :: [Power a]}
  deriving (Show)

instance Foldable RLE where
  foldMap :: Monoid m => (a -> m) -> RLE a -> m
  foldMap f (RLE list) = foldMap (foldMap f) list

-- | I haven't decided whether we'll get back to RLE; regardless it's a neat example.  Anyway, now
-- let's build up a little library for ordered-lists.  Because this sort of datastructure relies on
-- an invariant (i.e., that it's ordered) that we will not be enforcing statically (i.e., with the
-- type system).  Normally, we'd use Haskell's module system to hide its constructor to keep users
-- from building illegal ordered lists.  Because I much prefer to keep everything to a single file
-- so it's easier to distribute.  So, unless I say otherwise, only use the 'OL' constructor to
-- create an 'OrderedList' when implementing type class instances.
newtype OrderedList a = OL {getOrderedList :: [a]}
  deriving (Show)

-- Instantiate 'Foldable' for 'OrderedList'.  Observe that even though the semantics of our data
-- structure is that its elements are already ordered, nothing about 'Foldable' actually requires
-- that ordering, so we may write:

instance Foldable OrderedList where
  foldMap :: Monoid m => (a -> m) -> OrderedList a -> m
  foldMap f (OL list) = foldMap f list

-- Instantiate 'Semigroup' for 'OrderedList'.  Smashing two 'OrderedLists' together should result in
-- an 'OrderedList' that contains all elements in both lists.  Observe that here, there's no way to
-- provide such an instance unless you know that the elements have an ordering.

-- $> OL [1,3,5] <> OL [2,3,4]
-- OL [1,2,3,3,4,5]

-- $> take 10 . getOrderedList $ OL [1,3..] <> OL [2,4..]
-- OL [1,2,3,4,5,6,7,8,9,10]

instance Ord a => Semigroup (OrderedList a) where
  OL [] <> list = list
  list <> OL [] = list
  OL (x : xs) <> OL (y : ys)
    | x <= y = OL (x : getOrderedList do OL xs <> OL (y : ys))
    | otherwise = OL (y : getOrderedList do OL (x : xs) <> OL ys)

-- | Let's do 'Monoid' as well.
instance Ord a => Monoid (OrderedList a) where
  mempty = OL []

-- | Create a an ordered list containing only the given element.  You may (indeed must) use the 'OL'
-- constructor to implement this function.
--
-- >>> toList $ singleton "foo"
-- ["foo"]
singleton :: Ord a => a -> OrderedList a
singleton = OL . (: [])

-- | We can think of an 'OrderedList' as a multiset (that is, a set datastructure where elements can
-- appear more than once).  Below are a number of functions you might expect to have in a (multi)set
-- library.  Implement them all relying only on 'singleton' and on 'OrderedList' being 'Foldable'
-- and a 'Monoid'.  That means you can use any method from 'Foldable' or 'Monoid' (or 'Semigroup'),
-- and any functions we've developed earlier in this homework.  You may also use any 'Monoid' or
-- 'Semigroup' we've imported or that you might like to implement.  The idea here is to notice how
-- rich an interface we can get "for free" from 'Foldable' and 'Monoid'.

-- | The empty 'OrderedList'
emptyOL :: Ord a => OrderedList a
emptyOL = mempty

olIsEmpty :: OrderedList a -> Bool
olIsEmpty = isEmpty

-- | Insert an element into the list.  Remember you're not allowed to use 'OL'!
insert :: Ord a => a -> OrderedList a -> OrderedList a
insert = (<>) . singleton

-- | Construct an 'OrderedList' from the given list.

-- $> toList $ fromList "foobar"
-- "abfoor"

fromList :: Ord a => [a] -> OrderedList a
fromList = foldMap singleton

-- | Take the union of two 'OrderedLists'.
union :: Ord a => OrderedList a -> OrderedList a -> OrderedList a
union = (<>)

contains :: Eq a => a -> OrderedList a -> Bool
contains = member

subset :: Eq a => OrderedList a -> OrderedList a -> Bool
subset xs ys = getAll do foldMap (All . flip member ys) xs

largestElement :: Ord a => OrderedList a -> Maybe a
largestElement = fmap getMax . foldMap (Just . Max)

size :: OrderedList a -> Int
size = length

delete :: Ord a => a -> OrderedList a -> OrderedList a
delete x = foldMap (guard <$> (/=) x <*> singleton)

-- | A parting question or two: Of the functions above, which may be generalized to work on any
-- 'Foldable', and which depend essentially on what an 'OrderedList' is?  What other functions
-- mighty you want out of a (multi)set library, and which of those might you be able to get from the
-- typeclasses?

-- > I _think_ all of the above functions except those that rely on `singleton`
-- (`insert`, `delete`, `fromList`) could be generalized to any `Foldable`.

-- > I would probably also want `intersection`, not sure what else, but I think
-- that would need `singleton` as well. I think there's a `FreeList` class
-- (according to Eitan, but I can't find implementation on Hackage), that
-- extends `Foldable` with a singleton method. In that case, all these functions
-- would fall out of the type class instance.

class Foldable t => FreeList t where
  singleton' :: a -> t a

-- Actually `intersection` might not be so simple

intersection :: Ord a => OrderedList a -> OrderedList a -> OrderedList a
OL [] `intersection` _ = OL []
_ `intersection` OL [] = OL []
OL (x : xs) `intersection` OL (y : ys)
  | x == y = OL (x : getOrderedList do OL xs `intersection` OL ys)
  | x < y = OL xs `intersection` OL (y : ys)
  | otherwise = OL (x : xs) `intersection` OL ys

-- | Doesn't work :\
-- intersection :: Ord a => OrderedList a -> OrderedList a -> OrderedList a
-- intersection xs = foldMap (guard <$> flip member xs <*> singleton)

-- | Just delete one
delete' :: Ord a => a -> OrderedList a -> OrderedList a
delete' x = snd . foldr f (True, emptyOL)
  where
    f y (p, acc)
      | p && y == x = (not p, acc)
      | otherwise = (p, insert y acc)

intersection' :: Ord a => OrderedList a -> OrderedList a -> OrderedList a
intersection' xs = snd . foldr f (xs, emptyOL)
  where
    f y a@(xs', acc)
      | y `member` xs' = (delete' y xs', insert y acc)
      | otherwise = a
