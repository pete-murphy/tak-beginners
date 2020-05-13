{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Homework.W04 where

import Data.Coerce (Coercible, coerce)
import Data.Function (on)

data Option a = None | Some a
  deriving (Show)

instance Eq a => Eq (Option a) where
  None == None = True
  Some x == Some y = x == y
  _ == _ = False

instance Ord a => Ord (Option a) where
  Some x <= Some y = x <= y
  None <= _ = True
  _ <= None = False

newtype All = All {getAll :: Bool}
  deriving (Show)

instance Semigroup All where
  All x <> All y = All (x && y)

instance Monoid All where
  mempty = All True

newtype Any = Any {getAny :: Bool}
  deriving (Show)

instance Semigroup Any where
  Any x <> Any y = Any (x || y)

instance Monoid Any where
  mempty = Any False

newtype IntSum = IntSum {getIntSum :: Int}
  deriving (Show)

instance Semigroup IntSum where
  IntSum x <> IntSum y = IntSum (x + y)

instance Monoid IntSum where
  mempty = IntSum 0

newtype IntProduct = IntProduct {getIntProduct :: Int}
  deriving (Show)

instance Semigroup IntProduct where
  (<>) = IntProduct .:. on (*) getIntProduct
    where
      (.:.) = (.) . (.)

instance Monoid IntProduct where
  mempty = IntProduct 1

-- | In PureScript there's a @Newtype@ class, and it seems like there's one in
-- Haskell as well (https://hackage.haskell.org/package/newtype-0.2.2.0), which
-- seems to handle all this coercing, so I'm just experimenting here to see how
-- this all works
over ::
  Coercible a (n a) =>
  (a -> a -> a) ->
  (n a -> n a -> n a)
over = coerce

wrap :: Coercible a (n a) => a -> n a
wrap = coerce

newtype Sum a = Sum {getSum :: a}
  deriving (Show)

instance (Num a) => Semigroup (Sum a) where
  (<>) = over (+)

instance (Num a) => Monoid (Sum a) where
  mempty = wrap 0

newtype Product a = Product {getProduct :: a}
  deriving (Show)

instance Num a => Semigroup (Product a) where
  (<>) = over (*)

instance (Num a) => Monoid (Product a) where
  mempty = wrap 1

un :: Coercible a (n a) => n a -> a
un = coerce

newtype Down a = Down {getDown :: a}
  deriving (Show)

instance Eq a => Eq (Down a) where
  (==) = on (==) un

instance Ord a => Ord (Down a) where
  compare = on (flip compare) un

data List a = Nil | Cons a (List a)
  deriving (Show)

instance Semigroup (List a) where
  Nil <> ys = ys
  Cons x xs <> ys = Cons x (xs <> ys)

instance Monoid (List a) where
  mempty = Nil

newtype First a = First {getFirst :: a}
  deriving (Show)

instance Semigroup (First a) where
  (<>) = const

newtype Last a = Last {getLast :: a}
  deriving (Show)

instance Semigroup (Last a) where
  (<>) _ = id

newtype Max a = Max {getMax :: a}
  deriving (Show)

instance Ord a => Semigroup (Max a) where
  (<>) = over max

instance (Bounded a, Ord a) => Monoid (Max a) where
  mempty = wrap minBound

newtype Min a = Min {getMin :: a}
  deriving (Show)

instance Ord a => Semigroup (Min a) where
  (<>) = over min

instance (Bounded a, Ord a) => Monoid (Min a) where
  mempty = wrap maxBound

newtype Endo a = Endo {getEndo :: a -> a}

-- | Inference doesn't work here without @f ~ (a -> a)@:
-- @
-- over' :: Coercible (a -> a) (n a)
--       => ((a -> a) -> (a -> a) -> (a -> a))
--       -> (n a -> n a -> n a)
-- @
-- Fails with:
-- >   • Non type-variable argument
-- >       in the constraint: Coercible (a -> a) (n a)
-- >     (Use FlexibleContexts to permit this)
-- >   • In the type signature:
-- >       over' :: ...
-- But I'm not sure why this fixes it
over' ::
  (f ~ (a -> a), Coercible f (n a)) =>
  (f -> f -> f) ->
  (n a -> n a -> n a)
over' = coerce

wrap' ::
  (f ~ (a -> a), Coercible f (n a)) =>
  f ->
  n a
wrap' = coerce

instance Semigroup (Endo a) where
  (<>) = over' (flip (.))

instance Monoid (Endo a) where
  mempty = wrap' id

data Pair a b = Pair a b
  deriving (Show, Eq)

-- | I know there's a couple options here, I have no reason for preferring this
-- one
instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  Pair x y <> Pair x' y' = Pair (x <> x') (y <> y')

newtype StrangePair a b = StrangePair {getStrangePair :: Pair a b}
  deriving (Show, Eq)

instance Semigroup (StrangePair a b) where
  StrangePair (Pair x _) <> StrangePair (Pair _ y) = StrangePair (Pair x y)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = Pair mempty mempty

data Unit = Unit
  -- deriving (Bounded, Eq, Ord)
  -- deriving (Semigroup, Monoid) via (Max Unit)
  deriving (Show)

instance Semigroup Unit where
  (<>) = const

instance Monoid Unit where
  mempty = Unit

instance Semigroup a => Semigroup (Option a) where
  x <> None = x
  None <> y = y
  Some x <> Some y = Some (x <> y)

instance Semigroup a => Monoid (Option a) where
  mempty = None

newtype Dual a = Dual {getDual :: a}
  deriving (Show)

instance Semigroup a => Semigroup (Dual a) where
  Dual x <> Dual y = Dual (y <> x)

instance Monoid m => Monoid (Dual m) where
  mempty = Dual mempty

mapOption :: (a -> b) -> Option a -> Option b
mapOption f = \case
  Some x -> Some (f x)
  None -> None

first :: [a] -> Option a
first = mapOption getFirst . mconcat . map (Some . First)

final :: [a] -> Option a
final = mapOption getFirst . getDual . mconcat . map (Dual . Some . First)

average :: [Double] -> Option Double
average (mconcat . map (\a -> Pair (Sum a) (Sum 1)) -> Pair (Sum x) (Sum y))
  | y == 0 = None
  | otherwise = Some (x / y)
