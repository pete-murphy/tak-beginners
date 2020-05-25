{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Homework.W06 where

import Control.Applicative
import Data.Semigroup (All (..), Any (..))
import Homework.W05 (Power (..), Tree (..))

data Option a = None | Some a
  deriving (Eq, Show)

instance Functor Option where
  fmap _ None = None
  fmap f (Some x) = Some (f x)

instance Applicative Option where
  pure = Some
  liftA2 _ None _ = None
  liftA2 _ _ None = None
  liftA2 f (Some x) (Some y) = Some (f x y)

instance Monad Option where
  None >>= _ = None
  Some x >>= f = f x

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> list = list
  Cons x xs <> list = Cons x (xs <> list)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  -- pure x = Cons x (pure x)
  -- liftA2 f _ Nil = Nil
  -- liftA2 f Nil _ = Nil
  -- liftA2 f (Cons x xs) (Cons y ys) = f x y `Cons` liftA2 f xs ys
  pure x = Cons x Nil
  liftA2 f _ Nil = Nil
  liftA2 f Nil _ = Nil
  liftA2 f (Cons x xs) ys = (f x <$> ys) <> liftA2 f xs ys

instance Monad List where
  Nil >>= _ = Nil
  Cons x xs >>= f = f x <> (xs >>= f)

-- Applicative: Identity
--   pure id <*> v === v
-- Applicative: Composition
--   pure (.) <*> u <*> v <*> w === u <*> (v <*> w)
-- Applicative: Homomorphism
--   pure f <*> pure x === pure (f x)
-- Applicative: Interchange
--   u <*> pure y === pure ($ y) <*> u

-- Applicative: LiftA2 Part 1
-- Applicative: LiftA2 Part 2

-- data Tree a = Leaf | Branch a (Tree a) (Tree a)
--   deriving (Eq, Show)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Branch x t t') = Branch (f x) (fmap f t) (fmap f t')

data Pair a b = Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

-- data Power a = Power Int a
--   deriving (Eq, Show)

instance Functor Power where
  fmap f (Power n x) = Power n (f x)

newtype From r a = From (r -> a)

instance Functor (From r) where
  fmap f (From g) = From (f . g)

newtype To r a = To (a -> r)

{-
Can't do it:
instance Functor (To r) where
  fmap f (To g) = To h
    where
      h :: b -> r
      h = undefined

there's no way of combining `f :: a -> b`
and `g :: a -> r`
to get `h :: b -> r`
-}

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

instance Contravariant (To r) where
  contramap f (To g) = To (g . f)

newtype OrderedList a = OrderedList {getOrderedList :: [a]}
  deriving (Eq, Show)

{-
Also can't write a Functor instance for this: in order to preserve ordering,
you'd have to use `(<>)`, which needs an `Ord` instance for whatever's inside
the `OrderedList`, but `fmap` doesn't allow you to constrain it so.
-}

-- $> stupid "thingy" [("thingy", "wingy"), ("stuff", "things")]
-- Just False

-- $> stupid "stuff" [("thingy", "wingy"), ("stuff", "things")]
-- Just True

-- $> stupid "missing" [("thingy", "wingy"), ("stuff", "things")]
-- Nothing

stupid :: (Eq k, Foldable t) => k -> [(k, t a)] -> Maybe Bool
stupid k = (even <$> length) <$$> lookup k
  where
    (<$$>) = (<$>) <$> (<$>)

-- stupid :: forall k t a. (Eq k, Foldable t) => k -> [(k, t a)] -> Maybe Bool
-- stupid key xs = fmap getAny $ foldMap go $ fmap (fmap (Any . even . length)) xs
--   where
--     go :: (k, Any) -> Maybe Any
--     go (k, isEven) = if k == key then Just isEven else Nothing

foo :: forall k a. (Eq k) => k -> [(k, a)] -> Maybe a
foo key = foldr go Nothing
  where
    go :: (k, a) -> Maybe a -> Maybe a
    go (k, a) Nothing = if key == k then Just a else Nothing
    go _ x = x
