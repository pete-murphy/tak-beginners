{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Homework.Tape where

import Control.Comonad
import Control.Comonad.Cofree
import Data.Distributive
import Data.Functor.Rep
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as S

-- data Tape a = Tape [a] a [a]

data TPossible a
  = TPossible
      { leftward :: a,
        rightward :: a
      }
  deriving (Show, Eq, Functor)

data TChoice = L | R
  deriving (Show, Eq)

-- instance Distributive [] where
--   distribute :: Functor f => f [a] -> [f a]
--   distribute fas =

newtype Identity a
  = Identity {getIdentity :: a}
  deriving (Eq, Show, Functor)

newtype BoolTo a
  = BoolTo {getBoolTo :: Bool -> a}
  deriving (Functor)

instance Distributive Identity where
  distribute :: Functor f => f (Identity a) -> Identity (f a)
  distribute = Identity . fmap getIdentity

instance Distributive BoolTo where
  distribute :: Functor f => f (BoolTo a) -> BoolTo (f a)
  distribute fa = BoolTo \b -> fmap ((\f -> f b) . getBoolTo) fa
