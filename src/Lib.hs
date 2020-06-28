module Lib where

guard :: Monoid m => Bool -> m -> m
guard b m = if b then m else mempty
