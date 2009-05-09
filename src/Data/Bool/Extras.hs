module Data.Bool.Extras
  ( module Data.Bool
  , bool
  , boolM
  ) where

import Data.Bool
import Data.Monoid

-- | Defines the fold over a boolean data type.
-- Comparable to the `maybe' or `either' functions.
bool :: a -> a -> Bool -> a
bool x _ True  = x
bool _ y False = y

-- | Boolean operation for monoids.
-- Behaves like `id` when applied to `True`,
-- returns `mempty` for when applied to `False`
boolM :: (Monoid a) => a -> Bool -> a
boolM x True  = x
boolM _ False = mempty

