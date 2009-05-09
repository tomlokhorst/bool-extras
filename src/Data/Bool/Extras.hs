module Data.Bool.Extras
  ( module Data.Bool
  , bool
  , boolM
  , BoolAlgebra
  , cata
  , ana
  ) where

import Data.Bool
import Data.Monoid

-- Straightforward implementation (without the weird catamorphism stuff):
-- bool x _ True  = x
-- bool _ y False = y

-- | Defines the fold over a boolean data type.
-- Comparable to the `maybe' or `either' functions.
bool :: a -> a -> Bool -> a
bool = flip (curry cata)

-- | Boolean operation for monoids.
-- Behaves like `id` when applied to `True`,
-- returns `mempty` for when applied to `False`
boolM :: (Monoid a) => a -> Bool -> a
boolM x True  = x
boolM _ False = mempty

-- | Algebra for Bool data type.
type BoolAlgebra r = (r, r)

-- | Catamorphism for booleans.
cata :: BoolAlgebra r -> Bool -> r
cata (x, _) False = x
cata (_, y) True  = y

-- | Anamorphism for booleans.
ana :: (b -> Bool) -> b -> Bool
ana = id

