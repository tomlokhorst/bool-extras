-- | This module provides some convenient functions for dealing with Booleans.
-- 
-- The most important one being 'bool', a function that can be used in place of
-- the build-in @if then else@-syntax.
module Data.Bool.Extras
  (
  -- * Main function
    bool
  -- * Other functions
  , boolM
  , BoolAlgebra
  , cata
  , ana
  ) where

import Data.Bool
import Data.Monoid
import Control.Arrow
import Control.Category (Category)
import qualified Control.Category as Cat

-- | Defines the fold over a boolean data type.
-- Comparable to the `maybe' or `either' functions.
bool :: a -> a -> Bool -> a
bool x _ True  = x
bool _ y False = y

-- | Boolean operation for monoids.
-- Behaves like `id` when applied to `True`,
-- returns `mempty` for when applied to `False`.
boolM :: (Monoid a) => a -> Bool -> a
boolM x True  = x
boolM _ False = mempty

-- | Boolean operation for arrows.
-- Behaves like `id` when applied to `True`,
-- return `returnA` when applied to `False`.
boolA :: Arrow a => a b b -> Bool -> a b b
boolA a True  = a
boolA _ False = returnA

-- | Boolean operation for categories.
-- Behaves like `id` when applied to `True`,
-- return `Cat.id` when applied to `False`.
boolC :: Category cat => cat b b -> Bool -> cat b b
boolC c True  = c
boolC _ False = Cat.id

-- | Algebra for Bool data type.
type BoolAlgebra r = (r, r)

-- | Catamorphism for booleans.
cata :: BoolAlgebra r -> Bool -> r
cata (x, _) False = x
cata (_, y) True  = y

-- | Anamorphism for booleans.
ana :: (b -> Bool) -> b -> Bool
ana = id

