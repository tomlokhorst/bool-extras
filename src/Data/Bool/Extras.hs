-- | This module provides some convenient functions for dealing with Booleans.
-- 
-- The most important one being 'bool', a function that can be used in place of
-- the build-in @if then else@-syntax.
module Data.Bool.Extras
  (
  -- * Main function
    bool

  -- * Other functions
  , mwhen
  , whenA
  , whenC
  , whenM

  -- * Morphisms
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
-- 
-- Comparable to the `maybe' or `either' functions.
bool :: a -> a -> Bool -> a
bool x _ True  = x
bool _ y False = y
-- Less straightforward implementation:
-- bool = flip (curry cata)

-- | Boolean operation for monoids.
-- 
-- Returns its first argument when applied to `True',
-- returns `mempty' when applied to `False'.
mwhen :: (Monoid a) => a -> Bool -> a
mwhen x True  = x
mwhen _ False = mempty

-- | Boolean operation for arrows.
-- 
-- Returns its first argument when applied to `True',
-- returns `returnA' when applied to `False'.
whenA :: Arrow a => a b b -> Bool -> a b b
whenA a True  = a
whenA _ False = returnA

-- | Boolean operation for categories.
-- 
-- Returns its first argument when applied to `True',
-- returns `Control.Category.id' when applied to `False'.
whenC :: Category cat => cat b b -> Bool -> cat b b
whenC c True  = c
whenC _ False = Cat.id

-- | Boolean operation for monads.
-- 
-- Returns its first argument when applied to `True',
-- returns `Control.Category.id' when applied to `False'.
--
-- `Control.Monad.when' can be expressed in terms of `whenM', like so:
-- 
-- > when :: Monad m => Bool -> m () -> m()
-- > when b m = (const m `whenM` b) ()
whenM :: Monad m => (b -> m b) -> Bool -> (b -> m b)
whenM m = runKleisli . whenC (Kleisli m)

-- | Algebra for Bool data type.
-- 
-- The first field of the pair represents the `False' value,
-- the second field represents the `True' value.
type BoolAlgebra r = (r, r)

-- | Catamorphism for booleans.
cata :: BoolAlgebra r -> Bool -> r
cata (x, _) False = x
cata (_, y) True  = y

-- | Anamorphism for booleans.
ana :: (b -> Bool) -> b -> Bool
ana = id

