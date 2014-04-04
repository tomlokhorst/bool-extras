{-# LANGUAGE CPP #-}
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
  , mwhenM
  , whenA
  , whenC
  , whenM

  -- * Morphisms
  , BoolAlgebra
  , cata
  , ana
  ) where

import Control.Arrow
import Control.Category (Category)
import qualified Control.Category as Cat
import Control.Monad
import Data.Bool
import Data.Monoid

#if !MIN_VERSION_base(4,7,0)
-- | Defines the fold over a boolean value.
--
-- Returns its first argument when applied to `False',
-- returns its second argument when applied to `True'.
-- 
-- Comparable to the `maybe' or `either' functions for their respective data
-- types.
{-# INLINE bool #-}
bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True  = y
-- Expressed in terms of `cata':
-- bool = curry cata
#endif

-- | Boolean operation for monoids.
-- 
-- Returns its first argument when applied to `True',
-- returns `mempty' when applied to `False'.
mwhen :: (Monoid a) => a -> Bool -> a
mwhen = bool mempty

-- | Boolean operation for monads, with a monoid default.
-- 
-- Return its first argument when applied to `True',
-- returns `return mempty' when applied to `False'.
mwhenM :: (Monad m, Monoid a) => m a -> Bool -> m a
mwhenM = bool (return mempty)

-- | Boolean operation for arrows.
-- 
-- Returns its first argument when applied to `True',
-- returns `returnA' when applied to `False'.
whenA :: (Arrow a) => a b b -> Bool -> a b b
whenA = bool returnA

-- | Boolean operation for categories.
-- 
-- Returns its first argument when applied to `True',
-- returns @Control.Category.@`Cat.id' when applied to `False'.
whenC :: (Category cat) => cat a a -> Bool -> cat a a
whenC = bool Cat.id

-- | Boolean operation for monads.
-- 
-- Returns its first argument when applied to `True',
-- returns `return' when applied to `False'.
--
-- @Control.Monad.@`when' can be expressed in terms of `whenM', like so:
-- 
-- > when :: Monad m => Bool -> m () -> m ()
-- > when b m = (const m `whenM` b) ()
whenM :: (Monad m) => (a -> m a) -> Bool -> (a -> m a)
whenM = bool return
-- Alternative implementation using Kleisli arrows:
-- whenM m = runKleisli . whenC (Kleisli m)

{-
-- Functions that are also possible, but we haven't found an explicit need for

whenP :: (MonadPlus m) => a -> Bool -> m a
whenP = bool mzero . return

(<?>) :: (Applicative f) => (a -> f a) -> Bool -> (a -> f a)
(<?>) = bool pure
-}


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
ana f b = f b

