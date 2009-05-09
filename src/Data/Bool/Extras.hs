module Data.Bool.Extras
  ( module Data.Bool
  , bool
  ) where

import Data.Bool

-- | Defines the fold over a boolean data type.
-- Comparable to the `maybe' or `either' functions.
bool :: a -> a -> Bool -> a
bool x _ True  = x
bool _ y False = y

