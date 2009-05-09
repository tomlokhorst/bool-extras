module Morphisms where

import Data.Bool.Extras

bit :: Bool -> Int
bit = cata (0, 1)

main = do
  print (bit False)
  print (ana even 3)

