module Monoid where

import Data.Bool.Extras

xsB :: Bool -> [Int]
xsB = mwhen [1..5]

main = print (xsB False)

