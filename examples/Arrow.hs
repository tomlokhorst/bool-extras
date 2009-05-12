module Arrow where

import Data.Bool.Extras

maybePlus5 :: Bool -> Int -> Int
maybePlus5 b = (+5) `whenA` b

main = print (maybePlus5 True 4)

