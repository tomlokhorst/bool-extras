module Arrow where

import Data.Bool.Extras

maybePlus5 :: Bool -> Int -> Int
maybePlus5 = boolA (+5)

main = print (maybePlus5 True 4)

