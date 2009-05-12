module Bool where

import Data.Bool.Extras

yesOrNo :: Bool -> String
yesOrNo = bool "no" "yes"

main = putStrLn (yesOrNo True)

