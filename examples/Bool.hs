module Bool where

import Data.Bool.Extras

yesOrNo :: Bool -> String
yesOrNo = bool "yes" "no"

main = putStrLn (yesOrNo True)

