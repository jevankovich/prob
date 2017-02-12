module Util 
        (Util.sum
        ) where

fixedPoint :: (Eq a) => [a] -> a
fixedPoint [a] = a
fixedPoint (a:b:xs) = if a == b then a else fixedPoint (b:xs)

sum [] = 0
sum xs = fixedPoint . takeWhile (not . isNaN) . scanl1 (+) $ xs
