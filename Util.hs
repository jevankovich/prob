module Util 
        ( Util.sum
        , fixedPoint
        , infRange
        , showLongList
        , choose
        , zipMap
        ) where

fixedPoint :: (Eq a) => [a] -> a
fixedPoint [] = error "Empty List passed to fixedPoint"
fixedPoint [a] = a
fixedPoint (a:b:xs) = if a == b then a else fixedPoint (b:xs)

sum [] = 0
sum [x] = x
sum xs = fixedPoint . takeWhile (not . isNaN) . dropWhile (== 0) . scanl1 (+) $ xs

interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs

infRange = interleave [0..] [-1,-2..]

showLongList' xs
        | length xs == 10 = show xs ++ "..."
        | otherwise = show xs

showLongList :: (Show a) => [a] -> String
showLongList xs = showLongList' $ take 10 xs

choose :: (RealFloat a) => Integer -> Integer -> Integer
n `choose` k = product [k+1..n] `div` product [1..n-k]

zipMap :: (Num a) => (Integer -> b) -> [Integer] -> [(a, b)]
zipMap f xs = zip (map fromInteger xs) (map f xs)
