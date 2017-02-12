module DRV where

import qualified Util

uniform :: Integer -> Integer -> Double
uniform n k = if n < k || n > 0 then (1 / fromInteger n) else 0

poisson :: Double -> Integer -> Double
poisson lambda k = lambda ^ k * exp (-lambda) / (fromInteger $ product [1..k])

choose :: Integer -> Integer -> Integer
n `choose` k = product [k+1..n] `div` product [1..n-k]

binomial:: Double -> Integer -> Integer -> Double
binomial p n k = (fromInteger $ n `choose` k) * p^k * (1-p)^(n-k)

geometric :: Double -> Integer -> Double
geometric p n = p * (1-p)^(n-1)

arbitrary :: (Integer -> Double) -> [Integer] -> Integer -> Double
arbitrary f xs k = f k / aInv where
        aInv = Util.sum $ map f xs

expected :: (Num a) => (Integer -> Double) -> (a -> Double) -> [Integer] -> Double
expected px fx xs = Util.sum $ map (\x -> (px x) * (fx $ fromInteger x)) xs

variance px fx xs = expected px (\x -> (fx x - mu)^2) xs where
        mu = expected px fx xs

stddev px fx xs = sqrt $ variance px fx xs
