module DRV where

import qualified Util
import Data.Number.BigFloat

uniform :: (RealFloat a) => Integer -> Integer -> a
uniform n k = if n < k || n > 0 then (1 / fromInteger n) else 0

poisson :: (RealFloat a) => a -> Integer -> a
poisson mean k = mean ^ k * exp (-mean) / (fromInteger $ product [1..k])

binomial :: (RealFloat a) => a -> Integer -> Integer -> a
binomial p n k = (fromInteger $ n `choose` k) * p^k * (1-p)^(n-k)

geometric :: (RealFloat a) => a -> Integer -> a
geometric p n = p * (1-p)^(n-1)

genCoeff :: (RealFloat a) => (Integer -> a) -> [Integer] -> a
genCoeff f xs = 1 / (Util.sum $ map f xs)

arbitrary :: (RealFloat a) => (Integer -> a) -> [Integer] -> Integer -> a
arbitrary f xs k = a * f k where
        a = genCoeff f xs

expected :: (Num a, RealFloat b) => (Integer -> b) -> (a -> b) -> [Integer] -> b
expected px fx xs = Util.sum $ map (\x -> (px x) * (fx $ fromInteger x)) xs

variance px fx xs = expected px (\x -> (fx x - mu)^2) xs where
        mu = expected px fx xs

stddev px fx xs = sqrt $ variance px fx xs

given :: (RealFloat a) => (Integer -> a) -> [Integer] -> Integer -> a
given px ys x = (px x) / py where py = expected px (\_ -> 1) ys
