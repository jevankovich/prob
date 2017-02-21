{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module CRV where

import Util

import Data.Number.Erf
import Numeric.AD
import Numeric.AD.Internal.Type (AD)
import Numeric.AD.Internal.Forward (Forward)
import Numeric.Integration.TanhSinh

data CRV s a = CRV { support :: (a, a), cdf :: (AD s (Forward a) -> AD s (Forward a)) }

posInf :: (Floating a) => a
posInf = 1/0

negInf :: (Floating a) => a
negInf = -posInf

epsilon :: Double
epsilon = 1e-10

integrate :: (Double -> Double) -> Double -> Double -> Double

integrate f a b | a > b = -(integrate f b a)
                | a == negInf && b == posInf = result . relative epsilon $ everywhere parSimpson f
                | b == posInf = result . relative epsilon $ nonNegative parSimpson (\x -> f (x + a))
                | otherwise = result . relative epsilon $ parSimpson f a b

exponential :: (Floating a) => (AD s (Forward a)) -> CRV s a
exponential k = CRV 
        { support = (0, posInf)
        , cdf = \x -> 1 - exp (-k * x)
        }

gaussian :: (Floating a, Erf a) => (AD s (Forward a)) -> (AD s (Forward a)) -> CRV s a
gaussian mu sigma = CRV
        { support = (negInf, posInf)
        , cdf = \x -> 1 / 2 * (1 + erf ((x - mu) / (sigma * sqrt 2)))
        }

pdf :: Num a => (forall s.
        CRV s a)
             -> a -> a
pdf crv = diff $ cdf crv

given :: (Fractional a) => (a -> a) -> a -> a -> (a -> a)
given cdf a b = \x -> (cdf x - cdf a) / (cdf b - cdf a)

expected :: (forall s. CRV s Double) -> (Double -> Double) -> Double
expected crv@(CRV { support = (a, b)}) f = integrate (\(x :: Double) -> (pdf crv x) * (f x)) a b

mean :: (forall s. CRV s Double) -> Double
mean crv = expected crv id

variance :: (forall s. CRV s Double) -> Double
variance crv = expected crv (^2) - (expected crv id)^2

stddev :: (forall s. CRV s Double) -> Double
stddev crv = sqrt $ variance crv
