{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module CRV where

import Util

import Data.Number.Erf
import Numeric.AD
import Numeric.AD.Internal.Type (AD)
import Numeric.AD.Internal.Forward (Forward)
import Numeric.Integration.TanhSinh

data CRV s a = FromCDF { support :: (a, a), c :: forall s. AD s (Forward a) -> AD s (Forward a) }
             | FromPDF { support :: (a, a), p :: a -> a }

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

exponential :: (Floating a) => (forall s. AD s (Forward a)) -> CRV s a
exponential k = FromCDF 
        { support = (0, posInf)
        , c = \x -> 1 - exp (-k * x)
        }

gaussian :: (Floating a, Erf a) => (forall s. AD s (Forward a)) -> (forall s. AD s (Forward a)) -> CRV s a
gaussian mu sigma = FromCDF
        { support = (negInf, posInf)
        , c = \x -> 1 / 2 * (1 + erf ((x - mu) / (sigma * sqrt 2)))
        }

arbitraryPDF :: (Double -> Double) -> Double -> Double -> CRV s Double
arbitraryPDF p l h = FromPDF { support = (l, h), p = \x -> (p x) / integrate p l h }

pdf :: Num a => (forall s. CRV s a) -> a -> a
pdf crv@(FromPDF {p = p}) = p
pdf crv = diff $ c crv

cdf :: (forall s. CRV s Double) -> Double -> Double
cdf crv@(FromCDF {c = c}) = fst . diff' c
cdf crv@(FromPDF {support = (a, _), p = p}) = \x -> integrate p a x

-- TODO: May be broken
prob :: (forall s. CRV s Double) -> Double -> Double -> Double
prob crv@(FromCDF _ _) l h = c h - c l where
        c = cdf crv
prob (FromPDF _ pdf) l h = integrate pdf l h

given :: CRV s Double -> Double -> Double -> CRV s Double
given crv@(FromCDF _ cdf) l h =
            FromCDF { support = (l, h), c = \x
            -> (cdf x - cdf (auto l)) / (cdf (auto h) - cdf (auto l)) }
given crv@(FromPDF _ pdf) a b = FromPDF { support = (a, b), p = \x -> (pdf x) / (integrate pdf a b) }

expected :: (forall s. CRV s Double) -> (Double -> Double) -> Double
expected crv f = let (a, b) = support crv in integrate (\(x :: Double) -> (pdf crv x) * (f x)) a b

mean :: (forall s. CRV s Double) -> Double
mean crv = expected crv id

variance :: (forall s. CRV s Double) -> Double
variance crv = expected crv (^2) - (expected crv id)^2

stddev :: (forall s. CRV s Double) -> Double
stddev crv = sqrt $ variance crv
