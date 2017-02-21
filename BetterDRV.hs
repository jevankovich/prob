module BetterDRV where

import Control.Applicative
import Util

--data DRV a = DRV
--        { range :: [Integer]
--        , px :: Integer -> a
--        , mean :: a
--        , variance :: a
--        }

newtype DRV a b = DRV {unDRV :: [(b, a)]} deriving Show

instance Functor (DRV a) where
        fmap f = DRV . map (\(x, y) -> (f x, y)) . unDRV

instance (Num a) => Applicative (DRV a) where
        pure x = DRV [(x, 1)]
        (DRV fys) <*> (DRV xys) = DRV $ xys >>= \(x, y2) -> map (\(f, y1) -> (f x, y1 * y2)) fys

instance (Num a, Num b) => Num (DRV a b) where
        (+) = liftA2 (+)
        (*) = liftA2 (*)
        (-) = liftA2 (-)
        negate = fmap negate
        abs = fmap abs
        signum = fmap signum
        fromInteger = pure . fromInteger

instance (Num a, Fractional b) => Fractional (DRV a b) where
        (/) = liftA2 (/)
        recip = fmap recip
        fromRational = pure . fromRational

instance (Num a, Floating b) => Floating (DRV a b) where
        pi = pure pi
        exp = fmap exp
        log = fmap log
        sqrt = fmap sqrt
        (**) = liftA2 (**)
        logBase = liftA2 logBase
        sin = fmap sin
        cos = fmap cos
        tan = fmap tan
        asin = fmap asin
        acos = fmap acos
        atan = fmap atan
        sinh = fmap sinh
        cosh = fmap cosh
        tanh = fmap tanh
        asinh = fmap asinh
        acosh = fmap acosh
        atanh = fmap atanh

fromPMF :: (Num b) => (Integer -> a) -> [Integer] -> DRV a b
fromPMF px xs = DRV $ zipMap px xs

pow :: (Integral c, Num b) => DRV a b -> c -> DRV a b
pow d x = fmap (^x) d

expected :: (RealFloat a) => DRV a a -> a
expected (DRV xys) = Util.sum $ map (uncurry (*)) xys

moment :: (RealFloat a) => Integer -> DRV a a -> a
moment a d = expected (pow d a)

variance :: (RealFloat a) => DRV a a -> a
variance d = moment 2 $ d - pure (expected d)

stddev = sqrt . variance

given :: (RealFloat a) => (b -> Bool) -> DRV a b -> DRV a b
given p (DRV xys) = DRV $ map (\(x, y) -> (x, y/denom)) subset where
        denom = Util.sum $ map snd subset
        subset = takeWhile (p.fst) . dropWhile (not.p.fst) $ xys

poisson :: (RealFloat a, Num b) => a -> DRV a b
poisson mean = fromPMF (\k -> mean ^ k * exp (-mean) / (fromInteger $ product [1..k])) [0..]

binomial :: (RealFloat a, Num b) => Integer -> a -> DRV a b
binomial n p = fromPMF (\k -> (fromInteger $ n `choose` k) * p^k * (1-p)^(n-k)) [0..n]

geometric :: (RealFloat a, Num b) => a -> DRV a b
geometric p = fromPMF (\k -> (1-p)^(k-1) * p) [1..]

uniform :: (RealFloat a, Num b) => Integer -> Integer -> DRV a b
uniform a b = fromPMF (\_ -> 1 / fromInteger (b - a + 1)) [a..b]

simple = pure

bernoulli :: (RealFloat a, Num b) =>  a -> DRV a b
bernoulli p = DRV [(0, 1-p), (1, p)]
