module Diff where

import Data.List (intersperse, foldl', sort, partition)

data Expr a = Const a | Sym String | Prod [Expr a] | Quo (Expr a) (Expr a) | Sum [Expr a] | Exp (Expr a) | Ln (Expr a) deriving (Eq)

x = Sym "x"

type Rule a = (Expr a -> Bool, Expr a -> Expr a)

rewrite :: Rule a -> Expr a -> Expr a
rewrite (p, f) expr | p expr = f expr
rewrite r (Prod xs) = Prod $ map (rewrite r) xs
rewrite r (Sum xs) = Sum $ map (rewrite r) xs
rewrite r (Quo n d) = Quo (rewrite r n) (rewrite r d)
rewrite r (Exp x) = Exp $ rewrite r x
rewrite r (Ln x) = Ln $ rewrite r x
rewrite _ x = x

fixed :: (Eq a) => [a] -> a
fixed (a:b:xs) | a == b = a
               | otherwise = fixed (b:xs)

rewriteFix :: (Eq a) => Rule a -> Expr a -> Expr a
rewriteFix r e = fixed . iterate (rewrite r) $ e

chain :: [(a -> a)] -> a -> a
chain = foldl1 (.)

rewriteMany :: (Eq a) => [Rule a] -> Expr a -> Expr a
rewriteMany rs = fixed . iterate (chain . map rewrite $ rs)

simplify :: (Num a, Eq a) => Expr a -> Expr a
simplify = rewriteMany
        [ ( \e -> case e of
                    (Prod xs) -> 0 `elem` xs
                    _ -> False
          , \_ -> 0)
        , ( \e -> case e of
                    (Prod xs) -> 1 `elem` xs
                    _ -> False
          , \(Prod xs) -> Prod $ filter (/= 1) xs
          )
        , ( \e -> case e of
                    (Sum xs) -> 0 `elem` xs
                    _ -> False
          , \(Sum xs) -> Sum $ filter (/= 0) xs
          )
        , ( \e -> case e of
                    (Prod []) -> True
                    _ -> False
          , \(Prod []) -> 1
          )
        , ( \e -> case e of
                    (Sum []) -> True
                    _ -> False
          , \(Sum []) -> 0
          )
        , ( \e -> case e of
                    (Prod [_]) -> True
                    _ -> False
          , \(Prod [x]) -> x
          )
        , ( \e -> case e of
                    (Sum [_]) -> True
                    _ -> False
          , \(Sum [x]) -> x
          )
        , ( \e -> case e of
                    (Ln (Exp _)) -> True
                    _ -> False
          , \(Ln (Exp x)) -> x
          )
        , ( \e -> case e of
                    (Exp (Ln _)) -> True
                    _ -> False
          , \(Exp (Ln x)) -> x
          )
        , ( \e -> case e of
                    (Exp 0) -> True
                    (Ln 1) -> True
                    _ -> False
          , \_ -> 1
          )
        , ( \e -> case e of
                    (Quo 0 _) -> True
                    (Quo x 1) -> True
                    _ -> False
          , \(Quo x _) -> x
          )
        , ( \e -> case e of
                    (Prod xs) -> any isProd xs
                    _ -> False
          , \(Prod xs) -> let (prods, notProds) = partition isProd xs in
                              Prod $ (concatMap (\(Prod xs) -> xs) prods) ++ notProds
          )
        , ( \e -> case e of
                    (Sum xs) -> any isSum xs
                    _ -> False
          , \(Sum xs) -> let (sums, notSums) = partition isSum xs in
                             Sum $ (concatMap (\(Sum xs) -> xs) sums) ++ notSums
          )
        , ( \e -> case e of
                    (Prod xs) -> any isConst xs
                    _ -> False
          , \(Prod xs) -> let (consts, notConsts) = partition isConst xs in
                              Prod $ (Const . product . map (\(Const x) -> x) $ consts) : notConsts
          )
        , ( \e -> case e of
                    (Sum xs) -> any isConst xs
                    _ -> False
          , \(Sum xs) -> let (consts, notConsts) = partition isConst xs in
                             Sum $ (Const . sum . map (\(Const x) -> x) $ consts) : notConsts
          )
        , ( \e -> case e of
                    (Prod xs) -> any isQuo xs
                    _ -> False
          , \(Prod xs) -> let (quos, notQuos) = partition isQuo xs in
                              let num = Prod $ (map (\(Quo n _) -> n) quos) ++ notQuos in
                                  let denom = Prod $ (map (\(Quo _ d) -> d) quos) in
                                      Quo num denom
          )
        , ( \e -> case e of
                    (Quo (Quo _ _) (Quo _ _)) -> False
                    (Quo _ (Quo _ _)) -> True
                    _ -> False
          , \(Quo a (Quo b c)) -> Quo (a * c) b
          )
        , ( \e -> case e of
                    (Quo (Quo _ _) (Quo _ _)) -> False
                    (Quo (Quo _ _) _) -> True
                    _ -> False
          , \(Quo (Quo a b) c) -> Quo a (b * c)
          )
        , ( \e -> case e of
                    (Quo (Quo _ _) (Quo _ _)) -> True
                    _ -> False
          , \(Quo (Quo a b) (Quo c d)) -> Quo (a * d) (b * c)
          )
        ]

isProd (Prod _) = True
isProd _ = False

isSum (Sum _) = True
isSum _ = False

isConst (Const _) = True
isConst _ = False

isQuo (Quo _ _) = True
isQuo _ = False

instance (Show a) => Show (Expr a) where
        show (Sum xs) = "Σ(" ++ (foldl' (++) "" (intersperse ", " $ map show xs)) ++ ")"
        show (Prod xs) = "Π(" ++ (foldl' (++) "" (intersperse ", " $ map show xs)) ++ ")"
        show (Sym s) = s
        show (Const c) = show c
        show (Quo g h) = show g ++ " / " ++ show h
        show (Exp e) = "exp(" ++ show e ++ ")"
        show (Ln e) = "ln(" ++ show e ++ ")"

instance (Num a) => Num (Expr a) where
        x + y = Sum [x, y]
        x * y = Prod [x, y]
        abs = undefined
        signum = undefined
        fromInteger = Const . fromInteger
        negate expr = Prod [Const (-1), expr]

instance (Fractional a) => Fractional (Expr a) where
        (/) = Quo
        fromRational = Const . fromRational

instance (Floating a) => Floating (Expr a) where
        pi = Const pi
        exp = Exp
        log = Ln
        -- transcendentals could be defined in terms of exp
        sin = undefined
        cos = undefined
        asin = undefined
        acos = undefined
        atan = undefined
        sinh = undefined
        cosh = undefined
        asinh = undefined
        acosh = undefined
        atanh = undefined

diff :: (Num a, Ord a, Fractional a) => Expr a -> Expr a -> Expr a
diff (Const _) (Sym _) = 0
diff (Sym y) (Sym x) = if y == x then 1 else 0
diff (Prod fs) x@(Sym _) = sum . map (product . diffFirst) $ tuples where
        tuples = take len . grab len . cycle $ fs
        grab n xs = take n xs : grab n (tail xs)
        len = length fs
        diffFirst (y:ys) = diff y x : ys
diff (Quo g h) x@(Sym _) = (h * diff g x - g * diff h x) / (h ^ 2)
diff (Sum fs) x@(Sym _) = sum $ map (\f -> diff f x) fs
diff f@(Exp exponent) x@(Sym _) = diff exponent x * f
diff (Ln f) x@(Sym _) = diff f x / f
diff _ _ = undefined -- Don't know how to take the derivative wrt a non-symbol

