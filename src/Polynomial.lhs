> {-# LANGUAGE InstanceSigs #-}
> module Polynomial where
> import Data.List
> import Data.Ratio
> import Field
> import UltrametricCalculator
> import BlockDisplay
>
> data Polynomial a = Polynomial [a] deriving (Eq)
>
> instance (BlockDisplay a, Field a) => BlockDisplay (Polynomial a) where
>  blockDisplay (Polynomial xs)
>   | length with_powers == 0 = OrdinaryText "0"
>   | otherwise = HBlock (HorizontalBlock NoSeparator joined)
>    where powers = [ 0 .. ]
>          with_powers = [ with_power_string (coefficient, power, rendersAsNegative coefficient) | (coefficient, power) <- zip (reverse xs) powers,
>                            not (isZero coefficient)]
>          --with_power_string :: (a, Integer, Bool) -> [BlockElement]
>          with_power_string (coefficient, 0, p) = ([blockDisplay (coefficient)], p)
>          with_power_string (coefficient, 1, p) = ([blockDisplay (coefficient)] ++ [OrdinaryText " x"], p)
>          with_power_string (coefficient, power, p) = (([blockDisplay coefficient]) ++ [OrdinaryText " x", Superscript (show power)],p)
>          joiner [] = []
>          joiner [(x,_)] = x
>          joiner ((x1,p1):((x2,p2):xs))
>           | p2 = x1 ++ [OrdinaryText " "] ++ (joiner ((x2,p2):xs))
>           | otherwise = x1 ++ [OrdinaryText " + "] ++  (joiner ((x2,p2):xs))
>          joined = joiner (reverse with_powers)
>
> 
> instance (BlockDisplay a,Field a) => Show (Polynomial a) where
>   show x = render (blockDisplay x)

> polyDegreeInt :: Field a => Polynomial a -> Int
> polyDegreeInt (Polynomial xs) = length (dropWhile isZero xs)
>
> zeroPadToLength :: Field a => Int -> Polynomial a -> Polynomial a
> zeroPadToLength n (Polynomial xs)
>  | length xs > n = Polynomial xs
>  | otherwise = Polynomial ((take (n - (length xs)) (repeat zeroElement)) ++ xs)
> trimLeadingZeros :: Field a => Polynomial a -> Polynomial a
> trimLeadingZeros (Polynomial xs) = Polynomial (dropWhile isZero xs) 
> 
> addPolynomials :: Field a => Polynomial a -> Polynomial a -> Polynomial a
> addPolynomials p1 p2 = trimLeadingZeros (add' p1' p2')
>   where maxdegree = max (polyDegreeInt p1) (polyDegreeInt p2)
>         p1' = zeroPadToLength maxdegree p1
>         p2' = zeroPadToLength maxdegree p2
>         add' (Polynomial cd) (Polynomial ds) =
>            Polynomial ([c `plus` d | (c,d) <- zip cd ds])
>
> subtractPolynomial :: Field a => Polynomial a -> Polynomial a -> Polynomial a
> subtractPolynomial p1 p2 = trimLeadingZeros (add' p1' p2')
>   where maxdegree = max (polyDegreeInt p1) (polyDegreeInt p2)
>         p1' = zeroPadToLength maxdegree p1
>         p2' = zeroPadToLength maxdegree p2
>         add' (Polynomial cd) (Polynomial ds) =
>            Polynomial ([c `minus` d | (c,d) <- zip cd ds])
>
> polyDifferenceDegreeFunc :: (Field a) => Polynomial a -> Polynomial a -> Double
> polyDifferenceDegreeFunc p1 p2 = fromIntegral (polyDegreeInt (p1 `subtractPolynomial` p2))
>
> polyDifferenceDegreeFuncOverRationals :: Polynomial (Ratio Integer) -> Polynomial (Ratio Integer) -> Double
> polyDifferenceDegreeFuncOverRationals = polyDifferenceDegreeFunc
> polyOverRationalsDegree = UltrametricCalculator polyDifferenceDegreeFuncOverRationals

