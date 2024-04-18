> {-# LANGUAGE InstanceSigs #-}
> module Polynomial where
> import Data.List
> import Data.Ratio
> import Field
> import UltrametricCalculator
>
> data Polynomial a = Polynomial [a] deriving (Eq)
>
> display_polynomial :: (Show a, Field a) => Polynomial a -> String
> display_polynomial (Polynomial xs)
>  | length with_powers == 0 = "0"
>  | otherwise = cleaned
>   where powers = [ 0 .. ]
>         with_powers = [with_power_string (coefficient, power) | (coefficient, power) <- zip (reverse xs) powers,  not (isZero coefficient)]
>         --with_power_string :: (a, Integer) -> String
>         with_power_string (coefficient, 0) = show (coefficient)
>         with_power_string (coefficient, 1) = show (coefficient) ++ "x"
>         with_power_string (coefficient, power) = (show coefficient) ++ "x^" ++ (show power)
>         joined = concat (intersperse " + " with_powers)
>         plus_minus_cleanup :: String -> String
>         plus_minus_cleanup "" = ""
>         plus_minus_cleanup (xs)
>            | isPrefixOf "+ -" xs = "- " ++ (plus_minus_cleanup (drop 3 xs))
>            | otherwise = (head xs) : (plus_minus_cleanup (tail xs))
>         cleaned = plus_minus_cleanup joined

>
> 
> instance (Show a,Field a) => Show (Polynomial a) where
>   show x = display_polynomial x

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

