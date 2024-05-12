> module Padic where
> import UltrametricCalculator
> import Field
> import BlockDisplay
> import Data.Ratio
> import Data.List
>
> data Padic = Padic { prime :: Integer, number :: Rational }  deriving (Eq)
> data PadicExpansionTerm = PadicExpansionTerm Integer Integer Int deriving (Eq)
> data PadicExpansion = PadicExpansion [PadicExpansionTerm] deriving (Eq)
> padic_expansion :: Integer -> Integer -> PadicExpansion
> padic_expansion prime n = PadicExpansion (padic_expansion' n 0)
>  where padic_expansion' n k
>          | n < prime = [PadicExpansionTerm prime n k]
>          | n `mod` prime == 0 = padic_expansion' (n `div` prime) (k+1)
>          | otherwise = (PadicExpansionTerm prime (n `mod` prime) k) : padic_expansion' (n `div` prime) (k+1)
> expansion_term_to_string :: PadicExpansionTerm -> Maybe String
> expansion_term_to_string (PadicExpansionTerm prime coefficient power_raised)
>  | coefficient == 0 = Nothing -- should be impossible tohappen
>  | power_raised == 0 = Just (show coefficient)
>  | (coefficient == 1) && (power_raised == 1) = Just (show prime)
>  | power_raised == 1 = Just ((show coefficient) ++ " * " ++ (show prime))
>  | (coefficient == 1) = Just ((show prime) ++ "^" ++ (show power_raised))
>  | otherwise = Just ((show coefficient) ++ " * " ++ (show prime) ++ "^" ++ (show power_raised))
> justs_to_list :: [Maybe a] -> [a]
> justs_to_list [] = []
> justs_to_list (Nothing:xs) = justs_to_list xs
> justs_to_list ((Just x):xs) = x : justs_to_list xs
> expansion_term_to_blockdisplay :: PadicExpansionTerm -> Maybe BlockElement
> expansion_term_to_blockdisplay (PadicExpansionTerm prime coefficient power_raised)
>  | coefficient == 0 = Nothing -- should be impossible tohappen
>  | power_raised == 0 = Just (OrdinaryText (show coefficient))
>  | (coefficient == 1) && (power_raised == 1) = Just (OrdinaryText (show prime))
>  | power_raised == 1 = Just (OrdinaryText ((show coefficient) ++ " * " ++ (show prime)))
>  | (coefficient == 1) = Just (HBlock (HorizontalBlock (NoSeparator) [OrdinaryText (show prime), Superscript (show power_raised)]))
>  | otherwise = Just (
>      HBlock (HorizontalBlock (NoSeparator) [OrdinaryText ((show coefficient) ++ " * " ++ (show prime)), Superscript (show power_raised)]))
> 
> instance Show PadicExpansion where
>   show (PadicExpansion powers) =
>      intercalate " + " (justs_to_list [expansion_term_to_string pwr | pwr <- powers])
> 
> instance (Show Padic) where
>   show (Padic { prime=p, number=n })
>     | denominator n == 1 = show (expansion (numerator n))
>     | denominator n < p && numerator n < p = (show (numerator n)) ++ " / " ++ (show (denominator n))
>     | denominator n < p = "(" ++ (show (expansion (numerator n))) ++ ") / " ++ (show (denominator n))
>     | numerator n < p = (show (numerator n)) ++ " / (" ++ (show (expansion (denominator n))) ++ ")"
>     | otherwise = "(" ++ (show (expansion (numerator n))) ++ ") / (" ++ (show (expansion (denominator n))) ++ ")"
>    where expansion = padic_expansion p
>
>
> instance BlockDisplay PadicExpansion where
>   blockDisplay (PadicExpansion powers) =
>      HBlock (HorizontalBlock (NoSeparator) (intersperse (OrdinaryText "  +  ") (justs_to_list [expansion_term_to_blockdisplay pwr | pwr <- powers])))
>
> fake_fraction_block :: BlockElement -> BlockElement -> BlockElement
> fake_fraction_block top bottom = VBlock (VerticalBlock {
>    internal_blocks_alignment=CentredHorizontally,
>    blocks=[top,middle,bottom],
>    block_alignment=CentredVertically
>    })
>   where middle = OrdinaryText line
>         line = take line_length (repeat '-')
>         line_length = maximum [3, blockWidth top, blockWidth bottom]
>
> instance BlockDisplay Padic where
>   blockDisplay (Padic {prime=p, number=n})
>     | denominator n == 1 = blockDisplay (expansion (numerator n))
>     | denominator n < p && numerator n < p = Fraction (show (numerator n)) (show (denominator n))
>     | denominator n < p = fake_fraction_block (blockDisplay (expansion (numerator n))) (blockDisplay (denominator n))
>     | numerator n < p = fake_fraction_block (blockDisplay (numerator n)) (blockDisplay (expansion (denominator n)))
>     | otherwise = fake_fraction_block (blockDisplay (expansion (numerator n))) (blockDisplay (expansion (denominator n)))
>    where expansion = padic_expansion p
> 
> 
> padic_int :: Integer -> Integer -> Padic
> padic_int p n = Padic {prime=p, number=n%1}

How many times can I divide the second integer using the first integer?

> data PDivisibility = InfinitelyDivisible | FinitelyDivisible Integer
> pdivisibility :: Integer -> Integer -> PDivisibility
> pdivisibility _ 0 = InfinitelyDivisible
> pdivisibility p n
>   | n % p == 0 = case (pdivisibility p (n `div` p)) of
>                      InfinitelyDivisible -> InfinitelyDivisible -- should be impossible to happen
>                      FinitelyDivisible k -> FinitelyDivisible (1 + k)
>   | otherwise = FinitelyDivisible 0
>
> padic_measure :: Padic -> Double
> padic_measure (Padic {prime=p, number=n})
>  | denominator n == 0 = error "Division by zero"
>  | otherwise = case (pdivisibility p (numerator n), pdivisibility p (denominator n)) of
>                   (InfinitelyDivisible,FinitelyDivisible _) -> 0
>                   (FinitelyDivisible _,InfinitelyDivisible) -> error "Division by zero error"
>                   (InfinitelyDivisible,InfinitelyDivisible) -> error "0 divided by 0 error"
>                   (FinitelyDivisible m1, FinitelyDivisible m2) ->
>                                (fromInteger p) ** (fromInteger (m2 - m1))
> 
> padic_distance :: Padic -> Padic -> Double
> padic_distance p1 p2
>   | prime p1 /= prime p2 = error "Asked to do padic measure of two different p-adic measures"
>   | otherwise = padic_measure (Padic { prime = prime p1, number = (number p1) - (number p2) })
>
> padic_distance_calculator = UltrametricCalculator padic_distance
